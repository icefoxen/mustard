% Okay, here's how it's going to go...
% The thing is, each TCP socket needs to be able to talk to some
% other TCP sockets, essentially at random.

% tcp socket <-> listener<-\
%                  ...   <-|
%                  ...   <-+-> router <--> shell <--> database
%
% Upon contemplation, I need to work on the shell first.


% Echo server, to start with.
% I think that eventually we should spawn off worker threads to handle
% input...
% And they should each be able to recieve messages to produce output as well.
% Let's try that.


% XXX: Have to come  up with a way to make it shut down in a reasonable
% fashion.
-module( muserver ).
-export( [start/0] ).
-export( [messenger/1, listen_loop/1, verifier/1] ).

-define( listenport, 3001 ).

%REGISTERS: mumessenger

% Shortcuts
send( Sock, Msg ) ->
    gen_tcp:send( Sock, Msg ++ "\r\n" ).

% XXX: Timeout?
% We have to trim the \r\n off the end of the string, and turn it from
% a binary into a liststring
recv( Socket ) ->
    case gen_tcp:recv( Socket, 0 ) of
	{ok, Data} ->
	    S = bitstring_to_list( Data ),
	    L = string:len( S ),
	    string:left( S, L - 2 );

	{error, Reason} ->
	    gen_tcp:close( Socket ),
	    throw( {muserver_error, Reason} )
    end.


% Listens on a socket and executes the command it gets.
listener( Socket, PlayerIdx ) ->
    try
	Command = recv( Socket ),
	io:format( "Got ~p from ~p~n", [Command, PlayerIdx] ),
	mushell:doCommand( PlayerIdx, Command ),
	listener( Socket, PlayerIdx )
    catch
	A -> 
	    mumessenger ! {remove, PlayerIdx},
	    throw( A )
    end.
	     

sendToList( Message, [Player|Rest], SockList ) ->
    case lists:keyfind( Player, 1, SockList ) of
	{_, Sock} ->
	    send( Sock, Message ),
	    sendToList( Message, Rest, SockList );
	false ->
	    % Player is disconnected, don't worry about it.
	    sendToList( Message, Rest, SockList )
    end;
sendToList( _Message, [], _SockList ) ->
    ok.

% Keeps track of active sockets, and listens for messages to send out
% to them.  Identified by player index number.
% XXX: TODO: Make sure instances are unique.
messenger( SockList ) ->
    receive
	{new, Idx, Socket} ->
	    {ok, {Ip, Port}} = inet:peername( Socket ),
	    io:format( "New socket: ~p:~p~n", [Ip, Port] ),
	    messenger( [{Idx, Socket}|SockList] );
	% Warning: Sliiiiight O(N^2) here.
	% To fix, use something better than an assoc list,
	% or write a smarter algorithm.
	{send, Players, Message} ->
	    io:format( "Sending ~p to ~p~n", [Message, Players] ),
%	    Socks = lists:map( fun(X) -> lists:keyfind( X, 1, SockList ) end,
%			       Players ),
	    sendToList( Message, Players, SockList ),
	    messenger( SockList );
	{remove, Player} ->
	    {_, Sock} = lists:keyfind( Player, 1, SockList ),
	    gen_tcp:close( Sock ), 
	    S = lists:keydelete( Player, 1, SockList ),
	    messenger( S )
    end.


%    receive
%	{new_socket, Sock} ->
%
%	    multiplexer( [Sock|SockList] );
%	{closed_socket, Sock} ->
%	    io:format( "Socket closed~n" ),
%	    multiplexer( SockList -- [Sock] );
%	{data, Data} ->
%	    lists:foreach(fun(Sock) -> gen_tcp:send( Sock, Data ) end, SockList ),
%	    multiplexer( SockList );
%	X ->
%	    io:format( "Got stuff: ~p~n", [X] ),
%	    multiplexer( SockList ) 
%    end.


% Checks character name and password and such.
verifier( Socket ) ->
    send( Socket, "What is your name?" ),
    Name = recv( Socket ),
    io:format( "Got name ~p~n", [Name] ),
    case mushell:verifyPlayer( Name ) of
	{ok, Idx} ->
	    send( Socket, "What is your quest?" ),
	    recv( Socket ),
	    send( Socket, "What is your favorite color?" ),
	    recv( Socket ),
	    send( Socket, "You may pass, " ++ Name ),
	    mumessenger ! {new, Idx, Socket},
	    io:format( "Verified player: ~p, ~p~n", [Name, Idx] ),
	    listener( Socket, Idx );
	{mushell_error, Message} ->
	    send( Socket, "Error: " ++ Message ),
	    gen_tcp:close( Socket )
    end.

listen_loop( ListenSocket ) ->
    case gen_tcp:accept( ListenSocket ) of
	{ok, Socket} ->
	    spawn( ?MODULE, verifier, [Socket] ),
	    listen_loop( ListenSocket );
	{error, Reason} ->
	    throw( {muserver_error, Reason} )
    end.


start() ->
    Messenger = spawn( ?MODULE, messenger, [[]] ),
    register( mumessenger, Messenger ),

    io:format( "Server starting on port ~p~n", [?listenport] ),

    L = gen_tcp:listen( ?listenport,
			 [binary,{active, false}, {reuseaddr, true}] ),
    case L of
	{ok, ListenSocket} ->
	    spawn( ?MODULE, listen_loop, [ListenSocket] );
	{error, Reason} ->
	    throw( {muserver, Reason} )
    end.


%start() ->
%   Multiplexer = spawn( ?MODULE, multiplexer, [[]] ),
%   case gen_tcp:listen( ?listenport, [binary,{active, false}, {reuseaddr, true}] ) of
%      {ok, ListenSocket} -> listen_loop( ListenSocket, Multiplexer );
%      {error, Reason} -> erlang:error( {start, Reason} )
%   end.
