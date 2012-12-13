% XXX: Error handling, d00d!  All over the damn place!

-module( mushell ).
-export( [doSay/2, doGo/2, doPose/2, doLook/1, doLookat/2, doQuit/1,
	 doChangeMyName/2, doChangeMyDesc/2, doChangeLinkName/3,
	 doChangeLinkSource/3, doChangeLinkDest/3, doChangeAreaName/3,
	 doChangeAreaDesc/3, doChangeRefName/3,
	 doCreateArea/3, doCreateLink/5, doCreateRef/3,
	 doDeleteArea/2, doDeleteLink/2, doDeleteRef/2,
	 doListRefs/1, doCommand/2] ).

-export( [lookWrap/2, quitWrap/2, listWrap/2, 
	  changeLinkSourceWrap/2, changeLinkDestWrap/2, changeLinkNameWrap/2,
	  changeAreaNameWrap/2, changeAreaDescWrap/2,
	  changeRefNameWrap/2, createAreaWrap/2, createLinkWrap/2] ).

-export( [verifyPlayer/1] ).

-define( me, Me = mudb:getPlayer( Who ) ).
-define( here, Here = mudb:getArea( Me#player.location ) ).


% grumble
-include( "mudb.hrl" ).

% Auxillary functions
getAllPlayersAt( Where ) ->
    mudb:selectPlayer( #player{location=Where, _ = '_'} ).

% Checks username and password.
verifyPlayer( Name ) ->
    case mudb:selectPlayer( #player{name=Name, _='_'} ) of
	[] -> {mushell_error, "Player does not exist."};
	[A] -> {ok, A#player.idx}
    end.
	    
    

% Communication functions.
% mumessenger is registered in muserver.erl.
sendToPlayer( Who, Str ) ->
    io:format( "Sending to ~p: ~p~n", [Who, Str] ),
    mumessenger ! {send, [Who], Str}.

sendToArea( Where, Str ) ->    
    P = getAllPlayersAt( Where ),
    io:format( "Sending to ~p: ~p~n", [P, Str] ),
    PlayerList = lists:map( fun(X) -> X#player.idx end, P ),
    mumessenger ! {send, PlayerList, Str}.

%    lists:foreach( fun(X) -> sendToPlayer( X#player.idx, Str ) end, P ).
%    lists:foreach( fun(X) -> sendToPlayer( X, Str ) end, P ).


exitString( WhereStruct ) ->
    Links = mudb:selectLink( #link{from=WhereStruct#area.idx, _='_'} ),
    if Links == [] -> "Exits: None";
       true -> L = lists:map( fun(X) -> X#link.name end, Links ),
	       string:concat( "Exits: ", string:join( L, ", " ) )
    end.

playerString( WhereStruct ) ->			    
    Players = mudb:selectPlayer( 
		#player{location=WhereStruct#area.idx, _='_'} ),
    if Players == [] -> "Players: None";
       true -> P = lists:map( fun(X) -> X#player.name end, Players ),
	       string:concat( "Players: ", string:join( P, ", " ) )
    end.





% commands:
% delete {area|link}, create {area|link}, 
% change {area|link|my} {name|desc}|{name|source|dest}|{name|desc}, 
% say, pose, go, look, lookat, quit
% listrefs,
% All commands are asynchronous.

% Who is the player's db index
doChangeMyName( Who, To ) ->
    ?me,
    NewMe = Me#player{name=To},
    mudb:setPlayer( NewMe ).
doChangeMyDesc( Who, To ) ->
    ?me,
    NewMe = Me#player{desc=To},
    mudb:setPlayer( NewMe ).

doChangeLinkName( Who, LinkRef, ToString ) ->
    R = mudb:getRef( Who, LinkRef ),
    L = mudb:getLink( R#ref.value ),
    mudb:setLink( L#link{name=ToString} ).
doChangeLinkSource( Who, LinkRef, ToRef ) ->
    LRef = mudb:getRef( Who, LinkRef ),  % Ref of link
    TRef = mudb:getRef( Who, ToRef ),    % Ref of new source
    L = mudb:getLink( LRef#ref.value ),  % Link
    mudb:setLink( L#link{from=TRef#ref.value} ).
doChangeLinkDest( Who, LinkRef, ToRef ) ->
    LRef = mudb:getRef( Who, LinkRef ),  % Ref of link
    TRef = mudb:getRef( Who, ToRef ),    % Ref of new dest
    L = mudb:getLink( LRef#ref.value ),  % Link
    mudb:setLink( L#link{to=TRef#ref.value} ).


doChangeAreaName( Who, AreaRef, ToString ) ->
    ARef = mudb:getRef( Who, AreaRef ),
    Area = mudb:getArea( ARef#ref.value ),
    NewArea = Area#area{name=ToString},
    mudb:setArea( NewArea ).
doChangeAreaDesc( Who, AreaRef, ToString ) ->
    ARef = mudb:getRef( Who, AreaRef ),
    Area = mudb:getArea( ARef#ref.value ),
    NewArea = Area#area{desc=ToString},
    mudb:setArea( NewArea ).

% Changing the name essentially changes the index, so we have to delete
% the ref and create a new one.
doChangeRefName( Who, RefString, ToString ) ->
    ARef = mudb:getRef( Who, RefString ),
    mudb:deleteRef( Who, RefString ),
    mudb:createRef( Who, ToString, ARef#ref.value ).
    

doCreateArea( Who, Name, RefName ) ->
    Area = mudb:createArea( Name ),
    mudb:createRef( Who, RefName, Area#area.idx ).
doCreateLink( Who, Name, RefName, FromRef, ToRef ) ->
    FRef = mudb:getRef( Who, FromRef ),
    TRef = mudb:getRef( Who, ToRef ),
    NewLink = mudb:createLink( Name, FRef#ref.value, TRef#ref.value ),
    mudb:createRef( Who, RefName, NewLink#link.idx ).


% XXX: Yeah, don't want to bother with this yet.
doCreateRef( _Who, _RefName, _ObjectName ) ->   
    ok.

% XXX: Deleting areas and links should delete refs to them as well.
% XXX: Deleting an area also needs to do something about all the people
% inside it, so they don't get messed up!
doDeleteArea( Who, AreaRef ) ->
    ARef = mudb:getRef( Who, AreaRef ),
    mudb:deleteArea( ARef#ref.value ).
doDeleteLink( Who, LinkRef ) ->
    LRef = mudb:getRef( Who, LinkRef ),
    mudb:deleteLink( LRef#ref.value ).
doDeleteRef( Who, Name ) ->
    mudb:deleteRef( Who, Name ).


doSay( Who, What ) ->
    ?me,
    Message = lists:append( [Me#player.name, " says, \"", What, "\""] ),
    sendToArea( Me#player.location, Message ).

doPose( Who, What ) ->
    ?me,
    Message = lists:append( [Me#player.name, " ", What] ),
    sendToArea( Me#player.location, Message ).

doGo( Who, ToString ) ->
    ?me,
    ?here,
    L = mudb:selectLink( #link{name=ToString, from=Here#area.idx, _ = '_'} ),
    case L of
    	[A|[]] ->
	    LeaveMessage = lists:append( Me#player.name, " has left." ),
	    ArriveMessage = lists:append( Me#player.name, " has arrived." ),
	    Dest = A#link.to,
	    NewMe = Me#player{location=Dest},
	    sendToArea( Dest, ArriveMessage ),
	    mudb:setPlayer( NewMe ),
	    sendToArea( Here#area.idx, LeaveMessage ),
	    doLook( Who );
	[] -> sendToPlayer( Me, "I don't see that link here." );
	[_|_] -> sendToPlayer( Me, "Which exit do you mean?" )
    end.

doLook( Who ) ->
    ?me,
    ?here,
    sendToPlayer( Who, Here#area.name ),
    sendToPlayer( Who, Here#area.desc ),
    sendToPlayer( Who, exitString( Here ) ),
    sendToPlayer( Who, playerString( Here ) ).

doLookat( Who, AtString ) ->
    ?me,
    ?here,
    P = mudb:selectPlayer( #player{location=Here#area.idx, name=AtString, _ = '_'} ),
    lists:foreach( fun(X) -> sendToPlayer( Who, X#player.desc ) end, P ).


doListRefs( Who ) ->
    Refs = mudb:selectRef( #ref{pn={Who,'_'}, _ = '_'} ),
    sendToPlayer( Who, io_lib:format( "~p", [Refs] ) ).
	    

% XXX: TODO: Make this work.
doQuit( Who ) ->
    ?me,
    ?here,
    sendToArea( Here, Me#player.name ++ " has quit." ),
    ok.



% Mmmmmm, recursive-descent parsing.

nextWord( Str ) ->
    Word = lists:takewhile( fun(X) -> X /= $  end, Str ),
    Rest = string:strip( 
	     lists:dropwhile( fun(X) -> X /= $  end, Str ) ),
    {Word, Rest}.


parse( Who, Grammar, CommandStr ) ->
    {Cmd, Rest} = nextWord( CommandStr ),
    case lists:keyfind( Cmd, 1, Grammar ) of
	false ->
	    sendToPlayer( Who, "Invalid command: " ++ Cmd );
	{_, Operation} ->
	    if is_atom( Operation ) ->
		    apply( mushell, Operation, [Who, Rest] );
	       is_list( Operation ) ->
		    parse( Who, Operation, Rest );
	       true ->
		    sendToPlayer( Who, "Error: invalid operation: " ++ 
				  io_lib:format( "~p", Operation ) )
	    end;
	Other ->
	    sendToPlayer( Who, "This should be impossible " ++
			  io_lib:format( "~p", Other ) )
    end.

% Wrapper functions to do additional command-dependant parsing as required.
lookWrap( Who, _ ) ->
    doLook( Who ).

quitWrap( Who, _ ) ->
    doQuit( Who ).

listWrap( Who, _ ) ->
    doListRefs( Who ).

% I want more powerful function operations.
annoyingThing2( Func, Who, Args ) ->
    {Ref, Rest} = nextWord( Args ),
    apply( mushell, Func, [Who, Ref, Rest] ).

changeLinkSourceWrap( Who, Args ) ->
    annoyingThing2( doChangeLinkSource, Who, Args ).

changeLinkDestWrap( Who, Args ) ->
    annoyingThing2( doChangeLinkDest, Who, Args ).

changeLinkNameWrap( Who, Args ) ->
    annoyingThing2( doChangeLinkName, Who, Args ).

changeAreaNameWrap( Who, Args ) ->
    annoyingThing2( doChangeAreaName, Who, Args ).

changeAreaDescWrap( Who, Args ) ->
    annoyingThing2( doChangeAreaDesc, Who, Args ).

changeRefNameWrap( Who, Args ) ->
    annoyingThing2( doChangeRefName, Who, Args ).

createAreaWrap( Who, Args ) ->
    {Name, Rest} = nextWord( Args ),
    {RefName, _Rest2} = nextWord( Rest ),
    doCreateArea( Who, Name, RefName ).

createLinkWrap( Who, Args ) ->
    {Name, Rest} = nextWord( Args ),
    {RefName, Rest2} = nextWord( Rest ),
    {FromRef, Rest3} = nextWord( Rest2 ),
    {ToRef, _Rest4} = nextWord( Rest3 ),
    doCreateLink( Who, Name, RefName, FromRef, ToRef ).

theGrammar() ->
    [{"say", doSay},
     {"go", doGo},
     {"pose", doPose},
     {"look", lookWrap},
     {"lookat", doLookat},
     {"quit", quitWrap},
     {"list", listWrap},
     {"create",
      [{"area", createAreaWrap},
       {"link", createLinkWrap}]},
     {"change",
      [{"my",
	[{"name", doChangeMyName},
	 {"desc", doChangeMyDesc}]},
       {"link",
	[{"source", changeLinkSourceWrap},
	 {"dest",   changeLinkDestWrap},
	 {"name",   changeLinkNameWrap}]},
       {"area",
	[{"name", changeAreaNameWrap},
	 {"desc", changeAreaDescWrap}]},
       {"ref",
	[{"name", changeRefNameWrap}]}
      ]}
     ].
      
	 
	 
doCommand( Who, Command ) ->
    parse( Who, theGrammar(), Command ).
