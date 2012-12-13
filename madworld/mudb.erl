-module( mudb ).

-export([makeNewDatabase/0, start/0]).
-export([createPlayer/1, createArea/1, createLink/3, createRef/3,
	 getPlayer/1, getArea/1, getLink/1, getRef/2,
	 setPlayer/1, setArea/1, setLink/1, setRef/1,
	 deletePlayer/1, deleteArea/1, deleteLink/1, deleteRef/2,
	 selectPlayer/1, selectArea/1, selectLink/1, selectRef/1]).

-include( "mudb.hrl" ).


%%%% Helper funcs %%%%

error( Why ) ->
    throw( {mudb_error, Why} ).

% T is the name of the transaction that goes in.
% Retval is the variable that the successful result gets bound to.
% Okval is the result that gets returned upon success.  This is usually
% the same as Retval, but not always; see newIndex, for instance.
-define( verifyTransaction( T, Retval, Okval ),
    case mnesia:transaction(T) of
	{atomic, Retval} ->
	    Okval;
	{aborted, Reason} ->
	    error( Reason )
    end ).

% CRUD!
createHandler( Type, Struct ) ->
    F = fun() ->
		mnesia:write( Type, Struct, write ),
		Struct
	end,
    ?verifyTransaction( F, P, P ).

readHandler( Type, Idx ) ->
    F = fun() ->
		[P] = mnesia:read( Type, Idx ),
		P
	end,
    case mnesia:transaction( F ) of
	{atomic, []} ->
	    error( [does_not_exist, Type, Idx] );
	{aborted, Reason} ->
	    error( Reason );
	{atomic, P} ->
	    P
    end.

% XXX: To consider: It should be impossible for the user to construct a
% record for an object that doesn't exist, so the check should be unnecessary.
updateHandler( Type, Struct ) ->
    Idx = element( 2, Struct ),
    F = fun() ->
		case mnesia:read( Type, Idx ) of
		    [] -> {error, update_does_not_exist};
		    [_] -> J = mnesia:write( Type, Struct, write ),
			   J
		end
	end,
    ?verifyTransaction( F, P, P ).

deleteHandler( Type, Idx ) ->
    F = fun() ->
		mnesia:delete( {Type, Idx} )
	end,
    ?verifyTransaction( F, _, ok ).
	
% Yeah, it's not a full select, but I think it's probably what we mostly 
% want.
selectHandler( Type, Matchspec ) ->
    F = fun() ->
		mnesia:match_object( Type, Matchspec, read )
	end,
    ?verifyTransaction( F, I, I ).

newIndex() ->
    F = fun() ->
		[I] = mnesia:read( servInfo, 1 ),
		mnesia:write( servInfo, 
			      #servInfo{ 
				idx=1, maxidx = I#servInfo.maxidx+1 }, 
			      write ),
		I
	end,
    ?verifyTransaction( F, I, I#servInfo.maxidx ).
    

   

%%%%  Actual API here %%%%
start() ->
    mnesia:start(),
    io:format( "DB started~n" ).

createPlayer( Name ) ->
    Idx = newIndex(),
    createHandler( player, #player{idx=Idx, name=Name} ).
createArea( Name ) ->
    Idx = newIndex(),
    createHandler( area, #area{idx=Idx, name=Name} ).
createLink( Name, From, To ) ->
    Idx = newIndex(),
    createHandler( link, #link{idx=Idx, name=Name, from=From, to=To} ).
createRef( Player, Name, Val ) ->
    createHandler( ref, #ref{pn={Player,Name}, value=Val} ).

getPlayer( Idx ) ->
    readHandler( player, Idx ).
getArea( Idx ) ->
    readHandler( area, Idx ).
getLink( Idx ) ->
    readHandler( link, Idx ).
getRef( Player, Name ) ->
    readHandler( ref, {Player,  Name} ).

setPlayer( New ) ->
    updateHandler( player, New ).
setArea( New ) ->
    updateHandler( area, New ).
setLink( New ) ->
    updateHandler( link, New ).
setRef( New ) ->
    updateHandler( ref, New ).

deletePlayer( Idx ) ->
    deleteHandler( player, Idx ).
deleteArea( Idx ) ->
    deleteHandler( area, Idx ).
deleteLink( Idx ) ->
    deleteHandler( link, Idx ).
deleteRef( Player, Name ) ->
    deleteHandler( ref, {Player, Name} ).

selectPlayer( Matchspec ) ->
    selectHandler( player, Matchspec ).
selectArea( Matchspec ) ->
    selectHandler( area, Matchspec ).
selectLink( Matchspec ) ->
    selectHandler( link, Matchspec ).
selectRef( Matchspec ) ->
    selectHandler( ref, Matchspec ).




% Run this in a fresh shell without doing anything else, then exit,
% and it will initialize a new database.
% Note that this doesn't nuke any existing tables, it seems.
makeNewDatabase() ->
    mnesia:start(),
    mnesia:create_schema( [node()] ),
    mnesia:create_table( area, [{attributes, record_info( fields, area )},
				 {disc_copies, [node()]}
				] ),
    mnesia:create_table( player, 
			 [{attributes, record_info( fields, player )},
			  {disc_copies, [node()]}
			 ] ),
    mnesia:create_table( link, [{attributes, record_info( fields, link )},
				 {disc_copies, [node()]}
				] ),

    mnesia:create_table( ref, [{attributes, record_info( fields, ref )},
				 {disc_copies, [node()]}
				] ),


% XXX: For some reason the below doesn't work quite right, but does okay when
% I create the table and initial value by hand.
% XXX: It seems to work now.  Double-check and test!
    mnesia:create_table( servInfo, 
			 [{attributes, record_info( fields, servInfo )},
				 {disc_copies, [node()]}
			 ] ),
    F = fun() ->
		mnesia:write( servInfo, #servInfo{ idx=1, maxidx=0 }, write )
	end,
    mnesia:transaction( F ).
