% Start with 'erl -mnesia'

-module( dbtest ).
-export( [init/0, test/0] ).


init() ->
    db:start().

%listen() ->
%    receive
%	A ->
%	    io:format( "Received: ~p~n", [A] ),
%	    listen()
%    after
%	10000 ->
%	    listen_timeout
%    end.

test() ->
    init(),
    io:format( " ~p,~n ~p,~n ~p.~n",
	       [gen_server:call( db, {create, player, "Icefox"} ),
		gen_server:call( db, {create, area, "Test Area 1", noparent} ),
		gen_server:call( db, {create, area, "Test Area 2", noparent} )
		] ).
