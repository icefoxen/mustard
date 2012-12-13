-module( musuper ).
-behavior( supervisor ).

-export( [start_link/0, init/1] ).


init( _Args ) ->
    {ok, {{one_for_one, 2, 60},
	  [{db, {db, start_link, []}, permanent, brutal_kill, worker, [db]}
	   ]}}.
     
start_link() ->
    supervisor:start_link( super, [] ).

