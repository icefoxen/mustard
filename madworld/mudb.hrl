% parent not yet necessary.
-record( area, {idx, 
		name="SomeArea", 
		desc=""} ).

-record( player, {idx, 
		  name="SomePlayer", 
		  location, 
		  desc=""} ).

-record( link, {idx, name, from, to} ).

% Funky uniqueness constraints here...
% Each player has a set of names and values.
% The tuple of {player, name} must be unique.
% Sadly, mnesia doesn't have any way to enforce this, so we kludge by making
% the key a {player, name} tuple and make selecting a bit more of a headache.
-record( ref, {pn, value} ).

-record( servInfo, {idx, maxidx} ).

