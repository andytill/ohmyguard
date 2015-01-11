-module(omg_simplest).

-compile({parse_transform, ohmyguard}).

-export([omg_1/1]).

omg_1(X/binary) -> 
	X.