-module(recursive).
-export([fac/1]).

%why do they always with such silly factorials.
%at least it seems to memoize?
fac(N) when N == 0 -> 1;
fac(N) when N > 0 -> N*fac(N-1).

