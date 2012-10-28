-module(functions).
-compile(export_all).

head([H|_]) ->
    H.

second([_,X|_]) ->
    X.

same(X,X)->
    true;
same(_,_) ->
    false.



