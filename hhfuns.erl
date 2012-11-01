-module(hhfuns).
-compile(export_all).

one() ->
    1.
two() ->
    2.

add(X, Y) ->
    X() + Y().

increment([]) ->
    [];
increment([H|T]) -> 
    [H+1|increment(T)].

decrement([]) ->
    [];
decrement([H|T]) -> 
    [H-1 | decrement(T)].

map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].

incr(X) ->
    X+1.
decr(X) ->
    X-1.

base(A) ->
    B = A + 1,
    F = fun() -> A * B end,
    F().

a() ->
    Secret = "pony",
    fun() -> Secret end.
 
b(F) ->
    "a/0's password is "++F().

even(L) ->
    lists:reverse(even(L, [])).

%%only keep even numbers
even([], _) ->
    [];
even([H|T], Acc) when H rem 2 == 0 ->
    even([T], [H|Acc]);
even([_|T],Acc) ->
    even(T,Acc).

old_men(L) ->
    lists:reverse(old_men(L,[])).

old_men([], Acc) ->
    Acc;
old_men([Person={male, Age}|People], Acc) when Age > 60 ->
    old_men(People, [Person|Acc]);
old_men([_|T],Acc) ->
    old_men(T,Acc).

filter(Pred, L) -> lists:reverse(filter(Pred, L, [])).

filter(_, [], Acc) ->
    Acc;
filter(Pred, [H|T], Acc) ->
    case(Pred(H)) of
        true ->
            filter(Pred, T, [H|Acc]);
        false ->
            filter(Pred, T, Acc)
    end.

%People = [{male,45},{female,67},{male,66},{female,12},{unkown,174},{male,74}].

%%does the same as old_men(People)
%hhfuns:filter(fun({Gender,Age}) -> Gender == male andalso Age>60 end, People).

max([H|T]) -> max2(T, H).

%why is this called max and just max/2 because that's going to ?
max2([], Max) ->
    Max;
max2([H|T], Max) when H > Max ->
    max2(T,H);
max2([_|T], Max) ->
    max2(T,Max).

min([H|T]) -> min2(T, H).

min2([], Min) -> Min;
min2([H|T], Min) when H < Min ->
    min2(T, H);
min2([_|T], Min) -> min2(T, Min).

sum(L)->
    sum(L,0).
sum([],Sum) ->
    Sum;
sum([H|T], Sum) ->
    sum(T, H+Sum).

%but all this tail recursion can be encapsulated as

fold(_, Start, []) ->
    Start;
fold(F, Start, [H|T]) -> fold(F, F(H,Start), T).

%% for example, max.
% hhfuns:fold(fun (A,B) when A>B -> A; (_,B) -> B end, 0, List).
%writing max using fold:
max_f([]) -> [];
max_f(List = [H|T]) ->
    fold(fun (X,Y) when X > Y -> X; (_,Y) -> Y end, H, T).
