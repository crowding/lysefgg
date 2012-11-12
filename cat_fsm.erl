-module(cat_fsm).
-export([start/0, event/2, test/0]).

start() ->
    spawn(fun() -> dont_give_crap() end).

event(Pid, Event) ->
    Ref = make_ref(),
    Pid ! {self(), Ref, Event},
    receive
        {Ref, Msg} -> {ok, Msg}
    after 5000 ->
            {error, timeout}
    end.

dont_give_crap() ->
    receive
        {Pid, Ref, _Msg} -> Pid ! {Ref, meh};
        _ -> ok
    end,
    io:format("switching to 'dont_give_crap' state~n"),
    dont_give_crap().


test() ->
    Cat = cat_fsm:start(),
    {ok, meh} = cat_fsm:event(Cat, pet),
    {ok, meh} = cat_fsm:event(Cat, love),
    {ok, meh} = cat_fsm:event(Cat, cherish),
    exit(Cat, stop).
