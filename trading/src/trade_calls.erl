-module(trade_calls).
-compile(export_all).

%% test a little bit of everything and also deadlocks on ready state
%% -- leftover messages possible on race conditions on ready state

%%what we are doing is spawning two processes "carl" and "jim" and letting
%%them talk to each other over a particular scenario.
main_ab() ->
    S = self(),
    PidCliA = spawn(fun() -> carl(S) end),
    receive PidA -> PidA end,
    spawn(fun() -> jim(PidA, PidCliA) end).

carl(Parent) ->
    {ok, Pid} = trade_fsm:start_link("Carl"),
    Parent ! Pid,
    io:format ("Spawned Carl:~p~n", [Pid]),
    sys:trace(Pid, true),
    timer:sleep(800),
    trade_fsm:accept_trade(Pid),                %800
    timer:sleep(400),
    io:format("~p~n",[trade_fsm:ready(Pid)]),   %1200
    timer:sleep(1000),
    trade_fsm:make_offer(Pid, "horse"),         %2200
    trade_fsm:make_offer(Pid, "sword"),
    timer:sleep(1000),
    io:format("a synchronizing~n"),             %3200
    sync2(),
    trade_fsm:ready(Pid),
    timer:sleep(200),
    trade_fsm:ready(Pid),                       %3400
    timer:sleep(1000).

jim(PidA, PidCliA) ->
    {ok, Pid} = trade_fsm:start_link("Jim"),
    io:format("Spawned Jim: ~p~n", [Pid]),
    sys:trace(Pid,true),
    timer:sleep(500),
    trade_fsm:trade(Pid, PidA),                 %500
    trade_fsm:make_offer(Pid, "boots"),
    timer:sleep(200),
    trade_fsm:retract_offer(Pid, "boots"),      %700
    timer:sleep(1000),
    trade_fsm:make_offer(Pid, "shotgun"),       %1700
    timer:sleep(1000),
    io:format("b synchronizing~n"),             %2700
    sync1(PidCliA),
    trade_fsm:make_offer(Pid, "horse"), %%race condition!
    trade_fsm:ready(Pid),
    timer:sleep(200),
    timer:sleep(1000).

sync1(Pid) ->
    Pid ! self(),
    receive ack -> ok end.

sync2() ->
    receive
        From -> From ! ack
    end.
