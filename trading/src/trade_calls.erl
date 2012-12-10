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

%% force a race condition on cd trade negotiation
main_cd() ->
    S = self(),
    PidCliC = spawn(fun() -> marc(S) end),
    receive PidC -> PidC end,
    spawn(fun() -> pete(S, PidC, PidCliC) end),
    receive PidD -> PidD end,
    PidCliC ! PidD.

marc(Parent) ->
    {ok, Pid} = trade_fsm:start_link("Marc"),
    Parent ! Pid,
    receive PidD -> PidD end,
    io:format("Spawned Marc: ~p~n", [PidD]),
    sys:trace(Pid, true),
    sync2(),
    trade_fsm:trade(Pid, PidD),
    %%no need to accept_trade thanks to the race condition???
    timer:sleep(600),
    trade_fsm:retract_offer(Pid, "car"),
    trade_fsm:make_offer(Pid, "horse"),
    timer:sleep(600),
    trade_fsm:cancel(Pid),
    timer:sleep(1000),
    ok.

pete(Parent, PidC, PidCliC) ->
    {ok, Pid} = trade_fsm:start_link("Pete"),
    Parent ! Pid,
    io:format("Spawned Pete: ~p~n", [Pid]),
    sys:trace(Pid, true),
    sync1(PidCliC),
    trade_fsm:trade(Pid, PidC),
    trade_fsm:retract_offer(Pid, "car"),
    trade_fsm:make_offer(Pid, "manatee"),
    timer:sleep(100),
    trade_fsm:ready(Pid),
    timer:sleep(1000).

main_ef() ->
    S = self(),
    PidCliE = spawn(fun() -> carlClient(S) end),
    receive PidE -> PidE end,
    spawn(fun() -> jimClient(PidE, PidCliE) end).

carlClient(Parent) ->
    {ok, Carl} = trade_fsm:start_link("Carl"),
    Parent ! Carl,
    io:format("Spawned Carl: ~p~n", [Carl]),
    sys:trace(Carl, true),
    timer:sleep(800),
    trade_fsm:accept_trade(Carl),
    timer:sleep(400),
    io:format("~p~n", [trade_fsm:ready(Carl)]), %what? this is a synchronous call? never gonna sync.
    timer:sleep(1000),
    trade_fsm:make_offer(Carl, "horse"), %note that it's make offer to
                                         %your trader, not the other
                                         %guy's trader.
    trade_fsm:make_offer(Carl, "sword"),
    timer:sleep(1000),
    io:format("carl synchronizing~n"),
    sync2(),
    trade_fsm:ready(Carl),
    timer:sleep(200),
    trade_fsm:ready(Carl),
    timer:sleep(1000).

%was vary confused why this needed both the other trader pid and its
%cient pid. Turns out it's just for testing, so that we can use the
%sync() function defined below.
jimClient(Carl, CarlClient) ->
    {ok, Jim} = trade_fsm:start_link("Jim"),
    io:format("Spawned Jim: ~p~n", [Jim]),
    sys:trace(Jim, true),
    timer:sleep(500),
    trade_fsm:trade(Jim,Carl),
    trade_fsm:make_offer(Jim, "boots"),
    timer:sleep(200),
    trade_fsm:retract_offer(Jim, "boots"),
    timer:sleep(200),
    trade_fsm:make_offer(Jim, "shotgun"),
    timer:sleep(1000),
    io:format("jim synchronizing~n"),
    sync1(CarlClient),
    trade_fsm:make_offer(Jim, "horse"),
    timer:sleep(200),
    trade_fsm:ready(Jim),
    timer:sleep(1000).

sync1(Pid) ->
    Pid ! self(),
    receive ack -> ok end.

sync2() ->
    receive
        From -> From ! ack
    end.

