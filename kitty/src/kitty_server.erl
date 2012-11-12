%%%%% Naive version
-module(kitty_server).

-export([start_link/0, order_cat/4, return_cat/2, close_shop/1,
         handle_cast/2, handle_call/4]).

-include("include/cat.hrl").

%%% Client API
start_link() -> spawn_link(fun init/0).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
    my_server:call(Pid, {order, Name, Color, Description}).

%% This call is asynchronous
return_cat(Pid, Cat = #cat{}) ->
    my_server:cast(Pid, {return, Cat}).

%% Synchronous call
close_shop(Pid) ->
    my_server:call(Pid, terminate).

%%% Server functions
init() -> my_server:loop(?MODULE, []).

handle_cast({return, Cat=#cat{}}, Cats) ->
    [Cat | Cats];

handle_cast(Unknown, Cats) ->
    %% do some logging here too
    io:format("Unknown st: ~p~n", [Unknown]),
    Cats.

handle_call({order, Name, Color, Description}, Pid, Ref, Cats) ->
    if Cats =:= [] ->
            Pid ! {Ref, make_cat(Name, Color, Description)},
            Cats;
       Cats =/= [] -> % got to empty the stock
            Pid ! {Ref, hd(Cats)},
            tl(Cats)
    end;

handle_call(terminate, Pid, Ref, Cats) ->
    Pid ! {Ref, ok},
    terminate(Cats),
    terminated;

handle_call(Unknown, Pid, Ref, Cats) ->
    %% do some logging here too
    io:format("Unknown call: ~p~n", [{Unknown, Pid, Ref}]),
    Cats.

%%% Private functions
make_cat(Name, Col, Desc) ->
    #cat{name=Name, color=Col, description=Desc}.

terminate(Cats) ->
    [io:format("~p was set free.~n",[C#cat.name]) || C <- Cats],
    ok.
