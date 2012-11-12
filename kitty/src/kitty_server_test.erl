-module(kitty_server_test).
-import(kitty_server, [start_link/0, order_cat/4, return_cat/2, close_shop/1]).
-include("include/cat.hrl").
-export([test/0]).

test() ->
    Pid = start_link(),
    Cat1=#cat{name=carl} =
        kitty_server:order_cat(Pid, carl, brown, "loves to burn bridges"),
    kitty_server:return_cat(Pid, Cat1),
    Cat2=#cat{name=carl} =
        kitty_server:order_cat(Pid, jimmy, orange, "cuddly"),
    #cat{name=jimmy} =
        kitty_server:order_cat(Pid, jimmy, orange, "cuddly"),
    kitty_server:return_cat(Pid, Cat2),
    %%this should print "carl was set free"; idk how to test that.
    close_shop(Pid),
    try
        close_shop(Pid),
        error(expected_error)
    catch
        error:normal -> ok
    end.
