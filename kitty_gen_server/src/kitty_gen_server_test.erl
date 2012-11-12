-module(kitty_gen_server_test).
-compile(export_all).
-include("include/cat.hrl").

test() ->
    {ok, Pid} = kitty_gen_server:start_link(),
    Pid ! <<"Test handle_info">>,
    Cat = #cat{name="Cat Stevens"}
        = kitty_gen_server:order_cat(Pid, "Cat Stevens", white,
                                     "not actualy a cat"),
    kitty_gen_server:return_cat(Pid, Cat),
    #cat{name="Cat Stevens"}
        = kitty_gen_server:order_cat(Pid, "Kitten Mittens", black,
                                     "look at them little paws!"),
    #cat{name="Kitten Mittens"}
        = kitty_gen_server:order_cat(Pid, "Kitten Mittens", black,
                                     "look at them little paws!"),
    ok = kitty_gen_server:return_cat(Pid, Cat),
    ok = kitty_gen_server:close_shop(Pid),
    ok.

