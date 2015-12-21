-module(prop_{{name}}).
-include_lib("proper/include/proper.hrl").
-define(MODEL, model_{{name}}).

prop_test() ->
    ?FORALL(Cmds, commands(?MODEL),
            begin
                actual_system:start_link(),
                {History, State, Result} = run_commands(?MODEL, Cmds),
                actual_system:stop(),
                ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
                                    [History,State,Result]),
                          aggregate(command_names(Cmds), Result =:= ok))
            end).
