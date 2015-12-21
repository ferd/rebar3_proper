-module(model_{{name}}).
-include_lib("proper/include/proper.hrl").
-export([command/1, initial_state/0, next_state/3,
         precondition/2, postcondition/3]).

-record(state, {}).

command(_State) ->
    oneof([
        {call, actual_system, some_call, [term(), term()]}
    ]).

%% Initial model value at system start. Should be deterministic.
initial_state() ->
    #state{}.

%% Picks whether a command should be valid under the current state.
precondition(#state{}, {call, _Mod, _Fun, _Args}) ->
    true.

%% Given the state `State' *prior* to the call `{call, Mod, Fun, Args}',
%% determine whether the result `Res' (coming from the actual system)
%% makes sense.
postcondition(_State, {call, _Mod, _Fun, _Args}, _Res) ->
    true.

%% Assuming the postcondition for a call was true, update the model
%% accordingly for the test to proceed.
next_state(State, _Res, {call, _Mod, _Fun, _Args}) ->
    NewState = State,
    NewState.
