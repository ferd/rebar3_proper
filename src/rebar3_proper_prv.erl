-module(rebar3_proper_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, proper).
-define(DEPS, [compile]).
-define(PRV_ERROR(Reason), {error, {?MODULE, Reason}}).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {profiles, [test]},
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 proper"},   % How to use the plugin
            {opts, proper_opts()},                   % list of options understood by the plugin
            {short_desc, "Run PropEr test suites"},
            {desc, "Run PropEr test suites"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Opts, ProperOpts} = handle_opts(State),
    rebar_api:debug("rebar3 proper options: ~p", [Opts]),
    rebar_api:debug("proper-specific options: ~p", [ProperOpts]),
    rebar_utils:update_code(rebar_state:code_paths(State, all_deps), [soft_purge]),
    maybe_cover_compile(State),
    Props = find_properties(State, Opts),
    Results = [{Mod, Fun, catch check(Mod, Fun, ProperOpts)} || {Mod, Fun} <- Props],
    rebar_api:debug("Results: ~p", [Results]),
    maybe_write_coverdata(State),
    rebar_utils:cleanup_code_path(rebar_state:code_paths(State, default)),
    Failed = [{M,F,Res} || {M,F,Res} <- Results, Res =/= true],
    case Failed of
        [] ->
            Tot = length(Results),
            rebar_api:info("~n~p/~p properties passed", [Tot, Tot]),
            {ok, State};
        [_|_] ->
            Tot = length(Results),
            FailedCount = length(Failed),
            Passed = Tot - FailedCount,
            rebar_api:error("~n~p/~p properties passed, ~p failed", [Passed, Tot, FailedCount]),
            ?PRV_ERROR({failed, Failed})
    end.


-spec format_error(any()) ->  iolist().
format_error({failed, Failed}) ->
    ["Failed test cases:",
     [io_lib:format("~n  ~p:~p() -> ~p", [M,F,Res]) || {M,F,Res} <- Failed]];
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private
%% ===================================================================
maybe_cover_compile(State) ->
    {RawOpts, _} = rebar_state:command_parsed_args(State),
    State1 = case proplists:get_value(cover, RawOpts, false) of
                 true  -> rebar_state:set(State, cover_enabled, true);
                 false -> State
             end,
    rebar_prv_cover:maybe_cover_compile(State1).

maybe_write_coverdata(State) ->
    {RawOpts, _} = rebar_state:command_parsed_args(State),
    State1 = case proplists:get_value(cover, RawOpts, false) of
                 true  -> rebar_state:set(State, cover_enabled, true);
                 false -> State
             end,
    rebar_prv_cover:maybe_write_coverdata(State1, ?PROVIDER).

check(Mod, Fun, Opts) ->
    rebar_api:info("Testing ~p:~p()", [Mod, Fun]),
    proper:quickcheck(Mod:Fun(), Opts).

find_properties(State, Opts) ->
    Dir = proplists:get_value(dir, Opts, "test"),
    case {proplists:get_value(module, Opts), proplists:get_value(properties, Opts)} of
        {undefined, undefined} ->
            find_properties(State, Dir, any, any);
        {Mods, undefined} ->
            find_properties(State, Dir, Mods, any);
        {undefined, Props} ->
            find_properties(State, Dir, any, Props);
        {Mods, Props} ->
            find_properties(State, Dir, Mods, Props)
    end.

find_properties(State, Dir, Mods, Props) ->
    %% Need to compile somewhere in there
    Dirs = [{App, TestDir}
            || App <- rebar_state:project_apps(State),
               not rebar_app_info:is_checkout(App),
               TestDir <- [filename:join(rebar_app_info:dir(App), Dir)],
               {ok, Files} <- [file:list_dir(TestDir)],
               lists:any(fun(File) -> prop_suite(Mods, File) end, Files)],
    compile_dirs(State, Dir, Dirs),
    [Prop || {_, TestDir} <- Dirs,
             {ok, Files} <- [file:list_dir(TestDir)],
             File <- Files,
             prop_suite(Mods, File),
             Prop <- properties(Props, module(File))].

prop_suite(Mods, File) ->
    Mod = filename:basename(File, ".erl"),
    filename:extension(File) =:= ".erl"
    andalso
    ((Mods =:= any andalso lists:prefix("prop_", Mod))
     orelse
     (Mods =/= any andalso lists:member(Mod, Mods))).

module(File) ->
    list_to_atom(filename:basename(File, ".erl")).

properties(any, Mod) ->
    [{Mod, Prop} || {Prop,0} <- Mod:module_info(exports), prop_prefix(Prop)];
properties(Props, Mod) ->
    [{Mod, Prop} || {Prop,0} <- Mod:module_info(exports),
                    lists:member(atom_to_list(Prop), Props)].

prop_prefix(Atom) ->
    lists:prefix("prop_", atom_to_list(Atom)).

compile_dirs(State, TestDir, Dirs) -> % [{App, Dir}]
    %% Set up directory -- may need to unlink then re-link
    %% copy contents into directory
    %% call the compiler
    [begin
       rebar_api:debug("Compiling ~s for PropEr", [rebar_app_info:name(App)]),
       OutDir = filename:join([rebar_app_info:out_dir(App), TestDir]),
       setup(State, OutDir),
       compile(State, Dir, OutDir)
     end || {App, Dir} <- Dirs],
    rebar_api:debug("App compiled", []).

setup(_State, OutDir) ->
    filelib:ensure_dir(filename:join([OutDir, "dummy.beam"])).

compile(State, Src, Out) ->
    rebar_api:debug("Compiling files in ~s to ~s", [Src, Out]),
    NewOpts = lists:foldl(fun({K, V}, Dict) -> rebar_opts:set(Dict, K, V) end,
                          rebar_state:opts(State),
                          [{src_dirs, ["."]}]),
    rebar_erlc_compiler:compile(NewOpts, Src, ec_cnv:to_list(Out)).

proper_opts() ->
    [{dir, $d, "dir", string,
      "directory where the property tests are located (defaults to \"test\")"},
     {module, $m, "module", string,
      "name of one or more modules to test (comma-separated)"},
     {properties, $p, "prop", string,
      "name of properties to test within a specified module (comma-separated)"},
     {numtests, $n, "numtests", integer,
      "number of tests to run when testing a given property"},
     {verbose, $v, "verbose", boolean,
      "each property tested shows its output or not (defaults to true)"},
     {cover, $c, "cover", {boolean, false},
      "generate cover data"},
     %% no short format for these buddies
     {long_result, undefined, "long_result", boolean,
      "enables long-result mode, displaying counter-examples on failure "
      "rather than just false"},
     {start_size, undefined, "start_size", integer,
      "specifies the initial value of the size parameter"},
     {max_size, undefined, "max_size", integer,
      "specifies the maximum value of the size parameter"},
     {max_shrinks, undefined, "max_shrinks", integer,
      "specifies the maximum number of times a failing test case should be "
      "shrunk before returning"},
     {noshrink, undefined, "noshrink", boolean,
      "instructs PropEr to not attempt to shrink any failing test cases"},
     {constraint_tries, undefined, "constraint_tries", integer,
      "specifies the maximum number of tries before the generator subsystem "
      "gives up on producing an instance that satisfies a ?SUCHTHAT "
      "constraint"},
     {spec_timeout, undefined, "spec_timeout", integer,
      "duration, in milliseconds, after which PropEr considers an input "
      "to be failing"},
     {any_to_integer, undefined, "any_to_integer", boolean,
      "converts instances of the any() type to integers in order to speed "
      "up execution"}
    ].

handle_opts(State) ->
    {CliOpts, _} = rebar_state:command_parsed_args(State),
    ConfigOpts = rebar_state:get(State, proper_opts, []),
    {rebar3_opts(merge_opts(ConfigOpts, CliOpts)),
     proper_opts(merge_opts(ConfigOpts, proper_opts(CliOpts)))}.

rebar3_opts([]) ->
    [];
rebar3_opts([{dir, Dir} | T]) ->
    [{dir, Dir} | rebar3_opts(T)];
rebar3_opts([{module, Mods} | T]) ->
    [{module, parse_csv(Mods)} | rebar3_opts(T)];
rebar3_opts([{properties, Props} | T]) ->
    [{properties, parse_csv(Props)} | rebar3_opts(T)];
rebar3_opts([_ | T]) ->
    rebar3_opts(T).

proper_opts([]) -> [];
proper_opts([{verbose, true} | T]) -> [verbose | proper_opts(T)];
proper_opts([{verbose, false} | T]) -> [quiet | proper_opts(T)];
proper_opts([{long_result, true} | T]) -> [long_result | proper_opts(T)];
proper_opts([{long_result, false} | T]) -> proper_opts(T);
proper_opts([{noshrink, true} | T]) -> [noshrink | proper_opts(T)];
proper_opts([{noshrink, false} | T]) -> proper_opts(T);
proper_opts([{any_to_integer, true} | T]) -> [any_to_integer | proper_opts(T)];
proper_opts([{any_to_integer, false} | T]) -> proper_opts(T);
%% those are rebar3-only options
proper_opts([{dir,_} | T]) -> proper_opts(T);
proper_opts([{module,_} | T]) -> proper_opts(T);
proper_opts([{properties,_} | T]) -> proper_opts(T);
proper_opts([{cover,_} | T]) -> proper_opts(T);
%% fall-through
proper_opts([H|T]) -> [H | proper_opts(T)].

merge_opts(Old, New) ->
    rebar_utils:tup_umerge(New, Old).

parse_csv(IoData) ->
    re:split(IoData, ", *", [{return, list}]).
