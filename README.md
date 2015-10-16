Rebar3 Proper Plugin
=====

Run PropEr test suites.

By default, will look for all modules starting in `prop_` in the `test/`
directories of a rebar3 project, and running all properties (functions of arity
0 with a `prop_` prefix) in them.

Todo/Gotchas
----

- Code coverage is not supported yet.
- The plugin still relies on the `master` branch of proper.
- No tests yet

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rebar3_proper, ".*", {git, "https://github.com/ferd/rebar3_proper.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:

    Usage: rebar3 proper [-d <dir>] [-m <module>] [-p <properties>]
                         [-n <numtests>] [-v <verbose>]
                         [--long_result <long_result>]
                         [--start_size <start_size>] [--max_size <max_size>]
                         [--max_shrinks <max_shrinks>]
                         [--noshrink <noshrink>]
                         [--constraint_tries <constraint_tries>]
                         [--spec_timeout <spec_timeout>]
                         [--any_to_integer <any_to_integer>]
    
      -d, --dir           directory where the property tests are located
                          (defaults to "test")
      -m, --module        name of one or more modules to test (comma-separated)
      -p, --prop          name of properties to test within a specified module
                          (comma-separated)
      -n, --numtests      number of tests to run when testing a given property
      -v, --verbose       each propertie tested shows its output or not
                          (defaults to true)
      --long_result       enables long-result mode, displaying
                          counter-examples on failure rather than just false
      --start_size        specifies the initial value of the size parameter
      --max_size          specifies the maximum value of the size parameter
      --max_shrinks       specifies the maximum number of times a failing test
                          case should be shrunk before returning
      --noshrink          instructs PropEr to not attempt to shrink any
                          failing test cases
      --constraint_tries  specifies the maximum number of tries before the
                          generator subsystem gives up on producing an
                          instance that satisfies a ?SUCHTHAT constraint
      --spec_timeout      duration, in milliseconds, after which PropEr
                          considers an input to be failing
      --any_to_integer    converts instances of the any() type to integers in
                          order to speed up execution

All of [PropEr's standard configurations](http://proper.softlab.ntua.gr/doc/proper.html#Options)
that can be put in a consult file can be put in `{proper_opts, [Options]}.` in your rebar.config file.
