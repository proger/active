## active

Active is an Erlang application that triggers rebuilds according to source changes.
Active is also a new [sync](https://github.com/rustyio/sync).

The key features are:

* `active` no longer hogs disk and cpu to check for changes.
Instead, it uses [erlfsmon](https://github.com/proger/erlfsmon) to observe filesystem events.
* `active` uses [rebar](https://github.com/basho/rebar) directly to build sources.
This leverages `rebar` to build all kinds of source files that it can build including `erlydtl` templates.

An extremely simplified version of `active` can look like this (however it does not load any code):

```make
# cat Makefile
sync:
	fsevent_watch -F . \
		| tee /dev/stderr \
		| env PERLIO=:raw perl -ne 's#.*\t.*\t$$ENV{"PWD"}/(apps|deps)/(\w+)/(?!ebin)#\2# && print "$$1=$$2\n"' \
		| xargs -n1 rebar compile
```

### Setting up

Just add a line to `rebar.config`:

```erlang
    {active, ".*", {git, "git://github.com/proger/active", "HEAD"}}
```

And make sure you start it along in your release boot scripts or application startup scripts:

```sh
ERL_LIBS=deps erl -pa ebin -config sys.config \
    -eval '[ok = application:ensure_started(A, permanent) || A <- [sasl,lager,gproc,erlfsmon,compiler,crypto,syntax_tools,tools,rebar,active]]'
```

That's it!

### Caveats

* `rebar` depends on `sasl`. If you don't wish to see large SASL logs (e.g. you use `lager`),
turn them off in your config:

```erlang
    {sasl, [{sasl_error_logger, false}]}
```

* `rebar` writes to stdout by default. If you wish to change this behaviour,
you may redefine `console_log_function` and `log_function` variables.
(this is pending [merge to upstream](https://github.com/rebar/rebar/pull/131),
`active` currently uses [my fork of rebar](https://github.com/proger/rebar) anyway)

```erlang
    {rebar, [
            {log_function, {error_logger, format}},
            {console_log_function, {error_logger, format}}
    ]}
```

* due to the fact that `rebar` changes code paths while running, your regular rebar escript executable may fail.
Make sure your rebar escript is as fresh as possible if it fails (take one from [my fork](https://github.com/proger/rebar))
