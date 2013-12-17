## active

Active is an Erlang application that triggers rebuilds according to source changes.
Active is also a new [sync](https://github.com/rustyio/sync).

The key features are:

* `active` no longer hogs disk and cpu to check for changes.
Instead, it uses [erlfsmon](https://github.com/proger/erlfsmon) to observe filesystem events.
* `active` picks up new module files in your working directory

### Setting up

Just add a line to `rebar.config`:

```erlang
    {active, ".*", {git, "git://github.com/proger/active", "HEAD"}}
```

And make sure you start it along in your release boot scripts or application startup scripts:

```sh
ERL_LIBS=deps erl -pa ebin -config sys.config \
    -eval '[ok = application:ensure_started(A, permanent) || A <- [erlfsmon,active]]'
```

That's it!
