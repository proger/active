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
