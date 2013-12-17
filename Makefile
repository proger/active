ERL_FLAGS= +sbwt none +swct lazy +swt high

run:
	ERL_LIBS=deps erl -pa ebin \
		 $(ERL_FLAGS) \
		 -eval '[ok = application:ensure_started(A, permanent) || A <- [erlfsmon,active]]'

ifeq ($(shell uname), Darwin)
WATCHER = fsevent_watch -F .
else
WATCHER = fanotify_watch -c
endif

sync:
	$(WATCHER) | env PERLIO=:raw perl -ne '/.*\t.*\t$$ENV{"PWD"}.*erl$$/ && print $$_."\0"' | tee /dev/stderr | xargs -0tn1 -I % rebar compile

.PHONY: run
