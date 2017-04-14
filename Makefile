deps:
	rebar3 get-deps

compile:
	rebar3 compile
	cd _build/default/lib/jsonx/ && $(MAKE)

start:
	rebar3 shell