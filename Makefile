.PHONY: all compile xref eunit ct dialyzer shell clean

all: compile xref eunit ct dialyzer

compile:
	rebar3 compile

xref:
	rebar3 xref

eunit:
	rebar3 eunit

ct:
	rebar3 ct

dialyzer:
	rebar3 dialyzer

shell:
	rebar3 shell

clean:
	rebar3 clean --all
