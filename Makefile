REBAR=$(shell which rebar || echo ./rebar)

.PHONY: all compile clean eunit test eqc doc check dialyzer

DIRS=src

all: deps compile

check: compile dialyzer

compile:
	$(REBAR) compile

deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean

eunit:
	$(REBAR) eunit

test: eunit

doc:
	$(REBAR) get-deps doc

dialyzer:
	$(REBAR) skip_deps=true dialyze