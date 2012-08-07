REBAR = rebar
ERL ?= erl
APP := gen_heartbeat

.PHONY: deps

ft: 
	@$(REBAR) skip_deps=true compile
	@$(REBAR) skip_deps=true eunit

all: deps
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean

distclean: clean
	@$(REBAR) delete-deps

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[{todo, true}]'

test: all
	@$(REBAR) skip_deps=true eunit

fast:
	@$(REBAR) skip_deps=true compile

run:
	@$(ERL) -pa ebin $(PWD) deps/*/ebin

dialyzer: all
	dialyzer \
	ebin --no_check_plt \
	-pa ../ \
	-Wunmatched_returns -Werror_handling -Wrace_conditions -Wunderspecs

typer: 
	typer src -I ../

full: all test dialyzer typer

