.PHONY: rel

REBAR=./rebar

all:
	@$(REBAR) get-deps compile

clean:
	@$(REBAR) clean

clean_all:
	@$(REBAR) clean delete-deps

quick_compile:
	@$(REBAR) compile skip_deps=true

rel: clean
	rm -rf ./rel/habanero
	@$(REBAR) compile generate force=1


