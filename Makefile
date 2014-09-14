.PHONY: rel deps

REBAR=./rebar

all: deps compile

deps:
	@$(REBAR) get-deps 

clean:
	@$(REBAR) clean

clean_all:
	@$(REBAR) clean delete-deps

compile:
	@$(REBAR) compile

quick_compile:
	@$(REBAR) compile skip_deps=true

rel: clean compile
	@cd rel && rm -rf habernero && ../rebar generate -f


