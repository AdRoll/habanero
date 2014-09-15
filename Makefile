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

copy_data:
	@cp -rf ./priv ./rel/files
	@cp -f ./habanero.config ./rel/files/sys.config

rel: clean compile copy_data
	@rm -rf ./rel/habanero && cd rel && ../rebar generate -f


