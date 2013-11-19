PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)

REBAR=./rebar
DIALYZER=`which dialyzer`
REVISION:=`git rev-parse HEAD`
TAR_FILE:=bodhi-$(REVISION).tar.bz2

all: compile

deps:
	@$(REBAR) get-deps deps_dir="deps"

compile: deps
	@$(REBAR) compile

doc:
	@$(REBAR) skip_deps=true doc

clean:
	@$(REBAR) clean

app.plt:
	@$(DIALYZER) --build_plt --output_plt app.plt --apps erts kernel stdlib crypto deps/*

dialyzer: app.plt
	@$(DIALYZER)  --plt app.plt ebin -Wunmatched_returns \
	-Werror_handling -Wrace_conditions -Wno_undefined_callbacks

xref:
	@$(REBAR) xref skip_deps=true

test: compile
	@$(REBAR) eunit skip_deps=true verbose=1

stress: validate
	@$(REBAR) ct skip_deps=true verbose=1

validate: compile xref dialyzer test

release:
	tar cjvf /tmp/$(TAR_FILE) --exclude=\.git deps ebin include bin priv production.config vm.args bodhi.sh
	s3cmd put /tmp/$(TAR_FILE) s3://adroll-releases/bodhi/$(TAR_FILE)

.PHONY: doc
