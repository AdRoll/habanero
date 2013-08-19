#!/bin/sh

# Usage:
#       ./habanero.sh -config CONFIGNAME
#
# Or to run headless...
#       ./habanero.sh -config CONFIGNAME -detached
#
: ${config:=habanero}
erl -pa ebin deps/*/ebin -args_file vm.args -config ${config} $@