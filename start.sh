#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/apps/*/ebin $PWD/deps/*/ebin .eunit -boot start_sasl -s lager -s sync 
