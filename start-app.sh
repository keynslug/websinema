#!/bin/sh

export ERL_LIBS="deps/:${ERL_LIBS}"
erl -pa ebin -boot start_sasl -s reloader -s websinema -config default

