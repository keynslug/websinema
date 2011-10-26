REBAR=./rebar

.PHONY: all compile get-deps clean distclean

all: clean compile

compile: get-deps
	${REBAR} compile

get-deps:
	${REBAR} get-deps

clean:
	${REBAR} clean
	rm -rfv erl_crash.dump

distclean: clean
	rm -rfv ebin deps
