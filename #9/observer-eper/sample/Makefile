all: compile

compile: deps
	./rebar compile

deps:
	./rebar get-deps

clean:
	./rebar clean

console: compile
	erl -pa ebin deps/*/ebin -setcookie sample -name sample@localhost.localdomain

sync-console: compile
	erl -pa ebin deps/*/ebin -run sync go -setcookie sample -name sample@localhost.localdomain

attach:
	erl -pa ebin deps/*/ebin -setcookie sample -remsh sample@localhost.localdomain -name attach-sample
