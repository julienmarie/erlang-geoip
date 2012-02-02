all: compile

compile:
	./rebar compile

dev: compile
	erl -pa ebin

clean:
	./rebar clean
