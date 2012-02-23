all:
	./rebar get-deps
	./rebar compile

run: all
	ERL_LIBS=deps erl -pa apps/*/ebin -config dev -s bchat_app start

clean:
	./rebar clean

depclean:
	rm -Rf deps/*

backup:
	rm -rf rel/bchat_bak
	if test -d "rel/bchat"; then mv rel/bchat rel/bchat; else echo "No old release."; fi

release: backup
	./rebar generate

appup:
	./rebar generate-appups previous_release=bchat_bak

startrel:
	./rel/bchat/bin/bchat start

stoprel:
	./rel/bchat/bin/bchat stop