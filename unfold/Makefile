all:
	cd deps/parse_trans && make
	mkdir -p ebin
	erlc -o ebin -pa deps/parse_trans/ebin src/unfold_parse_transform.erl
	erlc -o ebin -pa ebin test/unfold_tests_?.erl
