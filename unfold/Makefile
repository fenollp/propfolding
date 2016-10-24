all:
	cd deps/parse_trans && make
	mkdir -p ebin
	erlc -o ebin -pa deps/parse_trans/ebin src/unfold_parse_transform.erl
	erlc -o ebin -pa ebin test/unfold_tests_?.erl
	erl -noshell -pa ebin -eval 'R = unfold_tests_a:a(), R=unfold_tests_b:b(), R=unfold_tests_c:c(), R=unfold_tests_d:d().' -s init stop
