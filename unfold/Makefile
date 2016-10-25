all:
	cd deps/parse_trans && make
	erlc -o ebin -pa deps/parse_trans/ebin src/unfold_parse_transform.erl
#
	erlc -o ebin -pa ebin +to_asm test/unfold_tests_a.erl
	erlc -o ebin -pa ebin test/unfold_tests_a.erl
	erl -noshell -pa ebin -eval '{r,undefined,"DEUX","trois",undefined,"cinq"} = unfold_tests_a:t().' -s init stop
#
	erlc -o ebin -pa ebin +to_asm test/unfold_tests_b.erl
	erlc -o ebin -pa ebin test/unfold_tests_b.erl
	erl -noshell -pa ebin -eval 'R = unfold_tests_a:t(), R=unfold_tests_b:t().' -s init stop
#
	erlc -o ebin -pa ebin +to_asm test/unfold_tests_c.erl
	erlc -o ebin -pa ebin test/unfold_tests_c.erl
	erl -noshell -pa ebin -eval 'R = unfold_tests_a:t(), R=unfold_tests_c:t().' -s init stop
#
	erlc -o ebin -pa ebin +to_asm test/unfold_tests_d.erl
	erlc -o ebin -pa ebin test/unfold_tests_d.erl
	erl -noshell -pa ebin -eval 'R = unfold_tests_a:t(), R=unfold_tests_d:t().' -s init stop
#
	# erlc -o ebin -pa ebin +to_asm test/unfold_tests_e.erl
	# erlc -o ebin -pa ebin test/unfold_tests_e.erl
	# erl -noshell -pa ebin -eval 'R = unfold_tests_a:t(), R=unfold_tests_e:t().' -s init stop
#
	bash -c '[[ 0 -eq $$(git status --porcelain ebin/*.S | wc -l) ]]'

test: tests_c
tests_%:
	erlc -o ebin -pa deps/parse_trans/ebin src/unfold_parse_transform.erl
	erlc -o ebin -pa ebin +to_asm test/unfold_$@.erl
