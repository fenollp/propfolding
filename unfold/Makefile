UNFOLD = +'{parse_transform,unfold_parse_transform}'

all: ebin/unfold_parse_transform.beam
	erlc -o ebin -pa ebin test/unfold_tests_a.erl
	erl -noshell -pa ebin -eval '{r,undefined,"DEUX","trois",undefined,"cinq"} = unfold_tests_a:t().' -s init stop
	erlc -o ebin -pa ebin $(UNFOLD) +to_asm test/unfold_tests_a.erl
	erlc -o ebin -pa ebin $(UNFOLD) test/unfold_tests_a.erl
	erl -noshell -pa ebin -eval '{r,undefined,"DEUX","trois",undefined,"cinq"} = unfold_tests_a:t().' -s init stop
#
	make tests_b
	make tests_c
	make tests_d
	# make tests_e
#
	bash -c '[[ 0 -eq $$(git status --porcelain ebin/*.S | wc -l) ]]'

ebin/unfold_parse_transform.beam: src/unfold_parse_transform.erl
	cd deps/parse_trans && make
	erlc -o ebin -pa deps/parse_trans/ebin src/unfold_parse_transform.erl

test: tests_c
tests_%: ebin/unfold_parse_transform.beam
	erlc -o ebin -pa ebin test/unfold_$@.erl
	erl -noshell -pa ebin -eval 'R = unfold_tests_a:t(), R = unfold_$@:t().' -s init stop
	erlc -o ebin -pa ebin $(UNFOLD) +to_asm test/unfold_$@.erl
	erlc -o ebin -pa ebin $(UNFOLD) test/unfold_$@.erl
	erl -noshell -pa ebin -eval 'R = unfold_tests_a:t(), R = unfold_$@:t().' -s init stop
