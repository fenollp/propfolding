SUPERC = +'{parse_transform, superc}'
ERLC = erlc -o ebin -pa ebin -pa deps/parse_trans/ebin
ERL = erl -noshell -pa ebin +A0 -boot start_clean

all: ebin/superc.beam
	make tests_c
#
	# bash -c '[[ 0 -eq $$(git status --porcelain ebin/*.S | wc -l) ]]'

ebin/superc.beam: src/superc.erl
	cd deps/parse_trans && make
	mkdir -p ebin
	$(ERLC) src/superc.erl

test: tests_c
tests_%: ebin/superc.beam
	# $(ERLC) +to_asm test/unfold_$@.erl
	# mv ebin/unfold_$@.S ebin/unfold_$@_.S
	# $(ERLC) test/unfold_$@.erl
	$(ERLC) $(SUPERC) +to_asm test/unfold_$@.erl
	# $(ERLC) $(SUPERC) test/unfold_$@.erl
