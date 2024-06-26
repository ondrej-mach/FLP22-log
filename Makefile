all: src/flp22-log.pl src/input2.pl
	swipl -q -nodebug -G16g -g start -o flp22-log -c src/flp22-log.pl || swipl -q -nodebug --stack_limit=16g -g start -o flp22-log -c src/flp22-log.pl

README.pdf: README.md
	pandoc -s -o README.pdf -V geometry:margin=25mm -V lang:cs -V papersize:a4 README.md

.PHONY: test
test: all
	# Test if IO works correctly
	swipl -g read_and_print -s src/flp22-log.pl < test/spec_example.in | diff -B test/spec_example.in -
	swipl -g read_and_print -s src/flp22-log.pl < test/all_letters.in | diff -B test/all_letters.in -
	swipl -g read_and_print -s src/flp22-log.pl < test/two_digit.in | diff -B test/two_digit.in -

	# Test the example case given in specification
	#swipl -g read_and_print -s src/flp22-log.pl < test/spec_example.in | diff test/spec_example.out -

.PHONY: run
run: all
	./flp22-log < test/spec_example.in
