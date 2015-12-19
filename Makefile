emacs ?= emacs

el = $(wildcard *.el)
elc = $(el:%.el=%.elc)
ert_test_el = $(wildcard test/*.el)

all: $(elc)

.PHONY: all test test-ert build clean

clean:
	$(RM) $(elc)

test: build test-ert

build: $(elc)

$(elc): %.elc: %.el
	$(emacs) -batch -Q -L mocks -f batch-byte-compile $<


test-ert:
	$(emacs) -batch -Q -L . --eval "(require 'ert)" $(ert_test_el:%=-l "%") \
		-f ert-run-tests-batch-and-exit
