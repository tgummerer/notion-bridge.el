EMACS ?= emacs
LOAD_PATH = -L .

TEST_FILES = \
	test/notion-bridge-cache-test.el \
	test/notion-bridge-org-test.el \
	test/notion-bridge-api-test.el \
	test/notion-bridge-sync-test.el

.PHONY: test test-cache test-org test-api test-sync clean

test: $(TEST_FILES)
	$(EMACS) -Q -batch $(LOAD_PATH) -l ert \
		$(foreach f,$(TEST_FILES),-l $(f)) \
		-f ert-run-tests-batch-and-exit

test-cache:
	$(EMACS) -Q -batch $(LOAD_PATH) -l ert \
		-l test/notion-bridge-cache-test.el \
		-f ert-run-tests-batch-and-exit

test-org:
	$(EMACS) -Q -batch $(LOAD_PATH) -l ert \
		-l test/notion-bridge-org-test.el \
		-f ert-run-tests-batch-and-exit

test-api:
	$(EMACS) -Q -batch $(LOAD_PATH) -l ert \
		-l test/notion-bridge-api-test.el \
		-f ert-run-tests-batch-and-exit

test-sync:
	$(EMACS) -Q -batch $(LOAD_PATH) -l ert \
		-l test/notion-bridge-sync-test.el \
		-f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc test/*.elc
