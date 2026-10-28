# pi-deck Makefile

EMACS ?= emacs
PILISH_DIR ?= ../pilish
ORCHESTRATION_DIR ?= ../pi-ant/orchestration/emacs
EMACS_MAJOR_VERSION := $(shell $(EMACS) --batch -Q --eval '(princ emacs-major-version)' 2>/dev/null)
PACKAGE_USER_DIR ?= $(abspath .cache/elpa/$(EMACS_MAJOR_VERSION))
export PACKAGE_USER_DIR
SELECTOR ?=

BATCH = $(EMACS) --batch -Q -L . -L test -L $(PILISH_DIR) \
	--eval "(progn (setq load-prefer-newer t package-user-dir (getenv \"PACKAGE_USER_DIR\")) (package-initialize))"

SOURCES = $(wildcard pi-deck*.el)

.PHONY: deps compile test smoke check clean

.deps-stamp: scripts/install-deps.el
	$(EMACS) --batch -Q -l scripts/install-deps.el
	touch $@

deps: .deps-stamp

compile: deps
	$(BATCH) --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile $(SOURCES)

test: compile
	$(BATCH) -L $(ORCHESTRATION_DIR) -l ert -l test/pi-deck-test.el \
	  --eval "(ert-run-tests-batch-and-exit \"$(SELECTOR)\")"

# Real Pi sessions against an in-process faux provider in a disposable daemon.
smoke: compile
	PILISH_DIR=$(abspath $(PILISH_DIR)) ORCHESTRATION_DIR=$(abspath $(ORCHESTRATION_DIR)) \
	  EMACS=$(EMACS) test/smoke.sh

check: compile test

clean:
	rm -f *.elc test/*.elc .deps-stamp
