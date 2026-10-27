#!/bin/bash
# Run every automated check: shell lint, byte-compilation, checkdoc,
# package-lint when available, and the ERT suites.
#
# The elisp dependencies are looked up in the straight.el build directory
# this checkout normally lives next to.  Point MARKDOWN_MODE_DIR,
# CONSULT_DIR and COMPAT_DIR at the directories holding markdown-mode.el,
# consult.el and compat.el to use others.  package-lint runs when
# PACKAGE_LINT_DIR (or its straight build) holds package-lint.el.
set -euo pipefail

cd "$(dirname "$0")"

EMACS=${EMACS:-emacs}
SOURCES=(claude-archive.el claude-archive-consult.el)

emacs_version() {
	"$EMACS" --batch --eval '(princ emacs-version)'
}

straight_build_dir() {
	echo "../../build-$(emacs_version)"
}

MARKDOWN_MODE_DIR=${MARKDOWN_MODE_DIR:-$(straight_build_dir)/markdown-mode}
CONSULT_DIR=${CONSULT_DIR:-$(straight_build_dir)/consult}
COMPAT_DIR=${COMPAT_DIR:-$(straight_build_dir)/compat}
PACKAGE_LINT_DIR=${PACKAGE_LINT_DIR:-$(straight_build_dir)/package-lint}

check_dependencies() {
	local dir
	for dir in "$MARKDOWN_MODE_DIR" "$CONSULT_DIR" "$COMPAT_DIR"; do
		if [ ! -d "$dir" ]; then
			echo "missing elisp dependency directory: $dir" >&2
			echo "set MARKDOWN_MODE_DIR, CONSULT_DIR and COMPAT_DIR" >&2
			exit 1
		fi
	done
}

run_emacs() {
	"$EMACS" --batch -Q -L "$MARKDOWN_MODE_DIR" -L "$CONSULT_DIR" \
		-L "$COMPAT_DIR" -L . -L test "$@"
}

lint_shell() {
	echo "== shellcheck"
	if command -v shellcheck >/dev/null; then
		shellcheck check.sh
	else
		echo "skipped: shellcheck not found"
	fi
}

byte_compile() {
	echo "== byte-compile"
	local out
	out=$(mktemp -d)
	# Compile into a scratch directory so no .elc shadows the sources.
	run_emacs --eval "(setq byte-compile-error-on-warn t
                            byte-compile-dest-file-function
                            (lambda (f) (expand-file-name
                                         (concat (file-name-nondirectory f) \"c\")
                                         \"$out\")))" \
		-f batch-byte-compile "${SOURCES[@]}"
	rm -rf "$out"
}

run_checkdoc() {
	echo "== checkdoc"
	local file
	for file in "${SOURCES[@]}" test/*.el; do
		run_emacs --eval "(progn
                            (require 'checkdoc)
                            (setq checkdoc-diagnostic-buffer \"*checkdoc*\")
                            (checkdoc-file \"$file\")
                            (let ((report (get-buffer \"*checkdoc*\")))
                              (when (and report
                                         (not (string-blank-p
                                               (with-current-buffer report
                                                 (buffer-string)))))
                                (kill-emacs 1))))"
	done
}

run_package_lint() {
	echo "== package-lint"
	if [ ! -d "$PACKAGE_LINT_DIR" ]; then
		echo "skipped: no package-lint in $PACKAGE_LINT_DIR"
		return
	fi
	run_emacs -L "$PACKAGE_LINT_DIR" --eval "(require 'package-lint)" \
		--eval "(setq package-lint-main-file \"claude-archive.el\")" \
		-f package-lint-batch-and-exit "${SOURCES[@]}"
}

run_ert() {
	echo "== ERT"
	# ert-quiet lists only the tests that fail.
	run_emacs -l test/claude-archive-test.el -l test/claude-archive-consult-test.el \
		--eval "(setq ert-quiet t)" -f ert-run-tests-batch-and-exit
}

check_dependencies
lint_shell
byte_compile
run_checkdoc
run_package_lint
run_ert
