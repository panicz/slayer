AUTOMAKE_OPTIONS = color-tests
ACLOCAL_AMFLAGS = -I m4
SUBDIRS = src doc tools guile-modules 
EXTRA_DIST = demos
TESTS = test-suite/schess/elements.sh test-suite/tools/ply23d.sh \
	test-suite/tools/ply23d-unit.sh

.PHONY : etags

etags:
	find ./ -name '*.c' -or -name '*.h' -or -name '*.cc' \
		-or -name '*.hh' -or -name '*.scm' \
	| grep -v .hg \
	| grep -v ignore \
	| grep -v scmutils/ \
	| etags -
