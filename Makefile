# Copyright (c) 2013 Richard Mortier <mort@cantab.net>
#
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
# SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
# OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
# CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

.PHONY: all configure build clean
FLAGS ?=
CFLAGS ?= --annot
BFLAGS ?= 

OCAMLDOC=ocamlfind ocamldoc -html -sort -v -colorize-code -all-params

all: configure build

configure:
	obuild configure $(CFLAGS)

build: configure
	obuild build $(BFLAGS)

clean:
	obuild clean
	$(RM) -r doc
	find . -name "*.annot" | xargs rm

annot: 
	( cd lib &&                                                     \
	  for n in `\ls -1 ../dist/build/lib-revisions/*.annot` ; do    \
	    ln -sf $$n ;                                                \
	  done                                                          \
	)

doc: $(wildcard lib/*.ml*)
	( mkdir -p doc && cd doc &&						\
	  for n in `\ls -1 ../lib/revisions.mli` ; do	\
	    $(OCAMLDOC) $$n ;							\
	  done											\
	)
