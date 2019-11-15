srcdir := website
outdir := dist
lisp := $(shell find $(srcdir) -type f -name '*.lisp')
lisp_deps := www.nolanwright.dev.asd \
             packages.lisp \
             $(shell find shh-html -type f) \
             $(shell find shh-utils -type f)
markup := $(lisp:$(srcdir)/%.lisp=$(outdir)/%.html)
style := $(outdir)/style.css
# We also may need .png assets in addition to the favicon.ico.
assets := $(outdir)/favicon.ico $(outdir)/icons.svg
other := $(outdir)/pgpkey.asc

.PHONY: all serve clean
.INTERMEDIATE: $(outdir)/tailwind.css $(outdir)/tailwind.min.css

all: $(style) $(assets) $(other)

$(style): $(outdir)/tailwind.min.css $(outdir)/tailwind.min.css $(markup)
	npx purgecss -c purgecss.config.js -o $(@D)
	mv $(@D)/tailwind.min.css $@

$(outdir)/tailwind.min.css: $(outdir)/tailwind.css
	npx cleancss -O1 'all:on;specialComments:0' -O2 'all:on' -o $@ $<

$(outdir)/tailwind.css: $(srcdir)/style.css tailwind.config.js
	mkdir -p $(@D)
	npx tailwind build $< -o $@ > /dev/null

$(outdir)/%.html: $(outdir)/%.unmin.html
	npx html-minifier -c html-minifier.config.json -o $@ $<

$(outdir)/%.unmin.html: $(srcdir)/%.lisp $(lisp_deps)
	mkdir -p $(@D)
	sbcl --noinform \
	     --eval "(require 'asdf)" \
	     --eval "(import 'asdf:defsystem)" \
       --eval "(setf *load-verbose* nil)" \
       --eval "(setf *load-print* nil)" \
       --eval "(setf *compile-verbose* nil)" \
       --eval "(setf *compile-print* nil)" \
       --load www.nolanwright.dev.asd \
       --eval '(asdf:load-system "www.nolanwright.dev")' \
       --eval "(use-package 'shh-utils)" \
       --script $< > $@

$(outdir)/favicon.ico:
	mkdir -p $(@D)
	cp $(srcdir)/favicon.ico $(@D)

$(outdir)/icons.svg:
	mkdir -p $(@D)
	curl -Ls https://unpkg.com/feather-icons/dist/feather-sprite.svg > $@.temp
	scripts/purgesvgdefs $@.temp mail message-circle github gitlab > $@
	rm $@.temp

$(outdir)/pgpkey.asc:
	mkdir -p $(@D)
	gpg2 --armor --export nolan@nolanwright.dev > $@

serve: all
	npx http-server dist -o

clean:
	rm -rf $(outdir)
