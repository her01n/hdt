default: .tested

GUILE ?= guile

.tested: bin/hdt test/*.scm hdt/*.scm
	rm -rf ~/.cache/guile
	bin/hdt
	touch $@

clean:
	rm -rf .tested

GUILE_SITE_DIR ?= $(shell $(GUILE) -c "(display (%site-dir)) (newline)")

install:
	install -D --target-directory=$(GUILE_SITE_DIR)/hdt hdt/*.scm
	install -D --target-directory=/usr/local/bin bin/hdt

uninstall:
	rm -rf /usr/local/bin/hdt
	rm -rf $(GUILE_SITE_DIR)/hdt

