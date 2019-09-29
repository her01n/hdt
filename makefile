default: .tested

GUILE_CONFIG ?= guile-config
GUILE_SITE ?= $(shell $(GUILE_CONFIG) info sitedir)

.tested: bin/hdt test/*.scm hdt/*.scm
	rm -rf ~/.cache/guile
	bin/hdt
	touch $@

install: .tested
	install bin/hdt /usr/local/bin/hdt
	mkdir -p $(GUILE_SITE)/hdt
	install -t $(GUILE_SITE)/hdt hdt/*

clean:
	rm -rf .tested

uninstall:
	rm -rf /usr/local/bin/hdt
	rm -rf $(GUILE_SITE)/hdt

