default: test
test: bin/hdt test/*.scm hdt/*.scm
	rm -rf ~/.cache/guile
	bin/hdt
install: test
	install bin/hdt /usr/local/bin/hdt
	mkdir -p /usr/share/guile/site/hdt
	install -t /usr/share/guile/site/hdt hdt/*
uninstall:
	rm -rf /usr/local/bin/hdt
	rm -rf /usr/share/guile/site/hdt

.PHONY: test
