default: test
test: bin/hdt test/*.scm hdt/*.scm
	rm -rf ~/.cache/guile
	bin/hdt
install: test
	install bin/hdt /usr/local/bin/hdt
	mkdir -p /usr/local/share/guile/hdt
	install -t /usr/local/share/guile/hdt hdt/*

.PHONY: test