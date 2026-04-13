PREFIX ?= /usr/local
BINDIR  = $(PREFIX)/bin

.PHONY: build clean install uninstall

build:
	raco make main.rkt
	raco exe -o rl main.rkt

clean:
	rm -rf compiled rl

install: build
	install -d $(BINDIR)
	install -m 755 rl $(BINDIR)/rl

uninstall:
	rm -f $(BINDIR)/rl
