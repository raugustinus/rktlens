PREFIX ?= /usr/local
BINDIR  = $(PREFIX)/bin

.PHONY: build clean install uninstall

build:
	raco make main.rkt
	raco exe -o rl-bin main.rkt

clean:
	rm -rf compiled rl-bin

install: build
	install -d $(BINDIR)
	install -m 755 rl-bin $(BINDIR)/rl-bin
	install -m 755 bin/rl.sh $(BINDIR)/rl

uninstall:
	rm -f $(BINDIR)/rl $(BINDIR)/rl-bin
