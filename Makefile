DUNE := dune
INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

default:
	$(DUNE) build

install:
	$(DUNE) install $(INSTALL_ARGS)

uninstall:
	$(DUNE) uninstall $(INSTALL_ARGS)

reinstall: uninstall install

clean:
	$(DUNE) clean

.PHONY: default install uninstall reinstall clean
