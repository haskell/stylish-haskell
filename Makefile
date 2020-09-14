ARCH=$(shell uname -m)
UNAME=$(shell uname | tr 'A-Z' 'a-z')

STYLISH_BINARY=$(HOME)/.local/bin/stylish-haskell
STYLISH_TAG?=v$(shell sed -n 's/^Version: *//p' *.cabal)
STYLISH_PACKAGE=stylish-haskell-$(STYLISH_TAG)-$(UNAME)-$(ARCH)

UPX_VERSION=3.94
UPX_NAME=upx-$(UPX_VERSION)-amd64_$(UNAME)
UPX_BINARY=$(HOME)/.local/bin/upx

ifeq ($(UNAME), darwin)
ARCHIVE=zip
ARCHIVE_CREATE=zip -r
ARCHIVE_EXTRACT=unzip
else
ARCHIVE=tar.gz
ARCHIVE_CREATE=tar czf
ARCHIVE_EXTRACT=tar xvzf
endif

ifeq ($(UNAME), darwin)
COMPRESS_BIN_DEPS=
COMPRESS_BIN=ls
else
COMPRESS_BIN_DEPS=$(UPX_BINARY)
COMPRESS_BIN=upx
endif

STACK=stack --system-ghc

# Default target.
.PHONY: build
build: $(STYLISH_BINARY)

# When we want to do a release.
.PHONY: artifact
artifact: $(STYLISH_PACKAGE).$(ARCHIVE)
	mkdir -p artifacts
	cp $(STYLISH_PACKAGE).$(ARCHIVE) artifacts/

$(STYLISH_PACKAGE).$(ARCHIVE): $(STYLISH_BINARY) $(COMPRESS_BIN_DEPS)
	mkdir -p $(STYLISH_PACKAGE)
	cp $(STYLISH_BINARY) $(STYLISH_PACKAGE)/
	$(COMPRESS_BIN) $(STYLISH_PACKAGE)/stylish-haskell
	cp README.markdown $(STYLISH_PACKAGE)/
	cp CHANGELOG $(STYLISH_PACKAGE)/
	cp LICENSE $(STYLISH_PACKAGE)/
	$(ARCHIVE_CREATE) $(STYLISH_PACKAGE).$(ARCHIVE) $(STYLISH_PACKAGE)

$(STYLISH_BINARY):
	$(STACK) build --copy-bins

# UPX is used to compress the resulting binary.  We currently don't use this on
# Mac OS.
$(UPX_BINARY):
	curl -Lo /tmp/$(UPX_NAME).tar.xz \
	    https://github.com/upx/upx/releases/download/v$(UPX_VERSION)/$(UPX_NAME).tar.xz
	cd /tmp && tar xf $(UPX_NAME).tar.xz
	mv /tmp/$(UPX_NAME)/upx $(UPX_BINARY)
	upx --version

.PHONY: test
test:
	stack build --test
