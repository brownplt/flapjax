.PHONY: build

SHTMLS = $(shell find static -name "*.shtml")

all: build-static build .static

clean:
	rm -rf build
	rm -f .static
	rm -f build-static

# Compile build-static for faster builds
build-static: build-static.hs
	ghc --make build-static.hs
	rm build-static.o
	rm build-static.hi

# Don't create build if it already exists
ifeq ($(wildcard build),build)
build:
else
build:
	mkdir build
endif

# Use the timestamp on .static to determine if we need to build-static
.static: $(SHTMLS) build-static
	./build-static static build
	touch .static
