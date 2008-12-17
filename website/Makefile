.PHONY: build doc fx static

all: build doc fx static

clean:
	rm -rf build

build:
	mkdir build || true

doc:
	cd ../doc && $(MAKE)
	rsync -rlptz --delete ../doc/build/ build/docs

fx:
	cd ../fx && $(MAKE)
	rsync -rlptz --delete ../fx/ build/fx

static:
	./build-static.hs static build
