all: build build/index.html build/single-page.css

clean:
	rm -rf build

build:
	mkdir build || true

build/index.html: doc.html fxdoc/make-doc.ss
	cd fxdoc && ./make-doc.ss
	rsync -rlptz --delete build/ ../../website/build/docs

build/single-page.css: single-page.css
	cp single-page.css build/
	rsync -rlptz --delete build/ ../../website/build/docs
