all: build build/doc.html build/single-page.css

clean:
	rm -rf build

build:
	mkdir build || true

build/doc.html: doc.html fxdoc/make-doc.ss
	cd fxdoc && ./make-doc.ss

build/single-page.css: single-page.css
	cp single-page.css build/
