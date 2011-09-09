JSCC = jscc
COMPILERDATA = ../compiler/data
WEBSITEFX = ../website/build/fx

all: flapjax.debug.js flapjax.js flapjax.jsi

clean:
	rm -f flapjax.debug.js
	rm -f flapjax.js

flapjax.debug.js: flapjax-impl.js flapjax.jsi
	cp flapjax-impl.js flapjax.debug.js
	cp flapjax.debug.js $(COMPILERDATA)/flapjax.debug.js
	mkdir $(WEBSITEFX) || true
	cp flapjax.debug.js $(WEBSITEFX)/flapjax.debug.js

flapjax.js: flapjax-impl.js flapjax.jsi
	cp flapjax-impl.js flapjax.js
	cp flapjax.js $(COMPILERDATA)/flapjax.js
	mkdir $(WEBSITEFX) || true
	cp flapjax.js $(WEBSITEFX)/flapjax.js
	cp flapjax.jsi $(COMPILERDATA)/flapjax.jsi
	cp flapjax.jsi $(WEBSITEFX)/flapjax.jsi
	cp flapjax-impl.js $(WEBSITEFX)/flapjax-impl.js
