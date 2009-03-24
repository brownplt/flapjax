JSCC = jscc
COMPILERDATA = ../compiler/data

all: flapjax.debug.js flapjax.js
	cp flapjax.debug.js $(COMPILERDATA)/flapjax.debug.js
	cp flapjax.js $(COMPILERDATA)/flapjax.js
	cp flapjax.jsi $(COMPILERDATA)/flapjax.jsi

clean:
	rm -f flapjax.debug.js
	rm -f flapjax.js

flapjax.debug.js: flapjax-impl.js flapjax.jsi
	$(JSCC) --namespace flapjax --interface flapjax.jsi flapjax-impl.js > flapjax.debug.js

flapjax.js: flapjax-impl.js flapjax.jsi
	$(JSCC) --release --namespace flapjax --interface flapjax.jsi flapjax-impl.js > flapjax.js
