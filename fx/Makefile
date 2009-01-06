JSCC = /Users/arjun/Projects/javascript-contracts/dist/build/jscc/jscc

all: flapjax.debug.js flapjax.js

clean:
	rm -f flapjax.debug.js
	rm -f flapjax.js

flapjax.debug.js: flapjax-impl.js flapjax.jsi
	$(JSCC) --interface flapjax.jsi flapjax-impl.js > flapjax.debug.js

flapjax.js: flapjax-impl.js flapjax.jsi
	$(JSCC) --release --interface flapjax.jsi flapjax-impl.js > flapjax.js
