JSCC = /Users/arjun/Projects/javascript-contracts/dist/build/jscc/jscc
CONTRACTLIB = /Users/arjun/Projects/javascript-contracts/data/contracts.js

all: flapjax.debug.js flapjax.js

clean:
	rm -f flapjax.debug.js
	rm -f flapjax.js

flapjax.debug.js: flapjax-impl.js flapjax.jsi $(JSCC) $(CONTRACTLIB)
	$(JSCC) --namespace flapjax --interface flapjax.jsi flapjax-impl.js > flapjax.debug.js

flapjax.js: flapjax-impl.js flapjax.jsi $(JSCC) $(CONTRACTLIB)
	$(JSCC) --release --namespace flapjax --interface flapjax.jsi flapjax-impl.js > flapjax.js
