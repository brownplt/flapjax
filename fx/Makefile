CLOSURELIB=../tools/closure-library
CLOSUREBUILDER=$(CLOSURELIB)/closure/bin/build/closurebuilder.py
JSONEXTERN=../tools/json.js 
CLOSURE=../tools/compiler.jar

all: flapjax.min.js

 
flapjax.min.js: flapjax-impl.js
	java -jar $(CLOSURE) \
		--externs $(JSONEXTERN) \
		--language_in ECMASCRIPT5 \
		--warning_level VERBOSE \
		--js    flapjax-impl.js \
		--js_output_file flapjax.min.js

flapjaxlib.min.js: flapjax-impl.js
	$(CLOSUREBUILDER) \
		--root=$(CLOSURELIB)/closure \
		--root=$(CLOSURELIB)/third_party \
		--root=. \
		--namespace="F" \
		--compiler_jar=$(CLOSURE) \
		--output_mode="compiled" \
		--compiler_flags="--js_output_file=\"flapjaxlib.min.js\"" \
		--compiler_flags="--warning_level=VERBOSE" \
		--compiler_flag="--compilation_level=WHITESPACE_ONLY" \
		--compiler_flag="--formatting=PRETTY_PRINT"

clean:
	rm -f flapjax.debug.js
	rm -f flapjax.js

doc:
	cd ../tools/jsdoc-toolkit && \
	java -jar jsrun.jar app/run.js -a -t=templates/jsdoc \
	--directory=../../docs ../../fx/flapjax-impl.js -p
