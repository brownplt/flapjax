CLOSURELIB=../tools/closure-library
CLOSUREBUILDER=$(CLOSURELIB)/closure/bin/build/closurebuilder.py
JSONEXTERN=../tools/json.js 
CLOSURE=../tools/closure-compiler/compiler.jar

all:
	$(CLOSUREBUILDER) \
		--root=$(CLOSURELIB)/closure \
		--root=$(CLOSURELIB)/third_party \
		--root=. \
		--namespace="F" \
		--compiler_jar=$(CLOSURE) \
		--output_mode="compiled" \
		--compiler_flags="--js_output_file=\"../out/flapjax.min.js\"" \
		--compiler_flags="--warning_level=VERBOSE" \
		--compiler_flag="--compilation_level=WHITESPACE_ONLY" \
		--compiler_flag="--formatting=PRETTY_PRINT"


doc:
	cd ../tools/jsdoc-toolkit/jsdoc_toolkit-2.4.0/jsdoc-toolkit && \
	java -jar jsrun.jar app/run.js -a -t=templates/jsdoc \
	--directory=../../../../new-doc ../../../../new-src/flapjax.js
