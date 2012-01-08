all: closure-library closure-compiler jsdoc-toolkit

closure-library:
	svn checkout http://closure-library.googlecode.com/svn/trunk/ closure-library

closure-compiler: closure-compiler.zip
	unzip closure-compiler -d closure-compiler

closure-compiler.zip:
	curl http://closure-compiler.googlecode.com/files/compiler-latest.zip > closure-compiler.zip

jsdoc-toolkit: jsdoc-toolkit.zip
	unzip jsdoc-toolkit -d jsdoc-toolkit

jsdoc-toolkit.zip:
	curl http://jsdoc-toolkit.googlecode.com/files/jsdoc_toolkit-2.4.0.zip > jsdoc-toolkit.zip
