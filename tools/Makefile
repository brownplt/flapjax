all: closure-library closure-compiler

closure-library:
	svn checkout http://closure-library.googlecode.com/svn/trunk/ closure-library

closure-compiler: closure-compiler.zip
	unzip closure-compiler -d closure-compiler

closure-compiler.zip:
	curl http://closure-compiler.googlecode.com/files/compiler-latest.zip > closure-compiler.zip

