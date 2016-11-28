QL_SETUP=~/quicklisp/setup.lisp

contlife: contlife.lisp contlife.asd package.lisp systems.txt
	buildapp --manifest-file systems.txt \
		 --load-system contlife \
		 --entry contlife::main \
		 --output contlife

systems.txt: 
	sbcl --no-userinit --no-sysinit --non-interactive --load $(QL_SETUP) --eval '(ql:write-asdf-manifest-file "systems.txt")'

