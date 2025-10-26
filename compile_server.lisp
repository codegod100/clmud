(require :sb-bsd-sockets)
(load "src/packages.lisp")
(dolist (file '("src/server/core.lisp"
								"src/server/commands.lisp"
								"src/server/runtime.lisp"))
	(compile-file file))