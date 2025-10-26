(in-package :mud.server)

;; Load all command modules
(load (merge-pathnames "commands/package.lisp" *load-truename*))