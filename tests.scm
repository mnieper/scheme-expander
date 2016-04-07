(import (scheme base)
        (scheme eval)
	(rapid test)
	(rapid error)
	(rapid expansion)
	(rapid programs)
	(rapid expressions))

(define rapid-environment (environment '(rapid)))

(test-begin "Rapid Expander")

(parameterize
    ((current-search-paths '("." "share")))
  (guard-compile
   (let*
       ((program (program-read "hello-world.scm"))
	(expression (program-expand program))
	(form (expression->datum expression))
	(result (parameterize ((current-output-port (open-output-string)))
		  (eval form rapid-environment)
		  (get-output-string (current-output-port)))))
     (test-equal "A simple Hello World-program"
		 "Hello, World!\n"
		 result))))

(test-end "Rapid Expander")
