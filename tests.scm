(import (scheme base)
        (scheme eval)
	(rapid test)
	(rapid programs)
	(rapid expressions))

(test-begin "Rapid Expander")

(let*
    ((program (program-read "hello-world.scm"))
     (expression (program-expand program))
     (form (expression->datum expression))
     (result (parameterize ((current-output-port (open-output-string)))
               (eval form (environment '(rapid primitive)))
	       (get-output-string (current-output-port)))))
  (test-equal "A simple Hello World-program"
    "Hello, World!\n"
    result))

(test-end "Rapid Expander")
