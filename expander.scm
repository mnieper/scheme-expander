(import (scheme base)
	(scheme process-context)
	(rapid error)
	(rapid programs)
	(rapid expressions))

(guard-compile
 (let*
     ((program (program-read (list-ref (command-line) 1)))
      (expression (program-expand program))
      (form (expression->datum expression)))
   (write '(import (rapid)))
   (newline)
   (write form)))
