;;; Rapid Scheme --- An expander for R7RS programs

;; Copyright (C) 2016 Marc Nieper-Wißkirchen

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define current-bindings (make-parameter #f box))
(define (%get-bindings) (unbox (current-bindings)))
(define (get-bindings) (reverse (%get-bindings)))
(define (set-bindings! bindings) (set-box! (current-bindings) bindings))
(define current-expressions (make-parameter #f box))
(define (%get-expressions) (unbox (current-expressions)))
(define (get-expressions) (reverse (%get-expressions)))
(define (set-expressions! expressions) (set-box! (current-expressions) expressions))
(define current-context (make-parameter 'top-level))

(define (top-level-context?) (eq? (current-context) 'top-level))
(define (body-context?) (eq? (current-context) 'body))
(define (expression-context?) (eq? (current-context) 'expression))

(define (make-%binding formals expression-syntax syntax)
  (vector formals expression-syntax syntax))
(define (expand-%binding %binding)
  (if (binding? %binding)
      %binding
      (make-binding
       (vector-ref %binding 0)
       (expand-expression (vector-ref %binding 1))
       (vector-ref %binding 2))))

(define (expand-into-transformer transformer syntax)
  ((%expand-into-transformer) transformer syntax))
(define %expand-into-transformer
  (make-parameter
   (lambda (transformer syntax)
     (compile-error "unexpected transformer spec" syntax))))  

(define (expand-into-expression expression)
  ((%expand-into-expression) expression))
(define %expand-into-expression (make-parameter #f))

(define (insert-location! identifier-syntax)
  (define location (make-location identifier-syntax))
  (insert-binding! identifier-syntax location)
  location)

;; XXX: Where do we check that no two var's are the same?
;; In insert-binding!? What about top-level then?
;; --> TODO: redefine, that is set! (or new location, etc.)
(define (expand-into-definition fixed-variables
				rest-variable
				formals-syntax
				expression-syntax
			       	definition-syntax)
  (when (expression-context?)
    (compile-error "unexpected definition" definition-syntax))
  (let ((expressions (%get-expressions)))
    (when (and expressions (not (null? expressions)))
      (compile-error "definitions may not follow expressions in a body" definition-syntax))
    (let*
	((fixed-locations (map insert-location! fixed-variables))
	 (rest-location (if rest-variable (insert-location! rest-variable) #f))
	 (formals (make-formals fixed-locations rest-location formals-syntax)))
      (set-bindings! (cons (make-%binding formals expression-syntax definition-syntax)
			   (%get-bindings))))))

(define (expand-into-syntax-definition identifier-syntax expander syntax)
  (when (expression-context?)
    (compile-error "unexpected syntax definition" syntax))
  (let ((expressions (%get-expressions)))
    (when (and expressions (not (null? expressions)))
      (compile-error "syntax definitions may not follow expressions in a body" syntax))
    (insert-binding! identifier-syntax expander)))

(define (expand-into-sequence syntax* syntax)
  (cond  
   ((eq? (current-context) 'expression)
    (when (null? syntax*)
      (compile-error "begin expression may not be empty" syntax))
    (make-sequence (map-in-order expand-expression syntax*) syntax))
   (else
    (for-each expand-syntax! syntax*))))
  
;; Expands a top level program or a library's body
(define (expand-top-level syntax*)
  (parameterize ((current-bindings '())
		 (%expand-into-expression
		  (lambda (expression)
		    (set-bindings! (cons (make-binding (make-dummy-formals) expression #f)
					 (%get-bindings))))))
    (for-each expand-syntax! syntax*)
    (parameterize ((current-context 'expression))
      (map-in-order expand-%binding (get-bindings)))))

;; Expands a procedure body
(define (expand-body syntax* syntax)
  (parameterize ((current-context 'body)
		 (current-bindings '())
		 (current-expressions '())
		 (%expand-into-expression
		  (lambda (expression)
		    (set-expressions! (cons expression (%get-expressions))))))
    (for-each expand-syntax! syntax*)
    (when (null? (%get-expressions))
      (compile-error "no expression in body" syntax))
    (parameterize ((current-context 'expression))
      (make-letrec*-expression
       (map expand-%binding (get-bindings))
       (get-expressions)
       #f))))

;; Expands a transformer
(define (expand-transformer syntax)
  (call-with-current-continuation
   (lambda (return)
     (parameterize ((current-context 'expression)
		    (%expand-into-expression
		     (lambda (expression)
		       (compile-error "not a macro transformer" syntax)))
		    (%expand-into-transformer
		     (lambda (transformer syntax)
		       (return transformer))))
       (expand-syntax! syntax)))))

;; Expands an expression
(define (expand-expression syntax)
  (call-with-current-continuation
   (lambda (return)
     (parameterize ((current-context 'expression)
		    (%expand-into-expression return))
       (expand-syntax! syntax)))))

(define (expand-expression* syntax*)
  (map-in-order expand-expression syntax*))

(define (expand-syntax! syntax)
  (define (thunk) (%expand-syntax! syntax))
  (if (eq? (current-context) 'top-level)
      (with-isolated-references thunk)
      (thunk)))

(define (%expand-syntax! syntax)
  (let loop ((form (syntax-datum syntax)))
    (cond
     ((simple-datum? form)
      (expand-into-expression (make-literal form syntax)))
     ((null? form)
      (compile-error "empty application in source" syntax))
     ((identifier? form)
      (cond
       ((sc-lookup-denotation! form)
	=> (lambda (denotation)
	     (cond
	      ((primitive? denotation)
	       (expand-into-expression (make-primitive-reference (primitive-symbol denotation)
								 syntax)))
	      ((procedure? denotation)
	       ;; TODO: We want such a note whenever an identifier is mentioned
	       (compile-note (format "identifier ‘~a’ was bound here" (unclose-form form))
			     (sc-lookup-syntax! form))
	       (compile-error (format "invalid use of syntax ‘~a’ as value"
				      (unclose-form form))
			      syntax))
	      (else
	       (expand-into-expression (make-reference denotation syntax))))))
       (else
	(compile-error (format "undefined variable ‘~a’" (unclose-form form)) syntax))))
     ((list? form)
      (cond
       ((lookup-transformer! (car form))
	=> (lambda (transform!)
	     (transform! syntax)))
       (else
	(let ((operator (expand-expression (car form))))
	  (expand-into-expression (make-procedure-call operator
						       (expand-expression* (cdr form))
						       syntax))))))
     ((syntactic-closure? form)
      (call-in-syntactic-closure form loop))
     (else
      (compile-error (format "invalid form ‘~a’" (list? form)) syntax)))))

(define (lookup-transformer! syntax)
  (define form (syntax-datum syntax))
  (and
   (identifier? form)
   (let ((denotation (sc-lookup-denotation! form)))
     (and (procedure? denotation) denotation))))

;;; Utility procedures

(define (make-dummy-formals)
  (make-formals (list (make-location #f)) #f #f))

(define (simple-datum? expression)
  (or (number? expression)
      (boolean? expression)
      (char? expression)
      (string? expression)
      (bytevector? expression)
      (vector? expression)))
