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

(define (read-library library-name-syntax)
  (define library-definition-syntax
    (read-library-definition library-name-syntax))
  (define form (syntax-datum library-definition-syntax))
  (let loop ((declarations (cddr form))
	     (import-sets '())
	     (export-specs '())
	     (body '()))
    (if (null? declarations)
	(values (reverse import-sets) (reverse export-specs) (reverse body))
	(let* ((declaration (car declarations))
	       (declarations (cdr declarations))
	       (form (syntax-datum declaration)))
	  (unless (and (not (null? form)) (list? form))
	    (compile-error "bad library declaration" declaration))
	  (case (syntax-datum (car form))
	    ((export)
	     (loop declarations
		   import-sets
		   (append (reverse (cdr form)) export-specs)
		   body))
	    ((import)
	     (loop declarations
		   (append (reverse (cdr form)) import-sets)
		   export-specs
		   body))
	    ((begin)
	     (loop declarations
		   import-sets
		   export-specs
		   (append (reverse (cdr form)) body)))
	    ((include)
	     (loop declarations
		   import-sets
		   export-specs
		   (generator-fold cons body (read-file* (cdr form) #f))))
	    ((include-ci)
	     (loop declarations
		   import-sets
		   export-specs
		   (generator-fold cons body (read-file* (cdr form) #t))))
	    ((include-library-declarations)
	     (loop (generator-fold cons declarations (read-file* (cdr form) #f))
		   import-sets
		   export-specs
		   body))			   
	    ((cond-expand)
	     (let loop-clauses ((clauses (cdr form)))
	       (if (null? clauses)
		   (loop declarations import-sets export-specs body)
		   (let ((clause (car clauses)))
		     (define form (syntax-datum clause))
		     (unless (and (list? form) (>= (length form) 1))
		       (compile-error "bad cond-expand clause" clause))
		     (cond
		      ((eq? (car form) 'else)
		       (unless (null? (cdr clauses))
			 (compile-error "else clause not last" declaration))
		       (loop (append (cdr form) declarations)
			     import-sets
			     export-specs
			     body))
		      ((feature? (car form))
		       (loop (append (cdr form) declarations)
			     import-sets
			     export-specs
			     body))
		      (else
		       (loop-clauses (cdr clauses))))))))
	    (else
	     (compile-error "invalid library declaration" declaration)))))))

(define (feature? feature-requirement-syntax)
  (define form (syntax-datum feature-requirement-syntax))
  (cond
   ((symbol? form)
    (assq form rapid-features))
   ((and (not (null? form)) (list? form))
    (case (syntax-datum (car form))
      ((library)
       (unless (= (length form) 2)
	 (compile-error "bad library feature requirement" feature-requirement-syntax))
       (guard (condition
	       ((compile-error-object? condition) #f))
	      (read-library-definition (cadr form))  ;; TODO: should store the definition
	      #t))
      ((and)
       (let loop ((feature-requirement-syntax* (cdr form)))
	 (if (null? feature-requirement-syntax*)
	     #t
	     (let* ((r1 (feature? (car feature-requirement-syntax*)))
		    (r2 (loop (cdr feature-requirement-syntax*))))
	       (and r1 r2)))))
      ((or)
       (let loop ((feature-requirement-syntax* (cdr form)))
	 (if (null? feature-requirement-syntax*)
	     #f
	     (let* ((r1 (feature? (car feature-requirement-syntax*)))
		    (r2 (loop (cdr feature-requirement-syntax*))))
	       (or r1 r2)))))
      ((not)
       (unless (= (length form) 2)
	 (compile-error "bad not feature requirement" feature-requirement-syntax))
       (not (feature? (cadr form))))
      (else
       (compile-error "invalid feature requirement" feature-requirement-syntax))))
   (else
    (compile-error "bad feature requirement" feature-requirement-syntax))))

(define (read-library-definition library-name-syntax)
  (define library-name (syntax->datum library-name-syntax))
  (define (locate-library)
    ;; TODO: error handling
    ;; TODO: search several directories
    (let loop ((filename "share") (library-name library-name))
      (if (null? library-name)
	  (string-append filename ".sld")
	  (loop (path-join filename (symbol->string (car library-name)))
		(cdr library-name)))))
  (define source (locate-library))
  (define read-syntax (read-file source #f library-name-syntax))
  (let loop ()
    (define syntax (read-syntax))
    (when (eof-object? syntax)
      (compile-error (format "library definition of ‘~a’ not found in file ‘~a’"
			     library-name-syntax 
			     source)
		     library-name-syntax))
    (let ((form (syntax-datum syntax)))
      (cond
       ((and (list? form)
	     (>= (length form) 2)
	     (eq? (syntax-datum (car form)) 'define-library))
	(assert-library-name! (cadr form))
	(if (equal? (syntax->datum (cadr form)) library-name)
	    syntax
	    (loop)))
       (else
	(loop))))))
  
(define (read-file* string-syntax* ci?)
  (apply gappend (map-in-order
		  (lambda (string-syntax)
		    (%read-file string-syntax ci?))
		  string-syntax*)))

(define (%read-file string-syntax ci?)
  (define filename (syntax-datum string-syntax))
  (unless (string? filename)
    (compile-error "bad string literal" string-syntax))
  (read-file (locate-file filename string-syntax) ci? string-syntax))

(define (assert-library-name! library-name-syntax)
  (define form (syntax-datum library-name-syntax))
  (define (library-name?)
    (and (list? form)
	 (let loop ((form form))
	   (or (null? form)
	       (let ((datum (syntax-datum (car form))))
		 (and (or (and (exact-integer? datum) (>= datum 0))
			  (symbol? datum))
		      (loop (cdr form))))))))
  (unless (library-name?)
    (compile-error "bad library name" library-name-syntax)))
