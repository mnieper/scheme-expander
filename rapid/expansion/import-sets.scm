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

;;; FIXME
;; Contexts should not be nested due to import specs

(define current-location-bindings (make-parameter #f box))
(define (%get-location-bindings) (unbox (current-location-bindings)))
(define (get-location-bindings) (reverse (%get-location-bindings)))
(define (add-location-bindings! bindings)
  (set-box! (current-location-bindings) (append (reverse bindings) (%get-location-bindings))))
(define current-library-table (make-parameter #f))
(define (with-initial-environment library-name initial-environment thunk)
  (parameterize
      ((current-location-bindings (reverse (environment-bindings initial-environment)))
       (current-library-table
	(let ((table (make-table (make-equal-comparator))))
	  (table-set! table
		      library-name
		      (environment-syntactic-environment initial-environment))
	  table)))
    (thunk)))
(define (lookup-syntactic-environment library-name)
  (table-ref/default (current-library-table) library-name #f))
(define (insert-library! library-name)
  (table-set! (current-library-table) library-name #f))
(define (library-loading? library-name)
  (not (table-ref/default (current-library-table) library-name #t)))
(define (update-library! library-name syntactic-environment)
  (table-set! (current-library-table) library-name syntactic-environment))

(define (expand-import-sets import-sets)
  (with-initial-environment
   '(rapid primitive)
   primitive-environment
   (lambda ()
     (define syntactic-environment (import*! import-sets))
     (make-environment (get-location-bindings) syntactic-environment))))

(define (import*! import-sets)
  (with-syntactic-environment
   (make-syntactic-environment)
   (lambda ()
     (for-each
      (lambda (import-set)
	(insert-bindings-from! (import! import-set)))
      import-sets)
     (get-syntactic-environment))))

(define (import! import-set)
  (define form (syntax-datum import-set))
  (unless (list? form)
    (compile-error "bad import set" import-set))
  (cond
   ;; Import set modifier
   ((and (> (length form) 1) (list? (syntax-datum (cadr form))))
    (let ((syntactic-environment (import! (cadr form))))
      (case (syntax-datum (car form))
	;; Only import set
	((only)
	 (with-syntactic-environment
	  (make-syntactic-environment)
	  (lambda ()
	    (for-each
	     (lambda (identifier-syntax)
	       (assert-identifier! identifier-syntax)
	       (insert-binding-from! identifier-syntax syntactic-environment))
	     (cddr form)))
	  (get-syntactic-environment)))
	;; Except import set
	((except)
	 (with-syntactic-environment
	  (derive-syntactic-environment syntactic-environment import-set)
	  (lambda ()
	    (for-each
	     (lambda (identifier-syntax)
	       (assert-identifier! identifier-syntax)
	       (delete-binding! identifier-syntax))
	     (cddr form))
	    (get-syntactic-environment))))
	;; Prefix import set
	((prefix)
	 (unless (and (= (length form) 3)
			(symbol? (syntax-datum (caddr form))))
	   (compile-error "bad import set" import-set))
	 (derive-syntactic-environment syntactic-environment
				       import-set
				       (lambda (identifier)
					 (symbol-append
					  (syntax-datum (caddr form))
					  identifier))))
	;; Rename import set
	((rename)
	 (let ((table (make-table (make-eq-comparator))))
	   (for-each
	    (lambda (rename)
	      (define form (syntax-datum rename))
	      (unless (and (list? form)
			   (= (length form) 2)
			   (symbol? (syntax-datum (car form)))
			   (symbol? (syntax-datum (cadr form))))
		(compile-error "bad rename" rename))
	      (table-set! table (syntax-datum (car form)) (syntax-datum (cadr form))))
	    (cddr form))
	   (derive-syntactic-environment syntactic-environment
					 import-set
					 (lambda (identifier)
					   (or (table-ref/default table identifier #f)
					       identifier)))))
	(else (compile-error "invalid import set" import-set)))))
   ;; Simple import
   (else
    (assert-library-name! import-set)
    (derive-syntactic-environment (import-library! import-set) import-set))))

;; Returns the syntactic environment of a library
;; Adds entries to library table if library cannot be found
;; Adds bindings
(define (import-library! library-name-syntax)
  (define library-name (syntax->datum library-name-syntax))
  (cond
   ((lookup-syntactic-environment library-name))
   (else
    (when (library-loading? library-name)
      (compile-error "library references itself while loading" library-name-syntax))
    (insert-library! library-name)
    (let-values (((import-sets export-specs body)
		  (read-library library-name-syntax)))
      (with-syntactic-environment
       (import*! import-sets)
       (lambda ()
	 (with-scope
	  (lambda ()
	    (add-location-bindings! (expand-top-level body))
	    (let ((syntactic-environment (get-syntactic-environment)))
	      (with-syntactic-environment
	       (make-syntactic-environment)
	       (lambda ()
		 (for-each
		  (lambda (export-spec)
		    (define form (syntax-datum export-spec))
		    (cond
		     ((list? form)
		      (unless (= (length form) 2)
			(compile-error "bad export spec" export-spec))
		      (assert-identifier! (car form))
		      (assert-identifier! (cadr form))
		      (insert-binding-from! (car form) syntactic-environment (cadr form)))
		     (else
		      (assert-identifier! export-spec)
		      (insert-binding-from! export-spec syntactic-environment))))
		  export-specs)
		 (let ((syntactic-environment (get-syntactic-environment)))
		   (update-library! library-name syntactic-environment)
		   syntactic-environment))))))))))))
   
(define (assert-identifier! identifier-syntax)
  (unless (symbol? (syntax-datum identifier-syntax))
    (compile-error (format "bad identifier ‘~a’" (syntax-datum identifier-syntax))
		   identifier-syntax)))

(define (symbol-append symbol1 symbol2)
  (string->symbol
   (string-append (symbol->string symbol1) (symbol->string symbol2))))
