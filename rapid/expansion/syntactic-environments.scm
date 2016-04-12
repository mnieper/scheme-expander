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

(define-record-type <syntactic-environment>
  (%make-syntactic-environment bindings references)
  syntactic-environment?
  (bindings syntactic-environment-bindings
	    syntactic-environment-set-bindings!)
  (references syntactic-environment-references))

(define current-syntactic-environment (make-parameter #f))
(define current-scope current-syntactic-environment)
(define (get-syntactic-environment)
  (current-syntactic-environment))
(define (make-syntactic-environment)
  (%make-syntactic-environment (make-map (make-eq-comparator)) (box '())))

(define (get-bindings)
  (syntactic-environment-bindings (current-syntactic-environment)))
(define (set-bindings! bindings)
  (syntactic-environment-set-bindings! (current-syntactic-environment) bindings))

;; TODO: References should/could be a parameter of boxes
(define (current-references)
  (syntactic-environment-references (current-syntactic-environment)))
(define (get-references)
  (unbox (current-references)))
(define (set-references! references)
  (set-box! (current-references) references))

(define (with-syntactic-environment syntactic-environment thunk)
  (parameterize
      ((current-syntactic-environment syntactic-environment))
    (thunk)))

(define (with-scope thunk)
  (parameterize
      ((current-syntactic-environment
	(%make-syntactic-environment (get-bindings) (current-references))))
    (thunk)))

(define (with-isolated-references thunk)
  (define old-references #f)
  (define new-references '())
  (dynamic-wind
      (lambda ()
	(set! old-references (get-references))
	(set-references! new-references)
	(for-each
	 (lambda (binding)
	   (binding-increment-reference-count! binding))
	 (get-references)))
	thunk
	(lambda ()
	  (for-each
	   (lambda (binding)
	     (binding-decrement-reference-count! binding))
	   (get-references))
	  (set! new-references (get-references))
	  (set-references! old-references))))

;;; Denotations

;; This is used for identifiers denoting a primitive procedure.
;; At the moment, other denotations are either syntactic closures or
;; procedures. This is not extensible and the behaviour of eq?
;; on procedures may be implementation-dependent.

(define-record-type <denotation>
  (make-denotation type value syntax)
  denotation?
  (type denotation-type)
  (value denotation-value)
  (syntax denotation-syntax))

(define (make-primitive symbol syntax)
  (make-denotation 'primitive symbol syntax))
(define (primitive? denotation)
  (and (denotation? denotation)
       (eq? (denotation-type denotation) 'primitive)))
(define (primitive-symbol primitive)
  (denotation-value primitive))

;;; Syntactic bindings

(define-record-type <syntactic-binding>
  (%make-binding syntax denotation scope reference-count)
  syntactic-binding?
  (syntax syntactic-binding-syntax)
  (denotation binding-denotation)
  (scope binding-scope)
  (reference-count binding-reference-count binding-set-reference-count!))

(define (make-binding syntax denotation)
  (%make-binding syntax denotation (current-scope) 0))

(define (binding-increment-reference-count! binding)
  (binding-set-reference-count! binding (+ (binding-reference-count binding) 1)))

(define (binding-decrement-reference-count! binding)
  (binding-set-reference-count! binding (- (binding-reference-count binding) 1)))

(define (binding-identifier binding)
  (syntax-datum (syntactic-binding-syntax binding)))

(define (binding-reference! binding)
  (set-references! (cons binding (get-references)))
  (binding-increment-reference-count! binding))

(define (binding-referenced? binding)
  (and (eq? (binding-scope binding) (current-scope))
       (> (binding-reference-count binding) 0)))

(define (%lookup-binding identifier)
  (map-ref/default (get-bindings) identifier #f))

(define (identifier-referenced? identifier)
  (let ((binding (%lookup-binding identifier)))
    (and binding (binding-referenced? binding))))

(define (lookup-binding! identifier)
  (cond
   ((%lookup-binding identifier)
    => (lambda (binding)
	 (binding-reference! binding)
	 (let ((new-binding (make-binding (syntactic-binding-syntax binding)
					  (binding-denotation binding))))
	   (binding-reference! new-binding)
	   new-binding)))
   (else #f)))

(define (insert-binding! identifier-syntax denotation)
  (define identifier (syntax-datum identifier-syntax))
  (when (identifier-referenced? identifier)
    ;; TODO include note: initial binding was here
    (compile-error "identifier has already been referenced" identifier-syntax))
  (let ((binding (make-binding identifier-syntax denotation)))
    (set-bindings! (map-set (get-bindings) identifier binding))
    (binding-reference! binding)))

(define (lookup-syntax! identifier)
  (cond
   ((lookup-binding! identifier) => syntactic-binding-syntax)
   (else #f)))

(define (lookup-denotation! identifier)
  (cond
   ((lookup-binding! identifier) => binding-denotation)
   (else #f)))

(define (%insert-binding! identifier-syntax denotation)
  (define identifier (syntax-datum identifier-syntax))
  (cond
   ((%lookup-binding identifier)
    => (lambda (binding)
	 (unless (eq? (binding-denotation binding) denotation)
	   (compile-note "initial binding was here"
			 (syntactic-binding-syntax binding))
	   (compile-error (format "identifier ‘~a’ rebound with different denotation"
				  (syntax-datum identifier-syntax))
			  identifier-syntax))))
   (else
    (insert-binding! identifier-syntax denotation))))

(define insert-binding-from!
  (case-lambda
   ((identifier-syntax syntactic-environment)
    (insert-binding-from! identifier-syntax syntactic-environment identifier-syntax))
   ((identifier-syntax syntactic-environment new-identifier-syntax)
    (define identifier (syntax-datum identifier-syntax))
    (cond
     ((with-syntactic-environment
       syntactic-environment
       (lambda () (%lookup-binding identifier)))
      => (lambda (binding)
	   (%insert-binding! new-identifier-syntax (binding-denotation binding))))
     (else
      (compile-error (format "unbound identifier ‘~a’" identifier) identifier-syntax))))))
   
(define (delete-binding! identifier-syntax)
  (define identifier (syntax-datum identifier-syntax))
  (unless (%lookup-binding identifier)
	  (compile-error (format "unbound identifier ‘~a’" identifier) identifier-syntax))
  (set-bindings! (map-delete (get-bindings) identifier)))

(define derive-syntactic-environment
  (case-lambda
   ((syntactic-environment syntax)
    (derive-syntactic-environment syntactic-environment syntax (lambda (identifier) identifier)))
   ((syntactic-environment syntax rename)
    (with-syntactic-environment
     (make-syntactic-environment)
     (lambda ()
       (map-for-each
	(lambda (identifier binding)
	  (%insert-binding! (derive-syntax (rename (binding-identifier binding)) syntax)
			    (binding-denotation binding)))
	(with-syntactic-environment syntactic-environment (lambda () (get-bindings))))
       (get-syntactic-environment))))))

(define (insert-bindings-from! syntactic-environment)
  (map-for-each
   (lambda (identifier binding)
     (%insert-binding! (derive-syntax (binding-identifier binding)
				      (syntactic-binding-syntax binding))
		       (binding-denotation binding)))
   (with-syntactic-environment syntactic-environment (lambda () (get-bindings)))))

(define-syntax syntactic-environment
  (syntax-rules ()
    ((syntactic-environment (identifier denotation) ...)
     (with-syntactic-environment
      (make-syntactic-environment)
      (lambda ()
	(insert-binding! (datum->syntax 'identifier) denotation)
	...
	(get-syntactic-environment))))))
