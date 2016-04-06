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

(define *transformer-environment*
  (environment '(scheme base)
	       '(rapid lists)))

(define-syntax eval-transformer
  (syntax-rules ()
    ((eval-transformer transformer identifier ...)
     ((eval `(lambda (identifier ...)
	       ,transformer)
	    *transformer-environment*)
      identifier ...))))

(define (make-er-macro-transformer transformer macro-environment)
  (lambda (syntax environment)
    (define renames (make-table (make-eq-comparator)))
    (define (rename identifier)
      (table-intern! renames
		     identifier
		     (lambda ()
		       (make-syntactic-closure macro-environment '() identifier))))
    (define (compare identifier1 identifier2)
      (identifier=? environment identifier1 environment identifier2))
    (transformer syntax rename compare)))

(define current-macro-environment (make-parameter #f))
(define current-ellipsis? (make-parameter #f))
(define current-literal? (make-parameter #f))
(define current-underscore? (make-parameter #f))
(define (ellipsis? identifier) ((current-ellipsis?) identifier))
(define (literal? identifier) ((current-literal?) identifier))
(define (underscore? identifier) ((current-underscore?) identifier))

(define (make-syntax-rules-transformer ellipsis?
				       literal?
				       underscore?
				       syntax-rule-syntax*
				       transformer-syntax
				       macro-environment)
  (parameterize ((current-macro-environment macro-environment)
		 (current-ellipsis? ellipsis?)
		 (current-literal? literal?)
		 (current-underscore? underscore?))
    (define syntax-rules-transformer
      (compile-syntax-rules-transformer syntax-rule-syntax*))
    (define pattern-syntax-vector
      (list->vector
       (map
	(lambda (syntax-rule-syntax)
	  (define pattern-syntax
	    (car (syntax-datum syntax-rule-syntax)))
	  (derive-syntax (cdr (syntax-datum pattern-syntax)) pattern-syntax))
	syntax-rule-syntax*)))
    (define template-syntax-vector
      (list->vector
       (map
	(lambda (syntax-rule-syntax)
	  (cadr (syntax-datum syntax-rule-syntax)))
	syntax-rule-syntax*)))
    (define er-macro-transformer
      ;; TODO: We could also make this internal to er-macro-transformer
      ;; or sc-transformer
      (eval-transformer syntax-rules-transformer
			compile-error compile-note transformer-syntax
			syntax-datum derive-syntax
			datum->syntax
			syntax?
			;; XXX
			log syntax->datum unclose-form
			;; END XXX
			template-syntax-vector
			pattern-syntax-vector))
    (make-er-macro-transformer er-macro-transformer macro-environment)))

(define (compile-syntax-rules-transformer syntax-rule-syntax*)
  (define clauses
    (let loop ((syntax-rule-syntax* syntax-rule-syntax*) (i 0))
      (if (null? syntax-rule-syntax*)
	  '()
	  (let ((syntax-rule (syntax-datum (car syntax-rule-syntax*))))
	    (unless (and (list? syntax-rule) (= (length syntax-rule) 2))
	      (compile-error "bad syntax-rule" (car syntax-rule-syntax*)))
	    (let*-values
		(((pattern-syntax)
		  (car syntax-rule))
		 ((template-syntax)
		  (cadr syntax-rule))
		 ((identifiers matcher)
		  (compile-pattern pattern-syntax i))
		 ((transcriber)
		  (compile-template template-syntax identifiers i))  ;; identifiers => variable-map
		 ((clause)
		  `(,matcher => ,transcriber)))
	      (cons clause (loop (cdr syntax-rule-syntax*) (+ i 1))))))))
  `(lambda (syntax rename compare)
     (define form (syntax-datum syntax))
     (cond
      ,@clauses
      (else
       (compile-note "the macro definition is here" transformer-syntax)
       (compile-error "no expansion for macro use" syntax)))))

(define (make-pattern-variable index depth syntax) (vector index depth syntax))
(define (pattern-variable-index variable) (vector-ref variable 0))
(define (pattern-variable-depth variable) (vector-ref variable 1))
(define (pattern-variable-syntax variable) (vector-ref variable 2))

(define (compile-pattern pattern-syntax rule-index)
  (define pattern (syntax-datum pattern-syntax))
  (unless (and (pair? pattern) (identifier? (syntax-datum (car pattern))))
    (compile-error "invalid pattern" pattern-syntax))
  (let-values (((identifiers matcher)
		(compile-list-pattern (derive-syntax (cdr pattern) pattern-syntax))))
    (values identifiers `(,matcher (derive-syntax (cdr form) syntax)
				   (vector-ref pattern-syntax-vector ,rule-index)))))

(define (make-pattern-variable-map) (make-map (make-eq-comparator)))

(define (compile-subpattern pattern-syntax)
  (define pattern (syntax-datum pattern-syntax))
  (cond
   ((identifier? pattern)
    (cond
     
     ;; Literal identifier
     ((literal? pattern)
      (values
       (make-pattern-variable-map)
       `(lambda (syntax pattern-syntax)
	  (and (compare (syntax-datum syntax) (rename (syntax-datum pattern-syntax))) #()))))

     ;; _ identifier
     ((underscore? pattern)
      (values
       (make-pattern-variable-map)
       `(lambda (syntax pattern-syntax)
	  #())))

     ;; Pattern variable
     (else
      (values
       (map-set (make-pattern-variable-map) pattern (make-pattern-variable 0 0 pattern-syntax))
       `(lambda (syntax pattern-syntax)
	  (vector (syntax-datum syntax)))))))
   ((vector? pattern)
    (let-values
	(((variables-map matcher)
	  (compile-list-pattern (derive-syntax (vector->list pattern) pattern-syntax))))
      (values
       variables-map
       `(lambda (syntax pattern-syntax)
	  (let ((form (syntax-datum syntax)))
	    (and
	     (vector? form)
	     (let ((list (vector->list form)))
	       (,matcher (derive-syntax list syntax)
			 (derive-syntax list pattern-syntax)))))))))
   ((circular-list? pattern)
    (compile-error "circular pattern in source" pattern-syntax))
   ((null? pattern)
    (values
     (make-pattern-variable-map)
     `(lambda (syntax pattern-syntax)
	(null? (syntax-datum syntax)))))
   ((pair? pattern)
    (compile-list-pattern pattern-syntax))
   ((constant? pattern)
    (values
     (make-pattern-variable-map)
     `(lambda (syntax pattern-syntax)
	(equal? (syntax-datum syntax) (syntax-datum pattern-syntax)))))
   (else
    (compile-error "invalid subpattern" pattern-syntax))))

;; Helper functions for ‘compile-list-pattern’
(define (make-pattern-element syntax index from-end? repeated?)
  (vector syntax index from-end? repeated?))
(define (pattern-element-syntax pattern-element)
  (vector-ref pattern-element 0))
(define (pattern-element-index pattern-element)
  (vector-ref pattern-element 1))
(define (pattern-element-from-end? pattern-element)
  (vector-ref pattern-element 2))
(define (pattern-element-repeated? pattern-element)
  (vector-ref pattern-element 3))
(define (pattern-element-set-repeated?! pattern-element repeated?)
  (vector-set! pattern-element 3 repeated?))
(define (analyze-pattern-list pattern-list)
  (define (return %pattern-element* repeated dotted?)
    (values (reverse (if repeated (cons repeated %pattern-element*) %pattern-element*))
	    (and repeated #t)
	    dotted?))
  (let loop ((pattern-list pattern-list)
	     (%pattern-element* '())
	     (repeated #f)
	     (i 0))
    (cond
     ((null? pattern-list) 
      (return %pattern-element* repeated #f))
     ((pair? pattern-list)
      (cond
       ((ellipsis? (syntax-datum (car pattern-list)))
	(when repeated
	  (compile-error "extraneous ellipsis" (car pattern-list)))
	(when (null? %pattern-element*)
	  (compile-error "ellipsis is not preceded by a pattern"
			 (car pattern-list)))
	(pattern-element-set-repeated?! (car %pattern-element*) #t)
	(loop (cdr pattern-list)
	      (cdr %pattern-element*)
	      (car %pattern-element*)
	      (+ i 1)))
       (else
	(loop (cdr pattern-list)
	      (cons (make-pattern-element (car pattern-list) i (and repeated #t) #f)
		    %pattern-element*)
	      repeated
	      (+ i 1)))))
     (else
      (when (ellipsis? (syntax-datum pattern-list))
	(compile-error "ellipsis not allowed as dotted tail" pattern-list))
      (return (cons (make-pattern-element pattern-list i (and repeated #t) #f)
		    %pattern-element*)
	      repeated
	      #t)))))
(define (make-submatcher variable-map matcher index)
  (vector variable-map matcher index))
(define (submatcher-variable-map matcher)
  (vector-ref matcher 0))
(define (submatcher-matcher matcher)
  (vector-ref matcher 1))
(define (submatcher-index matcher)
  (vector-ref matcher 2))
(define (compile-list-pattern pattern-syntax)
  (define variable-count 0)
  (define variable-map (make-pattern-variable-map))
  (define (insert-pattern-variable! identifier variable offset depth-increase)
    (cond
     ((map-ref/default variable-map identifier #f)
      => (lambda (old-variable)
	   (compile-note "first appearance was here"
			 (pattern-variable-syntax old-variable))
	   (compile-error "pattern variable has already appeared once"
			  (pattern-variable-syntax variable)))))
    (set! variable-map
	  (map-set
	   variable-map
	   identifier
	   (make-pattern-variable (+ offset (pattern-variable-index variable))
				  (+ (pattern-variable-depth variable)
				     depth-increase)
				  (pattern-variable-syntax variable))))
    (set! variable-count (+ variable-count 1)))
  (define (submatcher-compile! pattern-element)
    (define depth-increase (if (pattern-element-repeated? pattern-element) 1 0))
    (define-values (subvariable-map matcher)
      (compile-subpattern (pattern-element-syntax pattern-element)))
    (define submatcher (make-submatcher subvariable-map matcher variable-count))
    (define offset variable-count)
    (map-for-each
     (lambda (identifier variable)
       (insert-pattern-variable! identifier variable offset depth-increase))
     subvariable-map)
    submatcher)
  (define pattern (syntax-datum pattern-syntax))
  (define-values (pattern-element* repeated? dotted-pattern?)
    (analyze-pattern-list pattern))
  (define submatcher* (map-in-order submatcher-compile! pattern-element*))
  (define (gen-submatcher-call pattern-element submatcher)
    (define variable-map (submatcher-variable-map submatcher))
    (define matcher (submatcher-matcher submatcher))
    (define variable-offset (submatcher-index submatcher))
    (define element-index (pattern-element-index pattern-element))
    (define from-end? (pattern-element-from-end? pattern-element))
    (define element-repeated? (pattern-element-repeated? pattern-element))
    (define input-index
      (if from-end?
	  `(+ input-length ,(- element-index (length pattern-element*)
			       1
			       (if (and dotted-pattern? repeated?) -1 0)))
	  element-index))
    ;; TODO: Refactor out common code of the two cases below
    (if element-repeated?
	;; Repeated pattern
	`(let* ((input-end (+ input-length ,(- input-index
					       (length pattern-element*)
					       (if dotted-pattern? -1 0))))
		(submatch*
		 (unfold (lambda (index) (> index input-end))
			 (lambda (index)
			   (,matcher (vector-ref input index)
				     (vector-ref pattern-vector ,element-index)))
		       	 (lambda (index) (+ index 1))
			 ,input-index)))
	   (and
	    (every (lambda (submatch) submatch) submatch*)
	    (begin
	      ,@(map-fold
		 variable-map
		 (lambda (identifier variable setter*)
		   (cons
		    `(vector-set! match
				  ,(+ variable-offset (pattern-variable-index variable))
				  (map
				   (lambda (submatch)
				     (vector-ref submatch
						 ,(pattern-variable-index variable)))
				   submatch*))
		    setter*))
		 '())
	      #t)))
	;; Not repeated
	`(let ((submatch
		(,matcher (vector-ref input ,input-index)
			  (vector-ref pattern-vector ,element-index))))
	   (and
	    submatch
	    (begin
	      ,@(map-fold
		 variable-map
		 (lambda (identifier variable setter*)
		   (cons
		    `(vector-set! match
				  ,(+ variable-offset (pattern-variable-index variable)) 
				  (vector-ref                               
				   submatch                                 
				   ,(pattern-variable-index variable)))
		    setter*))
		 '())
	      #t)))))
  (define matcher
    `(lambda (syntax pattern-syntax)
       (define pattern (syntax-datum pattern-syntax))
       (define pattern-vector
	 (list->vector
	  ,(if dotted-pattern?
	       `(append (drop-right pattern 0) (list (take-right pattern 0)))
	       `pattern)))
       (define form (syntax-datum syntax))
       (when (circular-list? form)
	 (compile-error "circular list in source" syntax))
       (let* ((left (drop-right form 0))
	      (right (take-right form 0))
	      (input-length (length left)))
	 (and
	  ,@(if dotted-pattern? '() `((null? right)))
	  ,(if (or repeated? dotted-pattern?)
	       `(>= input-length ,(- (length pattern-element*)
				     (if repeated? 1 0)
				     (if dotted-pattern? 1 0)))
	       `(= input-length ,(length pattern-element*)))
	  (let ((input
		 (list->vector
		  ,(if dotted-pattern?
		       (if repeated?
			   `(append left (list (if (null? right)
						   (derive-syntax '() syntax)
						   right)))
			   ;; Not repeated
			   `(let*-values
				(((head tail)
				  (split-at left
					    ,(- (length pattern-element*) 1)))
				 ((tail) (append tail right)))
			      (append head (list (if (syntax? tail)
						     tail
						     (derive-syntax tail syntax))))))
		       `left)))
		(match (make-vector ,variable-count)))
	       (and
		,@(map gen-submatcher-call pattern-element* submatcher*)
		match))))))
  (values
   variable-map
   matcher))

;; XXX: Remove me after debugging
(define (log . args)
  (for-each
   (lambda (arg)
     (display arg (current-error-port))
     (display " " (current-error-port))
     #t)
   args)
  (newline (current-error-port))
  #t)

(define (compile-template template-syntax variables rule-index)
  (define-values (slots transcriber)
    (compile-subtemplate template-syntax variables 0))
  `(lambda (pattern-variables)
     (,transcriber
      (vector ,@(map
		 (lambda (slot)
		   `(vector-ref pattern-variables ,slot))
		 (vector->list slots)))
      (vector-ref template-syntax-vector ,rule-index))))

(define (compile-subtemplate template-syntax variables depth)
  (define template (syntax-datum template-syntax))
  (cond
   ((identifier? template)
    (cond
     ((ellipsis? template)
      (compile-error "extraneous ellipsis in template" template-syntax))
     ((map-ref/default variables template #f)
      => (lambda (variable)
	   (define variable-depth (pattern-variable-depth variable))
	   (if (zero? variable-depth)
	       (values
		#()
		`(lambda (match template-syntax)
		   (derive-syntax (vector-ref pattern-variables
					      ,(pattern-variable-index variable))
				  template-syntax
				  syntax)))
	       (cond
		((> variable-depth depth)
		 (compile-error "pattern variable followed by too few ellipses"
				template-syntax))
		((< variable-depth depth)
		 (compile-error "pattern variable followed by too many ellipses"
				template-syntax))
		(else	   
		 (values
		  (vector (pattern-variable-index variable))
		  `(lambda (match template-syntax)
		     (derive-syntax (vector-ref match 0)
				    template-syntax       
				    syntax))))))))
     (else
      (values
       #()
       `(lambda (match template-syntax)
	  (derive-syntax (rename (syntax-datum template-syntax))
			 template-syntax
			 syntax))))))
   ((circular-list? template)
    (compile-error "circular template in source" template-syntax))
   ((null? template)
    (values #() `(lambda (match template-syntax) (derive-syntax '() template-syntax syntax))))
   ((pair? template)
    (if (and (list? template) (= (length template) 2) (ellipsis? (syntax-datum (car template))))
	(parameterize ((current-ellipsis? (lambda (identifier) #f)))
	  (let-values (((slots transcriber)
		       (compile-subtemplate (cadr template) variables depth)))
	    (values
	     slots
	     `(lambda (match template-syntax)
		(,transcriber match (cadr (syntax-datum template-syntax)))))))
	(compile-list-template template-syntax variables depth)))
   ((vector? template)
    (let-values
	(((slots transcriber)
	  (compile-list-template (derive-syntax (vector->list template) template-syntax)
				 variables
				 depth)))
      (values
       slots
       `(lambda (match template-syntax)
	  (let ((output
		 (,transcriber match (derive-syntax (vector->list (syntax-datum template-syntax))
						    template-syntax))))
	    (derive-syntax (list->vector (syntax-datum output)) output))))))
   ((constant? template)
    (values
     #()
     `(lambda (match template-syntax)
	(derive-syntax ,template template-syntax syntax))))
   (else
    (compile-error "invalid subtemplate" template-syntax))))

(define (make-template-element template-syntax repeated? index)
  (vector template-syntax repeated? index))
(define (template-element-syntax template-element)
  (vector-ref template-element 0))
(define (template-element-repeated? template-element)
  (vector-ref template-element 1))
(define (template-element-index template-element)
  (vector-ref template-element 2))
(define (analyze-template-list list)
  (let loop ((list list) (%template-element* '()) (index 0))
    (cond
     ((null? list)
      (values (reverse %template-element*) '()))
     ((pair? list)
      (let ((template-syntax (car list)))
	(if (and (pair? (cdr list)) (ellipsis? (syntax-datum (cadr list))))
	    (loop (cddr list)
		  (cons (make-template-element template-syntax #t index)
			%template-element*)
		  (+ index 2))
	    (loop (cdr list)
		  (cons (make-template-element template-syntax #f index)
			%template-element*)
		  (+ index 1)))))
     (else
      (values (reverse %template-element*) `(,(make-template-element list #f index)))))))
(define (make-subtranscriber slots transcriber)
  (vector slots transcriber))
(define (subtranscriber-slots subtranscriber)
  (vector-ref subtranscriber 0))
(define (subtranscriber-transcriber subtranscriber)
  (vector-ref subtranscriber 1))
(define (make-slots+slot-table subtranscriber*)
  (define table (make-table (make-eqv-comparator)))
  (define index 0)
  (define %slot* '())
  (for-each
   (lambda (subtranscriber)
     (vector-for-each
      (lambda (slot)
	(table-intern! table slot (lambda ()
				    (set! %slot* (cons slot %slot*))				    
				    (set! index (+ index 1))
				    (- index 1))))
      (subtranscriber-slots subtranscriber)))
   subtranscriber*)
  (values (list->vector (reverse %slot*)) table))

(define (compile-list-template template-syntax variables-map depth)
  (define template (syntax-datum template-syntax))
  (define-values (template-element* template-element-rest*)
    (analyze-template-list template))
  (define (template-element-compile template-element)
    (define-values (slots transcriber)
      (compile-subtemplate (template-element-syntax template-element)
			   variables-map
			   (if (template-element-repeated? template-element)
			       (+ depth 1)
			       depth)))
    (when (and (template-element-repeated? template-element) (= (vector-length slots) 0))
      (compile-error "no pattern variable to repeat here"
		     (template-element-syntax template-element)))
    (make-subtranscriber slots transcriber))
  (define subtranscriber* (map-in-order template-element-compile template-element*))
  (define subtranscriber-rest* (map-in-order template-element-compile template-element-rest*))
  (define-values (slots slot-table)
    (make-slots+slot-table (append subtranscriber-rest* subtranscriber*)))
  (define (gen-subtranscriber-call template-element subtranscriber)
    (define slot* (vector->list (subtranscriber-slots subtranscriber)))
    (if
     (template-element-repeated? template-element)
     ;; Repeated template element
     ;; TODO: Some slots correspond to depth 0... can use indefinitly
     `(unfold
       (lambda (match**)
	 (every (lambda (match*) (null? match*)) match**))
       (lambda (match**)
	 (when (any (lambda (match*) (null? match*)) match**)
	   (compile-error "output cannot be build" template-syntax))
	 (,(subtranscriber-transcriber subtranscriber)
	  (list->vector (map car match**))
	  (vector-ref template-syntax-vector ,(template-element-index template-element))))
       (lambda (match**)
	 (map cdr match**))
       (list ,@(map (lambda (slot)
		      `(vector-ref match ,(table-ref slot-table slot)))
		    slot*)))
     ;; Regular template element
     `(list (,(subtranscriber-transcriber subtranscriber)
	     (vector ,@(map
			(lambda (slot)
			  `(vector-ref match ,(table-ref slot-table slot)))
			slot*))
	     (vector-ref template-syntax-vector
			 ,(template-element-index template-element))))))
  (define transcriber
    `(lambda (match template-syntax)
       (let* ((template (syntax-datum template-syntax))
	      (template-syntax-vector
	       (list->vector (append (drop-right template 0)
				     (let ((rest (take-right template 0)))
				       (if (null? rest) rest (list rest))))))
	      (output
	       (append
		,@(map-in-order gen-subtranscriber-call template-element* subtranscriber*)
		,(if (null? template-element-rest*)
		     ''()
		     `(let ((tail
			     (car
			      ,(gen-subtranscriber-call (car template-element-rest*)
							(car subtranscriber-rest*)))))
			(if (or (pair? (syntax-datum tail)) (null? (syntax-datum tail)))
			    (syntax-datum tail)
			    tail))))))
	 (derive-syntax output template-syntax syntax))))
  (values slots transcriber))

(define (constant? datum)
  (or (char? datum) (string? datum) (boolean? datum) (number? datum) (bytevector? datum)
      (vector? datum)))
