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

(define (program-read filename)
  (generator->list (read-file filename #f #f)))

(define (program-expand syntax*)
  (define-values (body environment)
    (call-with-current-continuation
     (lambda (return)
       (let loop ((syntax* syntax*) (import-sets '()))
	 (or
	  (and-let*
	      (((not (null? syntax*)))
	       (form (syntax-datum (car syntax*)))
	       ((list? form))
	       ((>= (length form) 1))
	       ((eq? (syntax-datum (car form)) 'import)))
	    (return (loop (cdr syntax*) (append (cdr form) import-sets))))
	  (return syntax* (expand-import-sets (reverse import-sets))))))))
  (with-syntactic-environment
   (environment-syntactic-environment environment)
   (lambda ()
     (make-letrec*-expression
      (append (environment-bindings environment)
	      (with-scope
	       (lambda ()
		 (expand-top-level body))))
      (list (make-literal #t #f))
      #f))))
