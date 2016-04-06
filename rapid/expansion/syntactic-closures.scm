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

;;; Data types

(define-record-type <syntactic-closure>
  (make-syntactic-closure environment free-names form)
  syntactic-closure?
  (environment syntactic-closure-environment)
  (free-names syntactic-closure-free-names)
  (form syntactic-closure-form))

(define (close-syntax form environment)
  (make-syntactic-closure environment '() form))

(define (capture-syntactic-environment proc)
  (proc (get-syntactic-environment)))

(define (identifier? form)
  (or (symbol? form)
      (and (syntactic-closure? form)
	   (identifier? (syntactic-closure-form form)))))

(define (identifier->symbol form)
  (unclose-form form))

(define (unclose-form form)
  (if (syntactic-closure? form)
      (unclose-form (syntactic-closure-form form))
      form))

(define (make-synthetic-identifier identifier)
  (close-syntax identifier #f))

(define (identifier=? environment1 identifier1 environment2 identifier2)
  (define denotation1 (sc-lookup-denotation! identifier1 environment1))
  (define denotation2 (sc-lookup-denotation! identifier2 environment2))
  (cond
   ((and denotation1 denotation2) (eq? denotation1 denotation2))
   ((and (not denotation1) (not denotation2))
    (symbol=? (identifier->symbol identifier1)
	 (identifier->symbol identifier2)))
   (else #f)))

;; XXX: Is ‘free-names-map’ a good name?
(define current-free-names-map
  (make-parameter (make-map (make-eq-comparator)) box))
(define (get-free-names-map) (unbox (current-free-names-map)))
(define (set-free-names-map! map) (set-box! (current-free-names-map) map))
(define (lookup-syntactic-environment name)
  (or (map-ref/default (get-free-names-map) name #f)
      (get-syntactic-environment)))
(define (set-syntactic-environment! name syntactic-environment)
  (set-free-names-map! (map-set (get-free-names-map)
				name
				syntactic-environment)))

(define (call-in-syntactic-closure syntactic-closure proc)
  (let ((syntactic-environment (get-syntactic-environment)))
    (with-syntactic-environment
     (or (syntactic-closure-environment syntactic-closure)
	 (make-syntactic-closure))
     (lambda ()
       (parameterize ((current-free-names-map (get-free-names-map)))
	 (for-each
	  (lambda (name)
	    (set-syntactic-environment! name syntactic-environment))
	  (syntactic-closure-free-names syntactic-closure))
	 (proc (syntactic-closure-form syntactic-closure)))))))

(define sc-lookup-binding!
  (case-lambda
   ((identifier)
    (let loop ((identifier identifier))
      (or
       (with-syntactic-environment
	(lookup-syntactic-environment identifier)
	(lambda ()
	  (lookup-binding! identifier)))
       (and (syntactic-closure? identifier)
	    (call-in-syntactic-closure identifier loop)))))
   ((identifier environment)
    (with-syntactic-environment
     environment
     (lambda ()
       (sc-lookup-binding! identifier))))))

(define (sc-lookup-denotation! . arg*)
  (cond
   ((apply sc-lookup-binding! arg*) => binding-denotation)
   (else #f)))

(define (sc-lookup-syntax! . arg*)
  (cond
   ((apply sc-lookup-binding! arg*) => syntactic-binding-syntax)
   (else #f)))
