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

;; Fundamental binding construct

(define-syntax letrec*-values
  (syntax-rules ()
    ((letrec*-values ((formals init) ...) body1 body2 ...)
     (let ()
       (define-values formals init)
       ...
       (let ()
	 body1
	 body2
	 ...)))))

;; Procedural records

(define-record-type <rtd>
  (%make-rtd name fieldspecs make-record record? record-fields)
  rtd?
  (name rtd-name)
  (fieldspecs rtd-fieldspecs)
  (make-record rtd-make-record)
  (record? rtd-record?)
  (record-fields rtd-record-fields))

(define (find-index fieldspecs field)
  (let loop ((fieldspecs fieldspecs) (i 0))
    (if (eq? (car fieldspecs) field)
	i
	(loop (cdr fieldspecs) (+ i 1)))))

(define (make-rtd name . fieldspecs) ;; XXX Not the SRFI 99-convention
  (define-record-type <record>
    (make-record fields)
    record?
    (fields record-fields))
  (%make-rtd name fieldspecs make-record record? record-fields))

(define (rtd-constructor rtd . fieldspecs)
  (let*
      ((make-record
	(rtd-make-record rtd))
       (k
	(length (rtd-fieldspecs rtd)))
       (indexes
	(map
	 (lambda (fieldspec)
	   (find-index (rtd-fieldspecs rtd) fieldspec))
	 fieldspecs)))
    (lambda (cont flag marks . args)
      (let ((fields (make-vector k (if #f #f))))
	(for-each
	 (lambda (arg index)
	   (vector-set! fields index arg))
	 args indexes)
	(cont (make-record fields))))))

(define (rtd-predicate rtd)
  (let ((pred (rtd-record? rtd)))
    (lambda (cont flag marks obj)
      (cont (pred obj)))))

(define (rtd-accessor rtd field) 
  (let*
      ((record-fields (rtd-record-fields rtd))
       (index (find-index (rtd-fieldspecs rtd) field)))
    (lambda (cont flag marks record)
      (cont (vector-ref (record-fields record) index)))))

(define (rtd-mutator rtd field)
  (let*
      ((record-fields (rtd-record-fields rtd))
       (index (find-index (rtd-fieldspecs rtd) field)))
    (lambda (cont flag marks record value)
      (cont (vector-set! (record-fields record) index value)))))
