(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :named-readtables))

(defpackage #:silly-named-readtables (:use :cl #:named-readtables))
(in-package #:silly-named-readtables)

(defreadtable :silly-table
  (:merge :standard)
  (:dispatch-macro-char #\# #\' (lambda (stream char arg)
                                  (declare (ignore char arg))
                                  (let ((fname (read stream)))
                                    (etypecase fname
                                      ((or symbol (cons (eql cl:setf) *))
                                       `(function ,fname))
                                      (cons `(silly-function ,@fname))))))
  (:case :upcase))

(read-from-string "#'foo")           ;; => #'FOO
(read-from-string "#'(foo bar)")     ;; => #'(FOO BLA)

(in-readtable :silly-table)

(read-from-string "#'some-function") ;; => #'SOME-FUNCTION
(read-from-string "#'(foo bar)")     ;; => (SILLY-FUNCTION FOO BAR)
