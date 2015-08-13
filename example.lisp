(defpackage #:silly-named-readtables (:use :cl #:named-readtables))
(in-package #:silly-named-readtables)

(defreadtable :versioned-function
  (:merge :standard)
  (:dispatch-macro-char #\# #\' (lambda (stream char arg)
                                  (declare (ignore char arg))
                                  (let ((fname (read stream)))
                                    (etypecase fname
                                      ((OR SYMBOL (CONS (EQL CL:SETF) *))
                                       `(function ,fname))
                                      (CONS `(vfunction ,@fname))))))
  (:case :upcase))


(read-from-string "#'some-function") ;; => #'FUNCTION
(read-from-string "#'(version bla)") ;; => #'(VERSION BLA)

(in-readtable :versioned-function)

(read-from-string "#'some-function") ;; => #'FUNCTION
(read-from-string "#'(version bla)") ;; => (VFUNCTION VERSION BLA)
