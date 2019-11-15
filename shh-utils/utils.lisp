(in-package :shh-utils)


(defun print-html (form)
  (format t "~a~%" (html-from form :with-doctype-p t)))

(defun stringify-html (form &key with-doctype-p)
  (format nil "~a" (html-from form :with-doctype-p with-doctype-p)))

(defmacro body-from (&rest forms)
  (mapcar #'(lambda (form) (if (symbolp form) (string form) form)) forms))
