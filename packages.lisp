(defpackage :shh-html
  (:use :cl)
  (:export :html-from :*singleton-tags*))

(defpackage :shh-utils
  (:use :cl :shh-html)
  (:export :print-html :with-base :with-sections :anchor))
