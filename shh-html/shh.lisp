(in-package :shh-html)

(defvar *singleton-tags*
  '(:area
    :base
    :br
    :col
    :command
    :embed
    :hr
    :img
    :input
    :keygen
    :link
    :meta
    :param
    :source
    :track
    :wbr))

(defun should-close-p (tag)
  (not (member tag *singleton-tags* :test #'string=)))

;; The tag exists in the car position. It may
;; either be a symbol or a keyword.
(defun tag-of (form)
  (car form))

;; Attributes follow the tag and exist as
;; name-value pairs, delimited by whitespace.
;; The names may be either keywords or symbols.
;; The values are usually strings but may also
;; be boolean values for the case of boolean
;; attributes. Attributes having falsy values
;; will not be rendered in the output.
(defun attributes-of (form)
  (let ((form (cdr form)))
    (labels ((attributes-of (form)
               (when (and form (symbolp (car form)))
                 (cons (car form)
                       (cons (cadr form)
                             (attributes-of (cddr form)))))))
      (attributes-of form))))

;; The body optionally follows the attributes,
;; if there are any, or the tag otherwise. The
;; body, if it is present, may be composed, in
;; any proportion, of strings and recursive forms.
(defun body-of (form)
  (let ((form (cdr form)))
    (labels ((drop-attributes (form)
               (if (and form (symbolp (car form)))
                   (drop-attributes (cddr form))
                   form)))
      (drop-attributes form))))

(defun tag->string (tag)
  (string-downcase tag))

(defun attributes->string (attributes)
  (unless (null attributes)
    (destructuring-bind (name value &rest attributes) attributes
      (let ((string (attributes->string attributes)))
        (if value
            (format nil
                    "~(~a~)~@[=\"~a\"~]~@[ ~a~]"
                    name
                    (unless (eq t value) value)
                    string)
            string)))))

(defun html-from (form &key with-doctype-p)
  (let* ((tag (tag-of form))
         (should-close (should-close-p tag))
         (tag-string (tag->string tag))
         (attributes-string (attributes->string (attributes-of form)))
         (body (when should-close (body-of form))))
    (flet ((child->string (child)
             (if (stringp child)
                 child
                 (html-from child))))
      (concatenate
       'string
       (when with-doctype-p "<!DOCTYPE html>")
       (if should-close
           (format nil
                   "<~a~@[ ~a~]>~{~a~}</~a>"
                   tag-string
                   attributes-string
                   (mapcar #'child->string (remove-if #'null body))
                   tag-string)
           (format nil "<~a~@[ ~a~]>" tag-string attributes-string))))))
