(in-package :shh-utils)

(defun anchor (body &key href rel type title target)
  `(a class "text-black underline"
      href ,href
      rel ,rel
      type ,type
      title ,title
      target ,target
      ,body))
