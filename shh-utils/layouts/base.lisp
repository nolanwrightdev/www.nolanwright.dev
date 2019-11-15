(in-package :shh-utils)

(defun with-base (body &key title description)
  `(html lang "en"
    (head
     (meta charset "utf-8")
     (meta name "viewport" content "width=device-width, initial-scale=1")
     (meta name "author" content "Nolan Wright <nolan@nolanwright.dev>")
     (meta name "description" content ,description)
     (title ,title)
     (link rel "stylesheet" href "/style.css"))
    ,body))
