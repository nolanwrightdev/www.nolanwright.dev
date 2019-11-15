(in-package :shh-utils)


(defun with-sections (main &key title description)
  (with-base
   `(body class "font-mono m-0 text-gs5 flex flex-col h-screen"
     (header class "flex-initial mx-6"
      (nav class "max-w-xl m-auto"
       (ul class "flex justify-center border-b border-gs4"
        ,@(loop for (x y) in '(("/" "Home")
                               ("/about/" "About")
                               ("/blog/" "Blog"))
            collect `(li class "hover:bg-gs2"
                      (a class "block p-3" href ,x ,y))))))
     (main class "font-sans flex-auto p-6"
      (div class "max-w-xl mx-auto leading-relaxed"
       ,@main))
     (footer class "flex-initial mx-6"
      (div class "max-w-xl m-auto border-t border-gs4 flex justify-center"
       ,@(loop for (x y) in '(("mailto:nolan@nolanwright.dev" "mail")
                              ("xmpp:nwdev@jabber.fr" "message-circle")
                              ("https://github.com/nolanwrightdev" "github")
                              ("https://gitlab.com/nolanwrightdev" "gitlab"))
           collect `(a class "m-2 rounded-full hover:bg-gs2"
                       href ,x
                     (svg class "feather h-4 w-4 m-2"
                      ;; FIXME
                      ;; This element is problematic because the html minifier will
                      ;; remove it unless it is closed in the below fashion.
                      ,(format nil "<use xlink:href=\"/icons.svg#~a\" />" y)))))))
    :title title
    :description description))
