(print-html
 (with-base
  `(body class "font-mono m-0 text-gs5 flex flex-col h-screen"
    (header class "flex-initial bg-gs5 text-gs1"
     (nav class "max-w-xl mx-auto"
      (ul class "flex justify-center"
       ,@(loop for (x y) in '(("/" "Home")
                              ("/about/" "About")
                              ("/blog/" "Blog"))
            collect `(li
                      class "border-b border-gs5 hover:border-gs1 transition-all"
                      (a class "block p-3" href ,x ,y))))))
    (main class "flex-auto bg-gs5 text-gs1 flex justify-center items-center"
     (div class "max-w-xl"
      (h1 class "text-3xl text-center tracking-wider" "Nolan Wright")
      (div class "flex my-4"
       (hr class "flex-auto my-auto text-gs1")
       (p class "flex-initial mx-4 text-xl tracking-wider" "dev")
       (hr class "flex-auto my-auto text-gs1"))
      (p class "text-xl text-center" "My personal website and blog")))
    (footer class "flex-1"
     (div class "max-w-xl mx-auto flex justify-center"
      ,@(loop for (x y) in '(("mailto:nolan@nolanwright.dev" "mail")
                             ("xmpp:nwdev@jabber.fr" "message-circle")
                             ("https://github.com/nolanwrightdev" "github")
                             ("https://gitlab.com/nolanwrightdev" "gitlab"))
           collect
             `(a
               class "m-3 rounded-full hover:bg-gs5 hover:text-gs1 transition-all"
               href ,x
               (svg class "feather h-6 w-6 m-3"
                ;; FIXME
                ;; This element is problematic because the html minifier will
                ;; remove it unless it is closed in the below fashion.
                ,(format nil "<use xlink:href=\"/icons.svg#~a\" />" y)))))))
   :title "Nolan Wright | Home"
   :description "The home page of my personal website and blog."))
