(defsystem "www.nolanwright.dev"
    :components ((:file "packages")
                 (:module "shh-html"
                   :depends-on ("packages")
                   :components ((:file "shh")))
                 (:module "shh-utils"
                   :depends-on ("shh-html")
                   :components ((:file "utils")
                                (:file "layouts/base")
                                (:file "layouts/sections"
                                 :depends-on ("layouts/base"))
                                (:file "components/anchor")))))
