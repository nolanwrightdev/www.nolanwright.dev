(print-html
 (with-sections
  `((h1 class "leading-tight text-2xl mb-3" "About")
    (p class "mb-4"
     "My name is Nolan Wright. I am a software developer who loves learning new"
     "things and keeping up-to-date with the latest technological trends and"
     "advancements. If you would like to learn along with me, then please take"
     "a look at my " ,(anchor "blog" :href "/blog/") " where I periodically"
     "share and discuss the things I have been learning.")
    (p "If for any reason you would like to get in touch with me, do not"
       "hesitate to reach out either by "
       ,(anchor "email" :href "mailto:nolan@nolanwright.dev") " or "
       ,(anchor "XMPP message" :href "xmpp:nwdev@jabber.fr") ". "
       "For increased privacy, do too avail yourself of my "
       ,(anchor "PGP key"
                :href "/pgpkey.asc"
                :rel "pgpkey"
                :type "text/plain"
                :title "nolan@nolanwright.dev") "."))
   :title "Nolan Wright | About"
   :description "Information about me, including relevant contact information."))
