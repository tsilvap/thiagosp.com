(defpackage com.thiagosp
  (:use :cl)
  (:export #:runserver))
(in-package :com.thiagosp)

(defun runserver (&key (port 4242))
  (let ((server (make-instance 'easy-routes:easy-routes-acceptor :port port)))
    (hunchentoot:start server)
    (loop do (sleep 1000))))

(push (hunchentoot:create-folder-dispatcher-and-handler
       "/static/css/" (asdf:system-relative-pathname "thiagosp" "dist/css/"))
      hunchentoot:*dispatch-table*)
(push (hunchentoot:create-folder-dispatcher-and-handler
       "/static/webfonts/" (asdf:system-relative-pathname "thiagosp" "dist/webfonts/"))
      hunchentoot:*dispatch-table*)
(push (hunchentoot:create-folder-dispatcher-and-handler
       "/static/img/" (asdf:system-relative-pathname "thiagosp" "dist/img/"))
      hunchentoot:*dispatch-table*)

(defparameter *email* "thiagodasilva@protonmail.com")

(defmacro with-page ((&key title) &body body)
  `(spinneret:with-html-string
     (:doctype)
     (:html
      (:head
       (:link :href "static/css/main.css" :rel "stylesheet")
       (:title ,title))
      (:body
       ,@body
       (:footer.footer.footer-center.bg-base-300.text-base-content.p-4.mt-16
        (:aside
         (let ((spinneret:*html-style* :tree)
               (spinneret:*suppress-inserted-spaces* t)
               (*print-pretty* nil))
           (:p "The source code for this website is on "
               (:a.link :href "https://github.com/tsilvap/thiagosp.com"
                        "GitHub")
               "."))))))))

(defun home-page ()
  (with-page (:title "Thiago S. Pinto")
    (:nav.grid.grid-flow-col.gap-4.max-w-lg.mx-auto.text-center.my-8
     (:a.link.link-hover :href "/about" "about")
     (:a.link.link-hover :href "/contact" "contact")
     (:a.link.link-hover :href "https://github.com/tsilvap" "github")
     (:a.link.link-hover :href "https://www.linkedin.com/in/thidasilva/" "linkedin")
     (:a.link.link-hover :href "/cv" "cv"))

    (:div.hero.py-8
     (:div.hero-content
      (:div
       (:h1.text-5xl.font-bold.mb-6 "Hi, I'm Thiago S. Pinto")
       (:div.prose.mb-6
        (:p "I'm a software engineer with extensive professional experience, specializing in backend development.")
        (:p "My specialties are: Go, Python, SQL, Kafka, Bash scripting, Unix and Linux, CI/CD, AWS, test-driven development, and more."))
       (:div.alert.max-w-xl.bg-neutral.border-neutral
        :role "alert"
        (:div
         (:h3.font-bold.mb-2 "Open to work!")
         (:p
          "I'm currently looking for a new job. If you're interested in what you see, send me an "
          (:a
           :href (format nil "mailto:~a" *email*) "email")
          " and we can work out the details."))))))

    (:div.max-w-2xl.mx-auto
     (:h2.text-2xl.font-bold.mb-6 "Micro projects")
     (:ul.grid.gap-6
      (:li
       (:a :href "/in-theaters"
           (:div.card.bg-base-200.shadow-xl
            :class "hover:bg-base-300"
            (:div.card-body
             (:h3.card-title "In Theaters")
             (:p "Shows the list of movies currently showing in various Brazilian theaters, along with Rotten Tomatoes, Metacritic, IMDb scores, and more.")))))
      (:li
       (:a :href "/hermes"
           (:div.card.bg-base-200.shadow-xl
            :class "hover:bg-base-300"
            (:div.card-body
             (:h3.card-title "Hermes")
             (:p "Hermes is a text and file hosting website that just works.")))))))))

(easy-routes:defroute root ("/" :method :get) ()
  ;; XXX: Write HTML file to disk, so that Tailwind CSS can generate
  ;; the CSS classes correctly. This seems super hacky, I should find
  ;; out what's a better/correct way to solve this problem later.
  (with-open-file (f (asdf:system-relative-pathname "thiagosp"
                                                    "dist/index.html")
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (write-sequence (home-page) f))

  (home-page))
