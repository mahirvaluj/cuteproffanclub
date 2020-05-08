(defpackage :cute-prof-fanpage
  (:use :common-lisp)
  (:import-from :hunchentoot
                :create-prefix-dispatcher :easy-acceptor :start :*dispatch-table*)
  (:import-from :cl-who :with-html-output)
  (:export :run-server))
(in-package :cute-prof-fanpage)

(setq *dispatch-table* nil)

(defparameter *prof-table* nil)
(defvar *server* nil)

(defun run-server (port)
  (setf *server* (start (make-instance 'easy-acceptor :port port :document-root #p"./www/"))))

(defmacro standard-page (title &body body)
  `(with-html-output (*standard-output* nil :indent t)
     (:html
      (:head
       (:title ,title)
       (:meta :http-equiv "Content-Type" 
              :content    "text/html;charset=utf-8"))
      (:link :type "text/css" 
             :rel "stylesheet"
             :href "/cpfp.css")
      (:body 
       (:font :size 7 (:img :src "/logo.png" :alt "Cute Prof Fanpage" :height 100) (:b (:a :href "/home.html" "Cute Prof Fanpage")))
       (:div :class "row"
             (:div :class "column left"
                   (:ul
                    ,@(let ((acc))
                        (dolist (p *prof-table*)
                          (push `(:li (:font :size 5 (:a :href ,(format nil "/~(~a~).html" (second p)) ,(first p)))) acc))
                        acc)))
             (:div :class "column right"
                   ,@body))))))

(defmacro add-url-fn (name title &body body)
  `(progn
     (defun ,name ()
       (standard-page ,title ,@body))
     (push (create-prefix-dispatcher ,(format nil "/~(~a~).html" name) #',name) *dispatch-table*)))

(defmacro add-prof (full-name name &body body)
  `(progn 
     (push ',`(,full-name ,name) *prof-table*)
     (add-url-fn ,name ,full-name ,@body)))

(add-url-fn home "Homepage"
  (:h3 "Your one stop shop for your fix of cute profs")
  (:p "If you're like me, a handful of hour-long lectures a week don't
  provide enough cute prof material, and so I've made this fan page
  for those like myself who are going through withdrawals from their
  cute profs in these trying, distance-learning times."))

(add-prof "Hadi Salmasian" hadi
  (:img :src "/hadi.jpg" :alt "Professor Hadi Salmasian" :width "50%")
  (:h3 "Ah, where to start with Professor Salmasian? Truly, one of the
  greats. Not only is he passionate during lectures, caring to
  students, a fair grader, and a fantastic explainer, he is also
  extremely cute, as can be seen here. One of the highlights of my
  time here at the best university in the world.")
  (:br)
  (:b (:a :href "https://mysite.science.uottawa.ca/hsalmasi/" "His website can be found here."))
  (:br)
  (:br)
  (:img :src "/hadi2.jpg" :alt "Professor Hadi Salmasian" :width "50%"))

(add-prof "Steve Desjardins" steve
  (:img :src "/steve.png" :alt "Professor Steve Desjardins" :width "50%")
  (:h3 "I'm going to be straight with you, it's hard to find a
  professor more caring and kind than Steve Desjardins. Not only is he
  extremely accessible outside of class hours, he will take the time
  to get to know you, and he will spend the effort in order to help
  you through any material you do not understand. He has high
  expectations of students, and will help said students meet
  them. Below, you can see a compilation of Steve Desjardins being one
  of the best profs in the business.")
  (:br)
  (:b (:a :href "https://mysite.science.uottawa.ca/sdesjar2/" "His website can be found here."))
  (:br)
  (:br)
  (:img :src "/steve_medley.png" :width "75%"))

(add-prof "Elizabeth Maltais" maltais
  (:img :src "/maltais.png" :alt "Professor Elizabeth Maltais" :width "50%")
  (:h3 "Some of my fondest memories spent in a lecture hall were spent
  in front of this woman. Not only is she a passionate lecturer, she
  also makes sure to present her notes in a way so clear that those
  outside of our section would eat their hearts out. As seen here:")
  (:br)
  (:img :src "/maltais_notes.png" :alt "Professor Elizabeth Maltais Notes" :width "50%")
  (:br)
  (:br)
  (:b (:a :href "https://mysite.science.uottawa.ca/sdesjar2/"
          "I couldn't find a website of hers, but here is her masters
  thesis. You can see how fantastic an explainer and lecturer she is here."))
  (:br)
  (:br)
  (:b (:a :href "https://science.uottawa.ca/mathstat/en/news/elizabeth-maltais-distinguished-peter-rodney-book-prize"
          "In addition, she was distinguished with the Peter Rodney
          Book Prize for her lecturing, and I, for one, think she completely
          deserves it.")))

(add-prof "Gilles Lamothe" gilles
  (:img :src "/gilles.png" :alt "Professor Gilles Lamothe" :width "50%")
  (:br)
  (:h3 "Professor Gilles Lamothe is an interesting
  character. Originally, when I had seen him lecture for the first
  time, I will be honest, I was hardly smitten. His lectures were
  clear, and he presented the topic at hand quite well, but he did not
  have any pizzazz. How wrong could I have been? It all changed one
  day when I went to his office hours. He welcomed me in, and after
  helping me solve the class material, helped me understand advanced
  material only tangentially related to the class. It is clear that he
  enjoys teaching just as much as any other, and he is a fantastic
  explainer.")
  (:br)
  (:b (:a :href "http://aix1.uottawa.ca/~glamothe/" "His website can be found here."))
  (:br)
  (:br)
  (:img :src "/gilles2.png" :alt "Professor Gilles Lamothe" :width "50%")
  (:br))


