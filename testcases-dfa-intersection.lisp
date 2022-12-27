;; Make DFAs
(defparameter *dfa-0*
  (make-fa '((q0 0 q0)
             (q1 0 q0)
             (q1 1 q0)
	     )
           'q0
           '(q1)))

(defparameter *dfa-l*
  (make-fa '((a 0 a)
             (b 0 a)
             (b 1 a)
             (a 1 b)
	     )
           'a
           '(b)))

(defparameter *dfa-m*
  (make-fa '((c 0 d)
             (c 1 c)
             (d 0 d)
             (d 1 c)
	     )
           'c
           '(c)))

(fa-pdf (dfa-minimize *dfa-l*) "dfa-l-min.pdf")


(dfa-equivalent (dfa-minimize *dfa-l*) *dfa-l*)

(fa-pdf *dfa-0* "dfa-0.pdf")
(fa-pdf *dfa-l* "dfa-l.pdf")
(fa-pdf *dfa-m* "dfa-m.pdf")

;; need to return an FA that is the intersection of DFA-0 and DFA-1
(fa-pdf (dfa-intersection *dfa-l* *dfa-m*) "dfa-int.pdf")

(fa-pdf (rename-fa-states (dfa-intersection *dfa-l* *dfa-m*)) "dfa-int-renamed.pdf")


(fa-pdf (dfa-minimize (dfa-intersection *dfa-l* *dfa-m*)) "dfa-int-min.pdf")



(assert (eq nil (dfa-intersection *dfa-0* *dfa-1*)))
(assert (eq t (dfa-intersection *dfa-2* *dfa-1*)))

