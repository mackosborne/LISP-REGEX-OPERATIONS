;; Make an DFA
(defparameter *dfa-0*
  (make-fa '((q0 0 q0)
             (q0 1 q1)
             (q1 1 q2)
             (q1 0 q0)
	     (q2 0 q1)
	     )
           'q0
           '(q1)))

(fa-pdf *dfa-0* "dfa-0.pdf")
(fa-pdf (dfa-minimize *dfa-0*) "dfa-0-min.pdf")

(fa-pdf (regex->nfa '(:kleene-closure (:union A B))) "ab.pdf")

(fa-pdf (rename-fa-states (dfa-minimize (nfa->dfa (regex->nfa '(:kleene-closure (:union A B)))))) "kleene-unionab.pdf")


(fa-pdf (rename-fa-states (dfa-minimize (nfa->dfa (regex->nfa '(:kleene-closure (:union A B)))))) "unionab.pdf")


(fa-pdf (nfa->dfa (regex->nfa '(:kleene-closure (:union A B C D)))) "abcd-dfa.pdf")

;; this exhausts the stack... 
(fa-pdf (dfa-minimize (nfa->dfa (regex->nfa '(:kleene-closure (:union A B C D))))) "abcd-min.pdf")

(defparameter dfa-regex (nfa->dfa (regex->nfa '(:kleene-closure (:union A B C D)))))


;; check that reversing the dfa does not add an unnecessary start state
(defparameter *dfa-l*
  (make-fa '((a 0 a)
	     (a 1 b)
	     (b 0 a)
	     (b 1 b)
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

(fa-pdf *dfa-l* "dfa-l.pdf")
(fa-pdf *dfa-m* "dfa-m.pdf")

(fa-pdf (dfa-product *dfa-l* *dfa-m*) "lm-prod.pdf")

;; product dfa does NOT set accept states, we can't reverse a product dfa by itself
(fa-pdf (fa-reverse (dfa-product *dfa-l* *dfa-m*)) "rev-lm-prod.pdf")


(fa-pdf (nfa->dfa (fa-reverse *dfa-l*)) "rev-nfa-l.pdf")


(dfa-equivalent (dfa-minimize *dfa-l*) *dfa-l*)

(fa-pdf (dfa-minimize *dfa-l*) "dfa-l-min.pdf")
(fa-pdf (dfa-minimize *dfa-m*) "dfa-m-min.pdf")


