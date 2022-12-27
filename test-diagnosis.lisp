(defparameter *dfa-l*
  (make-fa '((a 0 a)
             (b 0 a)
             (b 1 a)
             (a 1 b)
	     )
           'a
           '(b)))

(fa-pdf *dfa-l* "dfa-l.pdf")
(fa-pdf (fa-reverse *dfa-l*) "rev-dfa-l.pdf")
(fa-pdf (dfa-minimize *dfa-l*) "min-dfa-l.pdf")




(fa-pdf (nf-rev *dfa-l*) "nfa-reverse.pdf")
(fa-pdf (rev-nf-rev *dfa-l*) "reverse-nfa-reverse.pdf")


(defun nf-rev (dfa)
  "Return an equivalent DFA with minimum state."
    (nfa->dfa
     (fa-reverse dfa)
    )
   )

(defun rev-nf-rev (dfa)
  "Return an equivalent DFA with minimum state."
   (fa-reverse
    (nfa->dfa
     (fa-reverse dfa)
     )
    )
  )

