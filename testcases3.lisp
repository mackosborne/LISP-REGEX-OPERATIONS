
;; create a simple nfa
;;(regex->nfa '(:concatenation (:union 0 1) (:kleene-closure (:union 0 1))))

;; test the base case of regex->nfa
(regex->nfa '())

(regex->nfa '(:concatenation 0 1))

(typecase '(a b c)
  (symbol
   (print "a symbol"))
  (list
   (print "a list")))

