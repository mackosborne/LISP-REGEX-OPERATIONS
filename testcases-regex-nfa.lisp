(regex->nfa '()) ;; empty regex

(fa-pdf (regex->nfa '(:concatenation (:union 0 1) (:kleene-closure (:union 0 1)))) "testcase-1.pdf")

(fa-pdf (regex->nfa '(:kleene-closure a)) "kleene-a.pdf")

(fa-pdf (regex->nfa '1) "1.pdf")

(fa-pdf (regex->nfa '(:union 0 1)) "union-0-1.pdf")

(fa-pdf (regex->nfa '(:union (:union 0 1) (:union 0 1) (:union 0 1))) "concatenate-0-1.pdf")

;; - (:concatenation a b c)
;; - (:union a b c :epsilon)
;; - (:union)
;; - (:kleene-closure a)
;; - (:concatenation (:union a b) (:kleene-closure c))

(fa-pdf (regex->nfa '(:concatenation a b c)) "concat-a-b-c.pdf")
(fa-pdf (regex->nfa '(:union a b c :epsilon)) "union-a-b-c-ep.pdf")
(fa-pdf (regex->nfa '(:union)) "union-default.pdf")
(fa-pdf (regex->nfa '(:kleene-closure a)) "kleene-a.pdf")
(fa-pdf (regex->nfa '(:concatenation (:union a b) (:kleene-closure c))) "large-test.pdf")

(fa-pdf (regex->nfa2 '(:union)) "union-empty.pdf")

(fa-pdf (regex->nfa '(:concatenation)) "concat-empty.pdf")
