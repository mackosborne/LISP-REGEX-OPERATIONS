;; optimize for debugging
(declaim (optimize (debug 3)))

(simplify-regex '(:. (:union 1 1)) '(0 1))

(simplify-regex '(:? (:concatenation 1 0)) '(0 1))

(simplify-regex '(:? :.) '(0 1))
;; => (:union :epsilon (:union 0 1))

(simplify-regex '(:+ :.) '(0 1))
;; => (:concatenation (:union 0 1) (:kleene-closure (:union 0 1)))

(simplify-regex '(:? (:union 0 1)) '(0 1))
