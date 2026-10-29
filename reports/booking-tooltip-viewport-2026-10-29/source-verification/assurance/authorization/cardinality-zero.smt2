(set-option :timeout 30000)
(set-logic QF_LIA)
(declare-fun count () Int)
(assert (let ((a!1 (= (- count (ite (> count 0) 1 0)) 0)))
  (and (>= count 0) (distinct a!1 (<= count 1)))))

(check-sat)
