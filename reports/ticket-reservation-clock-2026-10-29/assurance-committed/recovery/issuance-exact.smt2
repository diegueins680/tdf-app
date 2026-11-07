(set-option :timeout 30000)
(set-logic QF_BV)
(declare-fun issued () (_ BitVec 64))
(assert (let ((a!1 (= #b00000000000000000000000000000000000000000000000000000001110000100
              (bvsub ((_ sign_extend 1) (bvadd issued #x0000000000000384))
                     ((_ sign_extend 1) issued))))
      (a!2 (bvsge (bvsub ((_ sign_extend 1) (bvadd issued #x0000000000000384))
                         ((_ sign_extend 1) issued))
                  #b11000000000000000000000000000000000000000000000000000000000000000))
      (a!3 (bvsle (bvsub ((_ sign_extend 1) (bvadd issued #x0000000000000384))
                         ((_ sign_extend 1) issued))
                  #b00111111111111111111111111111111111111111111111111111111111111111)))
  (and (bvsle #x0000000000000000 issued)
       (bvsgt (bvadd issued #x0000000000000384) issued)
       a!1
       a!2
       a!3
       (distinct ((_ sign_extend 1) (bvadd issued #x0000000000000384))
                 (bvadd ((_ sign_extend 1) issued)
                        #b00000000000000000000000000000000000000000000000000000001110000100)))))

(check-sat)
