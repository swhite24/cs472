(load "main.lisp")

(format t "DE: 10^6 gens, single objective (energy)~%")
(rat_de :gens 1000000 :n 1 :summarize_freq 100000)

(format t "DE: 10^6 gens, two objective (energy + children)~%")
(rat_de :gens 1000000 :n 1 :summarize_freq 100000
	:obj_func #'two_obj)

(format t "GA: 10^6 gens, single objective (energy)~%")
(rat_ga :gens 1000000 :n 1 :summarize_freq 100000)

(format t "GA: 10^6 gens, two objective (energy + children)~%")
(rat_ga :gens 1000000 :n 1 :summarize_freq 100000
	:obj_func #'two_obj)