module Quase (quaseUm, quaseDois, quaseE) where
import Fact

quaseUm n = sum (map (\x->(1/(x^2 + x))) (take n [1..]))
quaseDois n = sum (map(\x->(2/(x^2 + x))) (take n [1..]))
quaseE n = 1 + sum(map(\x->(1/fat x)) (take n [1..]))
