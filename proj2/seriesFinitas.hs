module Soma (somaInt, totalInt, somaImpares, totalImpares, somaPares, totalPares, somaQuadradoInteiros, totalQuadradoInteiros, somaQuadradoImpares) where

somaInt n = map (\x->(x * (x + 1)/2))[1..n]
totalInt n = last( somaInt n)

somaImpares n = map(\x->(x * (1 + (x * 2 - 1))/2))[1..n]
totalImpares n = last (somaImpares n)

somaPares n = map(\x->(x * (2 + 2 * x)/2))[1..n]
totalPares n = last(somaPares n)

somaQuadradoInteiros n = map(\x->((x * (1 + x) * (2 * x + 1))/6))[1..n]
totalQuadradoInteiros n = last(somaQuadradoInteiros n)

somaQuadradoImpares n = map(\x->((x * (2 * x - 1) * (2 * x + 1))/3))[1..n]
totalQuadradoImpares n = last(somaQuadradoImpares n)