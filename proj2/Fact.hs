module Fact ( fat ) where
fat 0 = 1
fat n = n * fat (n - 1)
