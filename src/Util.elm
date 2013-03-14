module Util where

replicate n x = if n == 0
                   then []
                   else x :: replicate (n - 1) x
