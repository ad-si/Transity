module ApproxRatio where

-- TODO: Implement (cloned from Haskell's approxRational)
-- simplest x y =
--   let
--     xr  = fromInt x
--     n   = numerator xr
--     d   = denominator xr
--     nd' = fromInt y
--     n'  = numerator nd'
--     d'  = denominator nd'
--   in
--          if y < x     then simplest y x
--     else if x == y    then xr
--     else if x > 0     then simplest' n d n' d'
--     else if y < 0     then - simplest' (-n') d' (-n) d
--                       else  0 % 1

-- simplest' n d n' d' = -- assumes 0 < n%d < n'%d'
--   let
--     q = floor (n / d)
--     r = floor (n `mod` d)
--     q' = floor (n' / d')
--     r' = floor (n' `mod` d')
--     nd'' =  simplest' d' r' d r
--     n''  =  numerator nd''
--     d''  =  denominator nd''
--   in
--          if r == 0 then q % 1
--     else if q /= q'  then (q + 1) % 1
--                      else (q * n'' + d'') % n''


-- approxRational :: Number -> Number -> Rational
-- approxRational rat eps  =
--   simplest (rat - eps) (rat + eps)
