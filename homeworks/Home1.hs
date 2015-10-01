{-
 -
 -                4
 - pi = -------------------
 -                1^2
 -       1 + --------------
 -                   3^2
 -            2 + ---------
 -                     5^2
 -                 2 + ----
 -                      ...
 -}
pi1 n = 4 / (1 + (pi1' n 1))
    where pi1' 1 numerator = 2
          pi1' n numerator = (numerator ^ 2) / (2 + (pi1' (n-1) (numerator + 2)))

{-
 -                  1^2
 - pi = 3 + -------------------
 -                   3^2
 -           6 + -------------
 -                     5^2
 -                6 + ------
 -                     ...
 -}

pi2 n = 3 + pi2' n 1
    where pi2' 1 numerator = 6
          pi2' n numerator = (numerator ^ 2) / (6 + pi2' (n-1) (numerator + 2))

{-
 -                 4
 - pi = ------------------------
 -                   1^2
 -       1 + ------------------
 -                    2^2
 -            3 + -----------
 -                     3^2
 -                 5 + ---
 -                     ...
 -}

pi3 n = 4 / (1 + (pi3' n 1 3))
    where pi3' 1 numerator denominator = numerator
          pi3' n numerator denominator = ((numerator ^ 2) /
              (denominator + (pi3' (n-1) (numerator + 1) (denominator + 2))))


{-       4     4     4     4
 - pi = --- - --- + --- - --- + ...
 -       1     3     5     7
 -}

pi4 n = -(pi4' n 1)
    where pi4' 0 denominator = 0
          pi4' n denominator | n `mod` 2 == 0 = -(4 / denominator) + pi4' (n-1) (denominator+2)
                             | otherwise      = 4 / denominator + pi4' (n-1) (denominator+2)

{-             4         4         4         4
 - pi = 3 + ------- - ------- + ------- - -------- + ...
 -           2*3*4     4*5*6     6*7*8     8*9*10
 -}

pi5 n = 3 + pi5' n 2
    where pi5' 1 denominator = 0
          pi5' n denominator | n `mod` 2 == 0 = -(4 / (denominator * (denominator + 1) * (denominator + 2)))
                                + pi5' (n-1) (denominator+2)
                             | otherwise      =   4 / (denominator * (denominator + 1) * (denominator + 2))
                                + pi5' (n-1) (denominator+2)



factorial n = if n==0
              then 1
              else n*factorial (n-1)

{-
       x^1     x^2
e^x = ----- + ----- + ...
        1!      2!
-}

e x n = es' x n 1
        where es' x n 0 = 0
              es' x n i = (x**i) / (factorial i) + (es' x (n-1) (i+1))