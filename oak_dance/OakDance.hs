module OakDance ( oakDance ) where
import Euterpea

oakDance :: Music Pitch
oakDance = instrument Flute (tempo t (chorusFlute))

t = 92/120

chorusFlute :: Music Pitch
--                   |----------:---*-------:-------*---:-----------|----------:---*-------:-------*---:-----------|----------:---*-------:-------*---:-----------|----------:---*-------:-------*---:-----------|
chorusFlute = line [ d 5 tq2,                       a 5 tq,         a 5 tq2,                       a 5 tq,         g 5 tq2,                       f 5 tq,         e 5 tq53,              d 5 t13,c 5 tq
                   , d 5 tq2,                       a 5 tq,         g 5 tq2,                       a 5 qn,                                        g 5 t12,f 5 t12,g 5 tq2,                       f 5 tq
                   , g 5 tq2,                       g 5 tq,         f 5 tq2,                       e 5 tq,         f 5 tq2,                       e 5 t12,d 5 t12,e 5 tq2,                       a 4 tq
                   , a 5 tq,        d 6 tq,         a 5 tq,         g 5 tq,        f 5 tq,         e 5 tq,         d 5 t12,f 5 t12,e 5 en,                c 5 t12,d 5 tq2,                       d 5 tq
                   ]

-- durations
tq = qn / 3
tq2 = 2 * tq
tq53 = 5/3 * tq
t13 = tq / 3
t12 = tq / 2


voice :: [Music Pitch] -> Music Pitch
voice = foldr (:+:) (rest 0)
