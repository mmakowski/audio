module OakDance ( oakDance ) where
import Euterpea

mandolin = AcousticGuitarSteel

oakDance :: Music Pitch
oakDance = tempo t $  chord [ instrument Flute chorusFlute
                            , instrument mandolin chorusMandolin
                            ]

t = 80/120

-- flute

chorusFlute :: Music Pitch
--                   |----------:---*-------:-------*---:-----------|----------:---*-------:-------*---:-----------|----------:---*-------:-------*---:-----------|----------:---*-------:-------*---:-----------|
chorusFlute = line [ d 5 tq2,                       a 5 tq,         a 5 tq2,                       a 5 tq,         g 5 tq2,                       f 5 tq,         e 5 tq53,              d 5 t13,c 5 tq
                   , d 5 tq2,                       a 5 tq,         g 5 tq2,                       a 5 qn,                                        g 5 t12,f 5 t12,g 5 tq2,                       f 5 tq
                   , g 5 tq2,                       g 5 tq,         f 5 tq2,                       e 5 tq,         f 5 tq2,                       e 5 t12,d 5 t12,e 5 tq2,                       a 4 tq
                   , a 5 tq,        d 6 tq,         a 5 tq,         g 5 tq,        f 5 tq,         e 5 tq,         d 5 t12,f 5 t12,e 5 en,                c 5 t12,d 5 tq2,                       d 5 tq
                   ]

-- mandolin

chorusMandolin :: Music Pitch
chorusMandolin = line $ applySegmentedRhythm chorusMandolinChords chorusMandolinRhythm

chorusMandolinChords :: [Dur -> Music Pitch]
chorusMandolinChords = times 3 [mandolinChordD1, mandolinChordD1, mandolinChordG3, mandolinChordD1] ++ [mandolinChordD3, mandolinChordD3, mandolinChordG1, mandolinChordG1]

mandolinChordD1 :: Dur -> Music Pitch
mandolinChordD1 = mandolinChord (d 4) (a 4)

mandolinChordD3 :: Dur -> Music Pitch
mandolinChordD3 = mandolinChord (a 3) (d 4)

mandolinChordG1 :: Dur -> Music Pitch
mandolinChordG1 = mandolinChord (g 3) (d 4)

mandolinChordG3 :: Dur -> Music Pitch
mandolinChordG3 = mandolinChord (d 4) (g 4)

mandolinChord :: (Dur -> Music Pitch) -> (Dur -> Music Pitch) -> Dur -> Music Pitch
mandolinChord n1 n2 dur = n1 dur :=: n2 dur

chorusMandolinRhythm :: [[Dur]]
chorusMandolinRhythm = times 7 [mandolinRhythm1, mandolinRhythm2] ++ times 2 [mandolinRhythm2]

mandolinRhythm1 :: [Dur]
mandolinRhythm1 = [en, en * 1/3, en * 2/3]

mandolinRhythm2 :: [Dur]
mandolinRhythm2 = [tq2, tq]

-- adding rhythm

applySegmentedRhythm :: [Dur -> Music Pitch] -> [[Dur]] -> [Music Pitch]
applySegmentedRhythm chords rhythms = concat $ zipWith (\c r -> map c r) chords rhythms


-- durations
tq = qn / 3
tq2 = 2 * tq
tq53 = 5/3 * tq
t13 = tq / 3
t12 = tq / 2


times :: Int -> [a] -> [a]
times n = concat . take n . repeat