module OakDance where
import Euterpea

oakDance :: Music Pitch
oakDance = tempo (80/120) $ 
           voices [ flutePart
                  , mandolinPart
                  , violin1Part
                  , violin2Part
                  ]

voices = chord

-- flute

flutePart :: Music Pitch
flutePart = instrument Flute chorusFlute

chorusFlute :: Music Pitch
--                   |----------:---*-------:-------*---:-----------|----------:---*-------:-------*---:-----------|----------:---*-------:-------*---:-----------|----------:---*-------:-------*---:-----------|
chorusFlute = line [ d 6 tq2,                       a 6 tq,         a 6 tq2,                       a 6 tq,         g 6 tq2,                       f 6 tq,         e 6 tq53,              d 6 t13,c 6 tq
                   , d 6 tq2,                       a 6 tq,         g 6 tq2,                       a 6 qn,                                        g 6 t12,f 6 t12,g 6 tq2,                       f 6 tq
                   , g 6 tq2,                       g 6 tq,         f 6 tq2,                       e 6 tq,         f 6 tq2,                       e 6 t12,d 6 t12,e 6 tq2,                       a 5 tq
                   , a 6 tq,        d 7 tq,         a 6 tq,         g 6 tq,        f 6 tq,         e 6 tq,         d 6 t12,f 6 t12,e 6 en,                c 6 t12,d 6 tq2,                       d 6 tq
                   ]

-- mandolin

mandolinPart :: Music Pitch
mandolinPart = instrument mandolin chorusMandolin

mandolin = AcousticGuitarSteel

chorusMandolin :: Music Pitch
chorusMandolin = line $ applySegmentedRhythm chorusMandolinChords chorusMandolinRhythm

chorusMandolinChords :: [Dur -> Music Pitch]
chorusMandolinChords = times 3 [mandolinChordD1, mandolinChordD1, mandolinChordG3, mandolinChordD1] ++ [mandolinChordD3, mandolinChordD3, mandolinChordG1, mandolinChordG1]

mandolinChordD1 :: Dur -> Music Pitch
mandolinChordD1 = mandolinChord (d 5) (a 5)

mandolinChordD3 :: Dur -> Music Pitch
mandolinChordD3 = mandolinChord (a 4) (d 5)

mandolinChordG1 :: Dur -> Music Pitch
mandolinChordG1 = mandolinChord (g 4) (d 5)

mandolinChordG3 :: Dur -> Music Pitch
mandolinChordG3 = mandolinChord (d 5) (g 5)

mandolinChord :: (Dur -> Music Pitch) -> (Dur -> Music Pitch) -> Dur -> Music Pitch
mandolinChord n1 n2 dur = n1 dur :=: n2 dur

chorusMandolinRhythm :: [[Dur]]
chorusMandolinRhythm = times 7 [mandolinRhythm1, mandolinRhythm2] ++ times 2 [mandolinRhythm2]

mandolinRhythm1 :: [Dur]
mandolinRhythm1 = [en, en * 1/3, en * 2/3]

mandolinRhythm2 :: [Dur]
mandolinRhythm2 = [tq2, tq]


-- violin 1

violin1Part :: Music Pitch
violin1Part = instrument Violin chorusViolin1

chorusViolin1 :: Music Pitch
--                     |----------:-----------:-----------:-----------|
chorusViolin1 = line [ d 5 dhn,                           a 4 qn
                     , d 5 dhn,                           as 4 tq2, a 4 tq
                     , g 4 dhn,                           a 4 qn
                     , d 5 dhn,                           d 5 qn
                     ]


-- violin 2

violin2Part :: Music Pitch
violin2Part = instrument Violin violin2Chorus

violin2Chorus :: Music Pitch
violin2Chorus = line  $ applyRhythm violin2ChorusMelody violin2ChorusRhythm

violin2ChorusMelody :: [Dur -> Music Pitch]
violin2ChorusMelody = concat [times 2 [d 5, a 4], times 4 [a 4, g 4], [f 4, g 4, bf 4, a 4], times 2 [g 4, f 4], [e 4, f 4, e 4, c 4, d 4, a 4, d 5, a 4, g 4, a 4, d 4, d 5]]

violin2ChorusRhythm :: [Dur]
violin2ChorusRhythm = times 16 [qn * 2/3, qn * 1/3]


-- rhythm

applySegmentedRhythm :: [Dur -> Music Pitch] -> [[Dur]] -> [Music Pitch]
applySegmentedRhythm chords = concat . zipWith map chords

applyRhythm :: [Dur -> Music Pitch] -> [Dur] -> [Music Pitch]
applyRhythm = zipWith ($)

-- durations
tq = qn / 3
tq2 = 2 * tq
tq53 = 5/3 * tq
t13 = tq / 3
t12 = tq / 2


times :: Int -> [a] -> [a]
times n = concat . take n . repeat