{-|
An Euterpea rendition of a folk-ish tune I composed long time ago.
-}
module OakDance where
import Euterpea

oakDance :: Music Pitch
oakDance = tempo (80/120) $ 
           voices [ flutePart
                  , mandolinPart
                  , violin1Part
                  , violin2Part
                  , violaPart
                  , celloPart
                  ] :=:
           percussionPart

-- the octave numbers seem to be off by 1, need to transpose all melodic parts
voices = transpose 8 . chord


-- flute

flutePart :: Music Pitch
flutePart = instrument Flute $ line [fluteVerse1, fluteChorus, fluteVerse2, fluteChorus, fluteVerse3, fluteChorus]

fluteVerse1 :: Music Pitch
--                   |----------:---*-------:-------*---:-----------|----------:---*-------:-------*---:-----------|----------:---*-------:-------*---:-----------|----------:---*-------:-------*---:-----------|
fluteVerse1 = line [ g 5 qn,                                        f 5 qn,                                        e 5 tq,        d 5 tq,         c 5 tq,         d 5 qn
                   , g 5 qn,                                        f 5 qn,                                        e 5 tq,        f 5 tq,         c 5 tq,         d 5 qn
                   , g 5 qn,                                        f 5 qn,                                        e 5 tq,        d 5 tq,         c 5 tq,         d 5 qn
                   , g 5 tq,        a 5 tq,         bf 5 tq,        a 5 tq,        fs 5 tq,        d 5 tq,         c 5 tq,        d 5 tq,         fs 5 tq,        g 5 qn
                   , bf 5 qn,                                       c 6 qn,                                        bf 5 tq,       a 5 tq,         g 5 tq,         a 5 qn
                   , bf 5 qn,                                       g 5 qn,                                        fs 5 tq,       d 5 tq,         c 5 tq,         d 5 qn
                   , f 5 qn,                                        g 5 qn,                                        a 5 tq,        bf 5 tq,        a 5 tq,         g 5 qn
                   , f 5 tq,        e 5 tq,         d 5 tq,         e 5 tq2,                       f 5 tq,         e 5 tq2,                       cs 5 tq,        a 4 qn
                   , d 5 tq,        e 5 tq,         f 5 tq,         g 5 tq2,                       f 5 tq,         e 5 tq2,                       a 5 tq,         g 5 qn
                   , f 5 tq,        e 5 tq,         d 5 tq,         g 5 tq2,                       f 5 tq,         e 5 tq2,                       a 5 tq,         a 5 qn
                   , bf 5 tq,       a 5 tq,         g 5 tq,         c 6 tq2,                       a 5 tq,         f 5 tq2,                       g 5 tq,         a 5 qn
                   , g 5 qn,                                        f 5 tq,        e 5 tq,         d 5 tq,         e 5 hn
                   ]


fluteChorus :: Music Pitch
--                   |----------:---*-------:-------*---:-----------|----------:---*-------:-------*---:-----------|----------:---*-------:-------*---:-----------|----------:---*-------:-------*---:-----------|
fluteChorus = line [ d 5 tq2,                       a 5 tq,         a 5 tq2,                       a 5 tq,         g 5 tq2,                       f 5 tq,         e 5 tq53,              d 5 t13,c 5 tq
                   , d 5 tq2,                       a 5 tq,         g 5 tq2,                       a 5 qn,                                        g 5 t12,f 5 t12,g 5 tq2,                       f 5 tq
                   , g 5 tq2,                       g 5 tq,         f 5 tq2,                       e 5 tq,         f 5 tq2,                       e 5 t12,d 5 t12,e 5 tq2,                       a 4 tq
                   , a 5 tq,        d 6 tq,         a 5 tq,         g 5 tq,        f 5 tq,         e 5 tq,         d 5 t12,f 5 t12,e 5 en,                c 5 t12,d 5 tq2,                       d 5 tq
                   ]

fluteVerse2 :: Music Pitch
--                   |----------:---*-------:-------*---:-----------|----------:---*-------:-------*---:-----------|----------:---*-------:-------*---:-----------|----------:---*-------:-------*---:-----------|
fluteVerse2 = line [ a 4 tq2,                       d 5 tq,         a 5 tq2,                       d 5 tq,         g 5 tq2,                       f 5 tq,         e 5 tq2,                       d 5 t12, c 5 t12
                   , d 5 tq2,                       a 4 tq,         g 4 tq2,                       c 5 tq,         a 4 tq2,                       g 4 tq,         a 4 tq2,                       a 4 tq
                   , d 5 tq2,                       c 5 tq,         d 5 tq2,                       e 5 tq,         f 5 tq2,                       e 5 t12,d 5 t12,c 5 tq2,                       a 4 tq
                   , f 4 tq2,                       f 4 tq,         g 4 tq2,                       g 4 tq,         a 4 tq2,                       g 4 tq,         a 4 tq2,                       a 4 tq
                   , d 5 tq2,                       g 5 tq,         a 5 tq2,                       bf 5 tq,        g 5 tq2,                       f 5 tq,         g 5 tq2,                       a 5 tq
                   , d 6 tq2,                       a 5 tq,         c 6 tq2,                       a 5 tq,         g 5 tq2,                       f 5 tq,         g 5 tq2,                       c 5 tq
                   , d 5 tq2,                       d 5 tq,         e 5 tq2,                       e 5 tq,         f 5 tq2,                       e 5 t12,d 5 t12,c 5 tq2,                       a 4 tq
                   , d 5 tq2,                       a 4 tq,         g 4 tq2,                       c 5 tq,         a 4 tq2,                       g 4 tq,         a 4 tq2,                       a 4 tq
                   , c 5 tq2,                       d 5 tq,         f 5 tq2,                       g 5 tq,         f 5 tq2,                       e 5 tq,         f 5 tq2,                       g 5 tq
                   , a 5 tq2,                       d 6 tq,         g 5 tq2,                       c 6 tq,         a 5 tq2,                       g 5 tq,         a 5 tq2,                       a 5 tq
                   , bf 5 tq2,                      a 5 t12,g 5 t12,f 5 tq2,                       e 5 tq,         f 5 tq2,                       d 5 tq,         c 5 tq2,                       a 4 tq
                   , d 5 tq2,                       d 5 tq,         g 5 tq2,                       g 5 tq,         a 5 tq2,                       g 5 tq,         a 5 tq2,                       a 5 tq
                   , d 6 tq2,                       c 6 tq,         d 6 tq2,                       d 6 tq,         c 6 tq2,                       bf 5 tq,        a 5 tq2,                       a 5 tq
                   , d 6 tq2,                       c 6 tq,         d 6 tq2,                       c 6 t12,bf 5 t12,a 5 tq2,                      g 5 tq,         a 5 tq2,                       a 5 tq
                   , d 5 tq2,                       d 5 tq,         f 5 tq2,                       f 5 tq,         g 5 tq2,                       f 5 tq,         g 5 tq2,                       g 5 tq
                   , d 5 tq2,                       d 5 tq,         f 5 tq2,                       f 5 tq,         g 5 tq2,                       f 5 tq,         g 5 tq2,                       g 5 tq
                   , f 5 tq2,                       e 5 tq,         d 5 tq2,                       c 5 tq,         a 4 tq2,                       c 5 tq,         a 4 tq2,                       a 4 tq
                   , g 4 tq2,                       g 4 tq,         f 4 tq2,                       g 4 tq,         a 4 tq2,                       g 4 tq,         a 4 tq2,                       a 4 tq
                   , d 5 tq2,                       f 5 tq,         e 5 tq2,                       d 5 tq,         c 5 tq2,                       d 5 tq,         a 4 tq2,                       g 4 tq
                   , f 4 tq2,                       f 4 tq,         g 4 tq2,                       g 4 tq,         a 4 tq2,                       g 4 tq,         a 4 tq2,                       a 4 tq
                   ]

fluteVerse3 :: Music Pitch
--                   |----------:---*-------:-------*---:-----------|----------:---*-------:-------*---:-----------|----------:---*-------:-------*---:-----------|----------:---*-------:-------*---:-----------|
fluteVerse3 = line [ a 4 tq2,                       a 4 tq,         d 5 tq2,                       c 5 tq,         bf 4 tq2,                      c 5 tq,         a 4 tq2,                       d 4 tq
                   , f 4 tq2,                       f 4 tq,         f 4 tq2,                       f 4 tq,         g 4 tq2,                       f 4 t12,g 4 t12,a 4 tq2,                       a 4 tq
                   , d 5 tq2,                       e 5 tq,         f 5 tq2,                       e 5 t12,d 5 t12,c 5 tq,        a 4 tq,         g 4 tq,         a 4 tq2,                       d 5 tq
                   , f 4 qn,                                        g 4 qn,                                        a 4 hn
                   , tqr,           d 5 tq,         e 5 tq,         f 5 tq2,                       e 5 t12,f 5 t12,g 5 tq2,                       f 5 t12,g 5 t12,a 5 tq2,                       a 5 tq
                   , f 5 tq2,                       a 5 tq,         f 5 tq2,                       e 5 t12,d 5 t12,c 5 tq2,                       a 4 t12,g 4 t12,a 4 tq2,                       a 4 tq
                   , d 5 tq,        d 5 tq,         e 5 tq,         f 5 tq2,                       e 5 t12,d 5 t12,e 5 tq2,                       d 5 tq,         c 5 tq2,                       bf 4 tq
                   , a 4 tq,        d 5 tq,         a 4 tq,         g 4 tq,        f 4 tq,         g 4 tq,         a 4 tq2,                       g 4 tq,         a 4 tq2,                       a 4 tq
                   , d 5 tq,        a 5 tq,         a 5 tq,         d 5 tq,        a 5 tq,         a 5 tq,         g 5 tq,        f 5 tq,         g 5 tq,         e 5 tq2,                       c 5 tq
                   , d 5 tq,        a 4 tq,         a 4 tq,         d 5 tq,        a 4 tq,         a 4 tq,         g 4 t12,c 5 t12,a 4 tq32,              g 4 t12,a 4 tq2,                       a 4 tq
                   , d 5 tq,        a 4 tq,         d 5 tq,         c 5 tq2,                       a 4 tq,         bf 4 tq,       a 4 tq,         g 4 tq,         a 4 tq2,                       d 4 tq
                   , f 5 tq,        e 5 tq,         f 5 tq,         g 5 tq2,                       d 5 tq,         e 5 tq,        d 5 tq,         c 5 tq,         a 4 tq2,                       c 5 tq
                   ]

-- mandolin

mandolinPart :: Music Pitch
mandolinPart = instrument AcousticGuitarSteel $ line $ [mandolinVerse1, mandolinChorus, mandolinVerse2, mandolinChorus, mandolinVerse3, mandolinChorus]

mandolinVerse1 :: Music Pitch
mandolinVerse1 = line $ times 3 [mandolin4Bars]

mandolinChorus :: Music Pitch
mandolinChorus = mandolin4Bars

mandolinVerse2 :: Music Pitch
mandolinVerse2 = line $ times 5 [mandolin4Bars]

mandolinVerse3 :: Music Pitch
mandolinVerse3 = line $ times 3 [mandolin4Bars]

mandolin4Bars :: Music Pitch
mandolin4Bars = line $ applySegmentedRhythm mandolin4BarsChords mandolin4BarsRhythm

mandolin4BarsChords :: [Dur -> Music Pitch]
mandolin4BarsChords = times 3 [mandolinChordD1, mandolinChordD1, mandolinChordG3, mandolinChordD1] ++ [mandolinChordD3, mandolinChordD3, mandolinChordG1, mandolinChordG1]

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

mandolin4BarsRhythm :: [[Dur]]
mandolin4BarsRhythm = times 7 [mandolinRhythm1, mandolinRhythm2] ++ times 2 [mandolinRhythm2]

mandolinRhythm1 :: [Dur]
mandolinRhythm1 = [en, en * 1/3, en * 2/3]

mandolinRhythm2 :: [Dur]
mandolinRhythm2 = [tq2, tq]


-- violin 1

violin1Part :: Music Pitch
violin1Part = instrument Violin $ line [violin1Verse1, violin1Chorus]

violin1Verse1 :: Music Pitch
violin1Verse1 = verseRest

violin1Chorus :: Music Pitch
violin1Chorus = line [d 5 dhn, a 4 qn, d 5 dhn, bf 4 tq2, a 4 tq, g 4 dhn, a 4 qn, d 5 dhn, d 5 qn]


-- violin 2

violin2Part :: Music Pitch
violin2Part = instrument Violin $ line [violin2Verse1, violin2Chorus]

violin2Verse1 :: Music Pitch
violin2Verse1 = verseRest

violin2Chorus :: Music Pitch
violin2Chorus = line $ applyRhythm violin2ChorusMelody violin2ChorusRhythm

violin2ChorusMelody :: [Dur -> Music Pitch]
violin2ChorusMelody = concat [times 2 [d 5, a 4], times 4 [a 4, g 4], [f 4, g 4, bf 4, a 4], times 2 [g 4, f 4], [e 4, f 4, e 4, c 4, d 4, a 4, d 5, a 4, g 4, a 4, d 4, d 5]]

violin2ChorusRhythm :: [Dur]
violin2ChorusRhythm = times 16 [qn * 2/3, qn * 1/3]


-- viola

violaPart :: Music Pitch
violaPart = instrument Viola $ line [violaVerse1, violaChorus, violaVerse2, violaChorus, violaVerse3, violaChorus]

violaVerse1 :: Music Pitch
violaVerse1 = verseRest

violaChorus :: Music Pitch
violaChorus = line [d 3 wn, a 3 wn, bf 3 hn, c 4 hn, d 3 (wn * 11/12), d 4 tq]

violaVerse2 :: Music Pitch
violaVerse2 = line $ concat [times 3 [wnr], violaTutti, times 3 [wnr], violaTutti, times 3 (violaVerse2Motive ++ violaTutti)]

violaTutti :: [Music Pitch]
violaTutti = [d 3 qn, c 3 tq2, d 3 t12, c 3 t12, d 3 hn]

violaVerse2Motive :: [Music Pitch]
violaVerse2Motive = applyRhythm (map (applyOct 3) [d, d, c, c, d, f]) (times 6 [hn])

violaVerse3 :: Music Pitch
violaVerse3 = verseRest


-- cello

celloPart :: Music Pitch
celloPart = instrument Cello $ line [celloVerse1, celloChorus, celloVerse2, celloChorus, celloVerse3, celloChorus]

celloVerse1 :: Music Pitch
celloVerse1 = verseRest

celloChorus :: Music Pitch
celloChorus = line [d 3 hn, g 2 hn, d 3 hn, a 2 hn, bf 2 hn, c 3 hn, d 3 hn, g 2 tq2, a 2 tq, d 2 qn]

celloVerse2 :: Music Pitch
celloVerse2 = line $ concat [times 3 [wnr], [d 3 qn, c 3 qn, g 2 hn], times 4 [celloVerse2Motive]]

celloVerse3 :: Music Pitch
celloVerse3 = verseRest

celloVerse2Motive :: Music Pitch
celloVerse2Motive = line $ applyRhythm celloVerse2MotiveMelody (times 16 [tq2, tq])

celloVerse2MotiveMelody :: [Dur -> Music Pitch]
celloVerse2MotiveMelody = map (applyOct 2) [ d, d, d, d, g, g, d, d
                                           , c, c, c, c, d, f, d, d
                                           , g, g, g, g, f, e, f, c
                                           , d, f, d, f, g, g, g, g
                                           ]

applyOct :: Octave -> (Octave -> a) -> a
applyOct = flip ($)

-- percussion

percussionPart :: Music Pitch
percussionPart = instrument Percussion $ line [percussionVerse1, percussionChorus]

percussionVerse1 :: Music Pitch
percussionVerse1 = verseRest

percussionChorus :: Music Pitch
percussionChorus = percussionChorusTriangle :=: percussionChorusWoodBlocks 

percussionChorusTriangle :: Music Pitch
percussionChorusTriangle = line $ concat [times 3 [wnr], times 3 [qnr], map openTriangle [tq2, tq]]

percussionChorusWoodBlocks :: Music Pitch
percussionChorusWoodBlocks = line $ map lowWoodBlock $ times 16 [tq53, t13, tq]

openTriangle  = perc OpenTriangle
highWoodBlock = perc HiWoodBlock
lowWoodBlock  = perc LowWoodBlock

-- rhythm

applySegmentedRhythm :: [Dur -> Music Pitch] -> [[Dur]] -> [Music Pitch]
applySegmentedRhythm chords = concat . zipWith map chords

applyRhythm :: [Dur -> Music Pitch] -> [Dur] -> [Music Pitch]
applyRhythm = zipWith ($)

-- durations
tq = qn / 3
tq2 = 2 * tq
tq53 = 5/3 * tq
tq32 = 3/2 * tq
t13 = tq / 3
t12 = tq / 2
tqr = rest tq

verseRest :: Music Pitch
verseRest = line $ times 12 [wnr]

times :: Int -> [a] -> [a]
times n = concat . take n . repeat