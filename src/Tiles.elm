module Tiles where

{- presets -}
-- from https://groups.google.com/forum/#!msg/elm-discuss/hEfUdISni1M/bUqce1WllNMJ
cropImage url origWidth origHeight left top width height = 
          collage width height 
                  [sprite url origWidth origHeight (origWidth `div` 2 - left, origHeight `div` 2 - top)]

cropTileset tx ty = toForm (8, 8)
                  $ cropImage "../sprites/tileset1x.png"
                              256 960 (16 * tx) (16 * ty) 16 16

data Position9 = TopLeft | TopCenter | TopRight |
                 CenterLeft | Center | CenterRight |
                 BottomLeft | BottomCenter | BottomRight

data Tile = Dirt | Water | Coast
          | Residential Position9 | Commercial Position9 | Industrial Position9
          | PoliceDept Position9 | FireDept Position9
          | Road | Railroad

-- tileToSprite :: Tile -> Form
tileToSprite t = case t of
                      Dirt -> cropTileset 0 0
                      Water -> cropTileset 2 0

                      Residential TopLeft -> cropTileset 0 15
                      Residential TopCenter -> cropTileset 1 15
                      Residential TopRight -> cropTileset 2 15

                      Residential CenterLeft -> cropTileset 3 15
                      Residential Center -> cropTileset 4 15
                      Residential CenterRight -> cropTileset 5 15

                      Residential BottomLeft -> cropTileset 6 15
                      Residential BottomCenter -> cropTileset 7 15
                      Residential BottomRight -> cropTileset 8 15

                      Commercial TopLeft -> cropTileset 7 26
                      Commercial TopCenter -> cropTileset 8 26
                      Commercial TopRight -> cropTileset 9 26

                      Commercial CenterLeft -> cropTileset 10 26
                      Commercial Center -> cropTileset 11 26
                      Commercial CenterRight -> cropTileset 12 26

                      Commercial BottomLeft -> cropTileset 13 26
                      Commercial BottomCenter -> cropTileset 14 26
                      Commercial BottomRight -> cropTileset 15 26

                      Industrial TopLeft -> cropTileset 4 38
                      Industrial TopCenter -> cropTileset 5 38
                      Industrial TopRight -> cropTileset 6 38

                      Industrial CenterLeft -> cropTileset 7 38
                      Industrial Center -> cropTileset 8 38
                      Industrial CenterRight -> cropTileset 9 38

                      Industrial BottomLeft -> cropTileset 10 38
                      Industrial BottomCenter -> cropTileset 11 38
                      Industrial BottomRight -> cropTileset 12 38

                      FireDept TopLeft -> cropTileset 9 47
                      FireDept TopCenter -> cropTileset 10 47
                      FireDept TopRight -> cropTileset 11 47

                      FireDept CenterLeft -> cropTileset 12 47
                      FireDept Center -> cropTileset 13 47
                      FireDept CenterRight -> cropTileset 14 47

                      FireDept BottomLeft -> cropTileset 15 47
                      FireDept BottomCenter -> cropTileset 0 48
                      FireDept BottomRight -> cropTileset 1 48

                      PoliceDept TopLeft -> cropTileset 2 48
                      PoliceDept TopCenter -> cropTileset 3 48
                      PoliceDept TopRight -> cropTileset 4 48

                      PoliceDept CenterLeft -> cropTileset 5 48
                      PoliceDept Center -> cropTileset 6 48
                      PoliceDept CenterRight -> cropTileset 7 48

                      PoliceDept BottomLeft -> cropTileset 8 48
                      PoliceDept BottomCenter -> cropTileset 9 48
                      PoliceDept BottomRight -> cropTileset 10 48

                      Road -> cropTileset 2 4
                      Railroad -> cropTileset 2 14
