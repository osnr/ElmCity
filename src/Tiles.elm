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
