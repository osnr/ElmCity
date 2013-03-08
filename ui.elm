{- presets -}
-- from https://groups.google.com/forum/#!msg/elm-discuss/hEfUdISni1M/bUqce1WllNMJ
cropImage url origWidth origHeight left top width height = 
          collage width height 
                  [sprite url origWidth origHeight (origWidth `div` 2 - left, origHeight `div` 2 - top)]

replicate n x = if n == 0
                   then []
                   else x : replicate (n - 1) x

cropTileset tx ty = toForm (8, 8) $ cropImage "sprites/tileset1x.png" 256 960 (16 * tx) (16 * ty) 16 16

data Tile = Dirt | Water | Coast

mapWidth = 10
mapHeight = 10

mapTiles = replicate mapHeight $ replicate mapWidth Dirt

tileToSprite t = case t of
                      Dirt -> cropTileset 0 0
                      Water -> cropTileset 2 0

{- reactive part -}
delta = lift inSeconds (fps 30)

{- rendering part -}

-- horrifying
zipCoords rs = let rsWithX = map (\r -> zip r [0..length r - 1]) rs in
               zipWith (\r y -> zipWith (\(t, x) y -> (t, x, y)) r $ replicate (length r) y) rsWithX [0..length rsWithX]

mapViewTiles = map (\(t, x, y) -> move (16 * x) (16 * y) $ tileToSprite t) $ concat $ zipCoords mapTiles
mapView = collage 300 300

leftPane = layers [mapView mapViewTiles]

{- right pane -}
zones = [{ name = "Residential", shortName = "R" },
         { name = "Commercial", shortName = "C" },
         { name = "Industrial", shortName = "I" }]

services = [{ name = "Police Station", shortName = "PD" },
            { name = "Fire Department", shortName = "FD" }]

transports = [{ name = "Road", shortName = "Rd" },
              { name = "Railroad", shortName = "RR" }]

elementRows = map (flow right . map (fst . Input.button . .shortName)) [zones, services, transports]

rightPane = flow down elementRows

main = flow right [leftPane, rightPane]
