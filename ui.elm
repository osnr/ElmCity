import Automaton

{- presets -}
-- from https://groups.google.com/forum/#!msg/elm-discuss/hEfUdISni1M/bUqce1WllNMJ
cropImage url origWidth origHeight left top width height = 
          collage width height 
                  [sprite url origWidth origHeight (origWidth `div` 2 - left, origHeight `div` 2 - top)]

replicate n x = if n == 0
                   then []
                   else x :: replicate (n - 1) x

cropTileset tx ty = toForm (8, 8) $ cropImage "sprites/tileset1x.png" 256 960 (16 * tx) (16 * ty) 16 16

data Tile = Dirt | Water | Coast

defaultMapWidth = 10
defaultMapHeight = 10

defaultMapTiles = replicate defaultMapHeight $ replicate defaultMapWidth Dirt
defaultMapViewTiles = mapViewTiles defaultMapTiles

tileToSprite t = case t of
                      Dirt -> cropTileset 0 0
                      Water -> cropTileset 2 0

{- reactive part -}
zipCoords rs = let rsWithX = map (\r -> zip r [0..length r - 1]) rs in
               zipWith (\r y -> zipWith (\(t, x) y -> (t, x, y)) r $ replicate (length r) y) rsWithX [0..length rsWithX]

mapViewTiles rs = map (\(t, x, y) -> move (16 * x) (16 * y) $ tileToSprite t) $ concat $ zipCoords rs
mapView (x, y) ts = toForm (80 + x, 80 + y) $ color red $ collage 180 180 ts

-- main = color blue $ collage 500 500 [mapView (0, 0) $ mapViewTiles defaultMapTiles]

-- main = plainText $ concat $ map (\(t, x, y) -> case t of Dirt -> "; dirt, " ++ show x ++ ", " ++ show y) $ concat $ zipCoords defaultMapTiles

dragMapView initPos = Automaton.run (draggable
                      (mapView (0, 0) defaultMapViewTiles))
                      $ lift2 (\a b -> (a, b)) Mouse.isDown Mouse.position

main = (\view -> color blue $ collage 500 500 [view]) <~ dragMapView (0, 0)

-- leftPane = layers [mapView mapViewTiles]

-- {- right pane -}
-- zones = [{ name = "Residential", shortName = "R" },
--          { name = "Commercial", shortName = "C" },
--          { name = "Industrial", shortName = "I" }]

-- services = [{ name = "Police Station", shortName = "PD" },
--             { name = "Fire Department", shortName = "FD" }]

-- transports = [{ name = "Road", shortName = "Rd" },
--               { name = "Railroad", shortName = "RR" }]

-- elementRows = map (flow right . map (fst . Input.button . .shortName)) [zones, services, transports]

-- rightPane = flow down elementRows

-- main = flow right [leftPane, rightPane]