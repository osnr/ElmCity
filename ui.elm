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

{- view -}
tileToSprite t = case t of
                      Dirt -> cropTileset 0 0
                      Water -> cropTileset 2 0

zipCoords rs = let rsWithX = map (\r -> zip r [0..length r - 1]) rs in
               zipWith (\r y -> zipWith (\(t, x) y -> (t, x, y)) r $ replicate (length r) y) rsWithX [0..length rsWithX]

drawMapTiles rs = map (\(t, x, y) -> move (16 * x) (16 * y) $ tileToSprite t) $ concat $ zipCoords rs

drawMapView (x, y) ts = toForm (80 + x, 80 + y) . color red $ collage 180 180 ts

{- reactive part (controller?) -}
-- toolMapView :: Form -> Bool -> Form
toolMapView mv isDown = if isDown then (rotate 0.2 mv) else mv

-- draggable :: Form -> Automaton (Bool,(Int,Int)) Form
defaultToolView initPos = let mv = drawMapView initPos $ drawMapTiles defaultMapTiles in
                              Automaton.run (draggable mv >>^ toolMapView)
                                            (lift2 (\a b -> (a, b)) Mouse.isDown Mouse.position)
                                            ~ Mouse.isDown

-- defaultToolView (0, 0) :: Signal Form

-- draggable <~ defaultToolView (0, 0) :: Signal (Automaton (Bool,(Int,Int)) Form)
-- Automaton.run <~ (draggable <~ defaultToolView (0, 0)) :: Signal (Signal (Bool, Int) -> Signal Form)

-- dragMapView initPos = (Automaton.run <~ (draggable <~ defaultToolView (0, 0)))
--                     ~ lift2 (\a b -> (a, b)) Mouse.isDown Mouse.position

-- lift2 Automaton.run :: Signal (Automaton (Bool,Int) Form) -> Signal (Signal (Bool,Int)) -> Signal (Signal Form)

leftPane = (\view -> color blue $ collage 500 500 [view]) <~ defaultToolView (0, 0)

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

main = (\l -> flow right [l, rightPane]) <~ leftPane
