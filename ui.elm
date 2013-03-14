import Automaton

{- presets -}
-- from https://groups.google.com/forum/#!msg/elm-discuss/hEfUdISni1M/bUqce1WllNMJ
cropImage url origWidth origHeight left top width height = 
          collage width height 
                  [sprite url origWidth origHeight (origWidth `div` 2 - left, origHeight `div` 2 - top)]

replicate n x = if n == 0
                   then []
                   else x :: replicate (n - 1) x

cropTileset tx ty = toForm (8, 8)
                  $ cropImage "sprites/tileset1x.png"
                              256 960 (16 * tx) (16 * ty) 16 16

data Position4 = TopLeft | TopRight | BottomLeft | BottomRight

data Tile = Dirt | Water | Coast
          | Residential Position4 | Commercial Position4 | Industrial Position4
          | PoliceDept Position4 | FireDept Position4
          | Road | Railroad

defaultMapWidth = 10
defaultMapHeight = 10

defaultMapTiles = replicate defaultMapHeight
                            $ replicate defaultMapWidth Dirt

{- view -}
-- tileToSprite :: Tile -> Form
tileToSprite t = case t of
                      Dirt -> cropTileset 0 0
                      Water -> cropTileset 2 0

-- zipCoords :: [[Tile]] -> [[(Int,Int,Tile)]]
zipCoords rs = let rsWithX = map (\r -> zip [0..length r - 1] r) rs in
                   zipWith (\r y -> zipWith (\(x, t) y -> ((x, y), t)) r
                           $ replicate (length r) y) rsWithX [0..length rsWithX]

mapGrid rs = Dict.fromList . concat . zipCoords $ rs
defaultMapGrid = mapGrid defaultMapTiles

drawMapTiles ts = map (\((tx, ty), t) -> move (16 * tx) (16 * ty) $ tileToSprite t) $ Dict.toList ts
-- main = plainText . show $ Dict.toList defaultMapGrid
drawMapView ts = toForm (80, 80) . color red . collage 180 180 $ drawMapTiles ts

{- reactive part (controller?) -}
data PanState = PanListen (Float,Float) | PanIgnore (Float,Float) | PanFrom (Float,Float)

mapPos pans = case pans of
                   PanListen p -> p
                   PanIgnore p -> p
                   PanFrom p -> p

-- using drag state knowledge, convert an absolute mouse pos to a tile x and y
tileForMousePos pans (px, py) = let (mx, my) = mapPos pans in
                                (floor $ (px - mx) / 16, floor $ (py - my) / 16)

-- doPan :: (Int,Int) -> PanState -> Form -> (PanState,Form)
doPan pos pans ts form = case pans of -- FIXME annoying that I need to rebuild from ts
                              PanListen _ -> (PanFrom pos, form)
                              PanIgnore p0 -> (PanIgnore p0, form)
                              PanFrom p0 -> (PanListen pos, uncurry move pos $ drawMapView ts)

-- stateful automaton
-- mouseOnMapStep :: (Tool,(Int,Int),Bool) -> (Dict (Int,Int) Tile,PanState,Form) -> (Dict (Int,Int) Tile,PanState,Form)
stepMouseOnMap (tool, pos, isDown) (ts, pans, form) =
               if isDown && pos `isWithin` form
                  then if tool.shortName == "Pan"
                          then let (pans', form') = doPan pos pans ts form in
                               (ts, pans', form')
                          else let ts' = tool.apply ts $ tileForMousePos pans pos in
                               (ts',
                                pans,
                                drawMapView ts')
                  else (ts, pans, form)

-- defaultMapAutomaton :: Automaton (Tool,(Int,Int),Bool) (Dict (Int,Int) Tile,PanState,Form)
defaultMapAutomaton = init (defaultMapGrid, PanListen (0, 0), drawMapView defaultMapGrid)
                           stepMouseOnMap

-- defaultToolView :: Signal Tool -> Signal Form

defaultToolView tool = lift (\(ts, pans, form) -> form)
                            $ Automaton.run defaultMapAutomaton
                                            $ lift3 (\a b c -> (a, b, c)) tool Mouse.position Mouse.isDown

-- defaultToolView = constant $ drawMapView defaultMapGrid

-- defaultToolView (0, 0) :: Signal Form

-- draggable <~ defaultToolView (0, 0) :: Signal (Automaton (Bool,(Int,Int)) Form)
-- Automaton.run <~ (draggable <~ defaultToolView (0, 0)) :: Signal (Signal (Bool, Int) -> Signal Form)

-- dragMapView initPos = (Automaton.run <~ (draggable <~ defaultToolView (0, 0)))
--                     ~ lift2 (\a b -> (a, b)) Mouse.isDown Mouse.position

-- lift2 Automaton.run :: Signal (Automaton (Bool,Int) Form) -> Signal (Signal (Bool,Int)) -> Signal (Signal Form)

-- leftPane = (\view -> color blue $ collage 500 500 [view]) <~ defaultToolView

{- right pane -}
-- fourSquare :: Tile -> (Int,Int) -> Dict (Int,Int) Tile
fourSquare t p = Dict.fromList [(p, t TopLeft), ((fst p, snd p + 1), t TopRight),
                                ((fst p + 1, snd p), t BottomLeft), ((fst p + 1, snd p + 1), t BottomRight)]

applyFour t ts p = Dict.union (fourSquare t p) ts

-- apply :: Dict (Int,Int) Tile -> (Int,Int) -> Dict (Int,Int) Tile
pan = { name = "Pan", shortName = "Pan", size = (0-1, 0-1), apply ts p = ts }
controls = [pan]

zones = [{ name = "Residential", shortName = "R", size = (2, 2),
           apply = applyFour Residential },
         { name = "Commercial", shortName = "C", size = (2, 2),
           apply = applyFour Commercial },
         { name = "Industrial", shortName = "I", size = (2, 2),
           apply = applyFour Industrial }]

services = [{ name = "Police Station", shortName = "PD", size = (2, 2),
              apply = applyFour PoliceDept },
            { name = "Fire Department", shortName = "FD", size = (2, 2),
              apply = applyFour FireDept }]

transports = [{ name = "Road", shortName = "Rd", size = (1, 1),
                apply ts p = insert p Road ts },
              { name = "Railroad", shortName = "RR", size = (1, 1),
                apply ts p = insert p Railroad ts }]

-- [[Tool]] -> [[(Element, Signal Bool)]]
-- unzip into [[Element]] and [[Signal Bool]]
-- flow out the elements
-- concat, zip together [Signal Bool] and [Tool]
-- [(Tool, Signal Bool)] -> Signal Tool

-- toolRow :: [Tool] -> (Element,[(Tool,Signal Bool)])
toolRow tools = let (buttons, presses) = unzip $ map (Input.button . .shortName) tools in
                (flow right buttons, zip tools presses)

-- \tl -> snd <~ tl :: (Tool,Signal Bool) -> Signal Bool
-- signalize :: [(Tool,Signal Bool)] -> [Signal (Tool,Bool)]
signalize tls = map (\(tool, press) -> (\prss -> (tool, prss)) <~ press) tls

-- currentTool :: [(Tool,Signal Bool)] -> Signal Tool
currentTool tls = lift fst $ merges . map (keepIf snd (pan, True)) $ signalize tls

-- elementRows :: [Element]
-- tlss :: [[(Tool,Signal Bool)]]
-- toolBox :: [[Tool]] -> (Element,Signal Tool)
toolBox tlrs = let (elementRows, tlss) = unzip $ map toolRow tlrs in
               (flow down elementRows, currentTool $ concat tlss)

defaultToolBox = toolBox [controls, zones, services, transports]

main = (\tool -> flow right $ [fst defaultToolBox, plainText tool.name]) <~ snd defaultToolBox

--rightPane = flow down elementRows

--main = (\l -> flow right [l, rightPane]) <~ leftPane
--}