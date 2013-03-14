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

data Position9 = TopLeft | TopCenter | TopRight |
                 CenterLeft | Center | CenterRight |
                 BottomLeft | BottomCenter | BottomRight

data Tile = Dirt | Water | Coast
          | Residential Position9 | Commercial Position9 | Industrial Position9
          | PoliceDept Position9 | FireDept Position9
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

                      Residential TopLeft -> cropTileset 0 15
                      Residential TopCenter -> cropTileset 1 15
                      Residential TopRight -> cropTileset 2 15

                      Residential CenterLeft -> cropTileset 3 15
                      Residential Center -> cropTileset 4 15
                      Residential CenterRight -> cropTileset 5 15

                      Residential BottomLeft -> cropTileset 6 15
                      Residential BottomCenter -> cropTileset 7 15
                      Residential BottomRight -> cropTileset 8 15

-- zipCoords :: [[Tile]] -> [[(Int,Int,Tile)]]
zipCoords rs = let rsWithX = map (\r -> zip [0..length r - 1] r) rs in
                   zipWith (\r y -> zipWith (\(x, t) y -> ((x, y), t)) r
                           $ replicate (length r) y) rsWithX [0..length rsWithX]

mapGrid rs = Dict.fromList . concat . zipCoords $ rs
defaultMapGrid = mapGrid defaultMapTiles

drawMapTiles ts = map (\((tx, ty), t) -> move (16 * tx) (16 * ty) $ tileToSprite t) $ Dict.toList ts
-- main = plainText . show $ Dict.toList defaultMapGrid
drawMapView ts = let cWidth = defaultMapWidth * 16 in
                 let cHeight = defaultMapHeight * 16 in
                 toForm (cWidth / 2, cHeight / 2) . collage cWidth cHeight $ drawMapTiles ts

{- reactive part (controller?) -}
data PanState = PanListen | PanIgnore | PanFrom (Int,Int)

-- using drag state knowledge, convert an absolute mouse pos to a tile x and y
tileForMousePos (fx, fy) (mx, my) = (floor $ (mx - fx) / 16, floor $ (my - fy) / 16)

vecAdd (x1,y1) (x2,y2) = (x1+x2,y1+y2)
vecSub (x1,y1) (x2,y2) = (x1-x2,y1-y2)
-- doPan :: Form -> (Int,Int) -> (Int,Int) -> Bool -> PanState -> (Form,(Int,Int),PanState)
doPan form formPos pos press pans = case pans of -- FIXME annoying that I need to rebuild from ts
                                         PanListen -> (form, formPos,
                                                       if | not press -> PanListen
                                                          | pos `isWithin` form -> PanFrom pos
                                                          | otherwise -> PanIgnore)
                                         PanIgnore -> (form, formPos, if press then PanIgnore else PanListen)
                                         PanFrom p0 ->
                                                 if press then (uncurry move (vecSub pos p0) form, vecAdd formPos (vecSub pos p0), PanFrom pos)
                                                          else (form, formPos, PanListen)

-- stateful automaton
-- mouseOnMapStep :: (Tool,(Int,Int),Bool) -> (Dict (Int,Int) Tile,Form,(Int,Int),PanState) -> (Dict (Int,Int) Tile,Form,(Int,Int),PanState)
stepMouseOnMap (tool, pos, press) (ts, form, formPos, pans) =
               if tool.shortName == "Pan"
                  then let (form', formPos', pans') = doPan form formPos pos press pans in
                       (ts, form', formPos', pans')

                  else if press && pos `isWithin` form
                          then let ts' = tool.apply ts $ tileForMousePos formPos pos in
                               (ts', uncurry move formPos $ drawMapView ts', formPos, PanIgnore)

                          else (ts, form, formPos, PanIgnore)

-- defaultMapAutomaton :: Automaton (Tool,(Int,Int),Bool) (Dict (Int,Int) Tile,Form,(Int,Int),PanState)
defaultMapAutomaton = init (defaultMapGrid, drawMapView defaultMapGrid, (0, 0), PanListen)
                           stepMouseOnMap

-- defaultMapPane :: Signal Tool -> Signal Form
defaultMapPane tool = lift (\(_, form, _, _) -> collage 500 500 [form])
                           $ Automaton.run defaultMapAutomaton
                                           $ lift3 (\a b c -> (a, b, c))
                                                   tool Mouse.position Mouse.isDown

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
nineSquare t (tx, ty) = Dict.fromList [((tx, ty), t TopLeft), ((tx + 1, ty), t TopCenter), ((tx + 2, ty), t TopRight),
                                       ((tx, ty + 1), t CenterLeft), ((tx + 1, ty + 1), t Center), ((tx + 2, ty + 1), t CenterRight),
                                       ((tx, ty + 2), t BottomLeft), ((tx + 1, ty + 2), t BottomCenter), ((tx + 2, ty + 2), t BottomRight)]

applyNine t ts p = Dict.union (nineSquare t p) ts

-- apply :: Dict (Int,Int) Tile -> (Int,Int) -> Dict (Int,Int) Tile
pan = { name = "Pan", shortName = "Pan", size = (0-1, 0-1), apply ts p = ts }
controls = [pan]

zones = [{ name = "Residential", shortName = "R", size = (3, 3),
           apply = applyNine Residential },
         { name = "Commercial", shortName = "C", size = (3, 3),
           apply = applyNine Commercial },
         { name = "Industrial", shortName = "I", size = (3, 3),
           apply = applyNine Industrial }]

services = [{ name = "Police Station", shortName = "PD", size = (3, 3),
              apply = applyNine PoliceDept },
            { name = "Fire Department", shortName = "FD", size = (3, 3),
              apply = applyNine FireDept }]

transports = [{ name = "Road", shortName = "Rd", size = (1, 1),
                apply ts p = insert p Road ts },
              { name = "Railroad", shortName = "RR", size = (1, 1),
                apply ts p = insert p Railroad ts }]

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

main = let (toolPane, toolS) = defaultToolBox in
       lift2 (\mapPane tool -> flow right [mapPane, toolPane, plainText tool.name]) (defaultMapPane toolS) toolS
-- main = (\tool -> flow right [defaultToolView tool, fst defaultToolBox, plainText tool.name]) <~
--}