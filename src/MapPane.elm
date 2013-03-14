module MapPane where

import Util
import Tiles

-- zipCoords :: [[Tile]] -> [[(Int,Int,Tile)]]
zipCoords rs = let rsWithX = map (\r -> zip [0..length r - 1] r) rs in
                   zipWith (\r y -> zipWith (\(x, t) y -> ((x, y), t)) r
                           $ replicate (length r) y) rsWithX [0..length rsWithX]

mapGrid rs = Dict.fromList . concat . zipCoords $ rs

drawMapTiles ts = map (\((tx, ty), t) -> move (16 * tx) (16 * ty) $ tileToSprite t) $ Dict.toList ts

-- FIXME map width & height besides 10
drawMapView ts = let cWidth = 10 * 16 in
                 let cHeight = 10 * 16 in
                 toForm (cWidth / 2, cHeight / 2) . collage cWidth cHeight $ drawMapTiles ts

data PanState = PanListen | PanIgnore | PanFrom (Int,Int)

tileForMousePos (fx, fy) (mx, my) = ((mx - fx) `div` 16, (my - fy) `div` 16)

vecAdd (x1,y1) (x2,y2) = (x1+x2,y1+y2)
vecSub (x1,y1) (x2,y2) = (x1-x2,y1-y2)
-- doPan :: Form -> (Int,Int) -> (Int,Int) -> Bool -> PanState -> (Form,(Int,Int),PanState)
doPan form formPos pos press pans = case pans of
      PanListen -> (form, formPos,
                    if | not press -> PanListen
                       | pos `isWithin` form -> PanFrom pos
                       | otherwise -> PanIgnore)
      PanIgnore -> (form, formPos, if press then PanIgnore else PanListen)
      PanFrom p0 ->
              if press then (uncurry move (vecSub pos p0) form,
                             vecAdd formPos (vecSub pos p0),
                             PanFrom pos)
                       else (form, formPos, PanListen)

-- stepMouseOnMap :: (Tool,(Int,Int),Bool) -> (Dict (Int,Int) Tile,Form,(Int,Int),PanState) -> (Dict (Int,Int) Tile,Form,(Int,Int),PanState)
stepMouseOnMap (tool, pos, press) (ts, form, formPos, pans) =
               if tool.shortName == "Pan"
                  then let (form', formPos', pans') = doPan form formPos pos press pans in
                       (ts, form', formPos', pans')

                  else if press && pos `isWithin` form
                          then let ts' = tool.apply ts $ tileForMousePos formPos pos in
                               (ts', uncurry move formPos $ drawMapView ts', formPos, PanIgnore)

                          else (ts, form, formPos, PanIgnore)

-- mapAutomaton :: [[Tile]] -> Automaton (Tool,(Int,Int),Bool) (Dict (Int,Int) Tile,Form,(Int,Int),PanState)
mapAutomaton rs = let g = mapGrid rs in
                  Automaton.init (g, drawMapView g, (0, 0), PanListen)
                                 stepMouseOnMap

-- mapPane :: [[Tile]] -> Signal Tool -> Signal Form
mapPane rs toolS = lift (\(_, form, _, _) -> collage 500 500 [form])
                        $ Automaton.run (mapAutomaton rs)
                                        $ lift3 (\a b c -> (a, b, c))
                                                toolS Mouse.position Mouse.isDown
