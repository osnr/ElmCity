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
drawMapView ts = let cWidth = 20 * 16 in
                 let cHeight = 20 * 16 in
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

                  else let tPos = tileForMousePos formPos pos in
                       if press && pos `isWithin` form
                          then let ts' = tool.apply ts tPos in
                               (ts', uncurry move formPos $ drawMapView ts', formPos, PanIgnore)

                          else (ts, form, formPos, PanIgnore)

-- mapAutomaton :: [[Tile]] -> Automaton (Tool,(Int,Int),Bool) (Dict (Int,Int) Tile,Form,(Int,Int),PanState)
mapAutomaton rs = let g = mapGrid rs in
                  Automaton.init (g, drawMapView g, (0, 0), PanListen)
                                 stepMouseOnMap

-- mouseMapView :: [[Tile]] -> Signal Tool -> Signal (Form,(Int,Int))
mouseMapView rs toolS = lift (\(_, form, formPos, _) -> (form, formPos))
                             $ Automaton.run (mapAutomaton rs)
                                             $ lift3 (\a b c -> (a, b, c))
                                                     toolS Mouse.position Mouse.isDown

-- hoverLayer :: (Int,Int) -> (Int,Int) -> Form -> (Int,Int) -> Element
hoverLayer (tw, th) (mx, my) form (fx, fy) =
           let (w, h) = (tw * 16, th * 16) in
           let roundAndRecenter fz mz = 16 * ((mz - fz) `div` 16) + fz in
           opacity 0.5 $ collage 500 500
                                 [move (roundAndRecenter fx mx) (roundAndRecenter fy my)
                                       $ filled gray $ rect w h (w/2, h/2)]

hoverAndCollage tool pos (form, formPos) = let view = collage 500 500 [form] in
                                           if tool.name /= "Pan" && pos `isWithin` form
                                              then layers [view, hoverLayer tool.size pos form formPos]
                                                   else view

-- mapPane :: [[Tile]] -> Signal Tool -> Signal Element
mapPane rs toolS = hoverAndCollage <~ toolS ~ Mouse.position ~ (mouseMapView rs toolS)
