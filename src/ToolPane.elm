module ToolPane where

import Tiles

-- fourSquare :: Tile -> (Int,Int) -> Dict (Int,Int) Tile
nineSquare t (tx, ty) = Dict.fromList [((tx, ty), t TopLeft), ((tx + 1, ty), t TopCenter), ((tx + 2, ty), t TopRight),
                                       ((tx, ty + 1), t CenterLeft), ((tx + 1, ty + 1), t Center), ((tx + 2, ty + 1), t CenterRight),
                                       ((tx, ty + 2), t BottomLeft), ((tx + 1, ty + 2), t BottomCenter), ((tx + 2, ty + 2), t BottomRight)]

applyNine t ts p = Dict.union (nineSquare t p) ts

applyOne t ts p = Dict.insert p t ts

-- apply :: Dict (Int,Int) Tile -> (Int,Int) -> Dict (Int,Int) Tile
pan = { name = "Pan", shortName = "Pan", icon = image 34 34 "../sprites/icqry.png",
        size = (0-1, 0-1), apply ts p = ts }
controls = [pan]

zones = [{ name = "Residential", shortName = "R", icon = image 34 50 "../sprites/icres.png",
           size = (3, 3), apply = applyNine Residential },
         { name = "Commercial", shortName = "C", icon = image 34 50 "../sprites/iccom.png",
           size = (3, 3), apply = applyNine Commercial },
         { name = "Industrial", shortName = "I", icon = image 34 50 "../sprites/icind.png",
           size = (3, 3), apply = applyNine Industrial }]

services = [{ name = "Police Station", shortName = "PD", icon = image 34 34 "../sprites/icpol.png",
              size = (3, 3), apply = applyNine PoliceDept },
            { name = "Fire Department", shortName = "FD", icon = image 34 34 "../sprites/icfire.png",
              size = (3, 3), apply = applyNine FireDept }]

transports = [{ name = "Road", shortName = "Rd", icon = image 56 24 "../sprites/icroad.png",
                size = (1, 1), apply = applyOne Road },
              { name = "Railroad", shortName = "RR", icon = image 56 24 "../sprites/icrail.png",
                size = (1, 1), apply = applyOne Railroad }]

toolButton tool = let (button, press) = Input.button tool.shortName in
                  (layers [tool.icon,
                           opacity 0.5 $ size (widthOf tool.icon) (heightOf tool.icon) button],
                   press)

-- toolRow :: [Tool] -> (Element,[(Tool,Signal Bool)])
toolRow tools = let (buttons, presses) = unzip $ map toolButton tools in
                (flow right buttons, zip tools presses)

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
