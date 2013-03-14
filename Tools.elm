module Tools where

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

defaultToolBox = toolBox [controls, zones, services, transports]
