module Main where

import Automaton
import Tiles
import MapPane
import Tools

defaultMapWidth = 10
defaultMapHeight = 10

defaultMapTiles = replicate defaultMapHeight
                            $ replicate defaultMapWidth Dirt

main = let (toolPane, toolS) = defaultToolBox in
       lift2 (\mapPane tool -> flow right [mapPane, toolPane, plainText tool.name])
             (defaultMapPane defaultMapTiles toolS) toolS

-- main = (\tool -> flow right [defaultToolView tool, fst defaultToolBox, plainText tool.name]) <~
--}