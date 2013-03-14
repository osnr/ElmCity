module City where

import Util
import ToolPane
import MapPane
import Tiles

defaultMapWidth = 10
defaultMapHeight = 10

defaultMapTiles = replicate defaultMapHeight
                            $ replicate defaultMapWidth Dirt

main = let (toolPane, toolS) = defaultToolBox in
       lift2 (\mp tool -> flow right [mp, toolPane, plainText tool.name])
             (mapPane defaultMapTiles toolS) toolS
