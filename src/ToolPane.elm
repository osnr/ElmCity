module ToolPane where

-- toolRow :: [Tool] -> (Element,[(Tool,Signal Bool)])
toolRow tools = let (buttons, presses) = unzip $ map (Input.button . .shortName) tools in
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
