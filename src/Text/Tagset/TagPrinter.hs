module Text.Tagset.TagPrinter
( showTag
) where

import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust)

import Text.Tagset.Data ( Label, Tagset, tagView, TagView(NKJPView, MorphosView)
                        , ruleDefs, Rule(Rule) )

showTag :: Tagset -> Label -> T.Text
showTag tagset label =
    let values = tagValues tagset label in 
    case tagView tagset of
        NKJPView ->
            T.intercalate (T.singleton ':') $ catMaybes values
        MorphosView -> (fromJust $ head values)
            `T.append` T.singleton '.'
            `T.append` T.concat [ case value of
                                    Just x -> x
                                    -- Nothing -> T.singleton '_'
                                    Nothing -> T.empty
                              | value <- tail values ]

tagValues :: Tagset -> Label -> [Maybe T.Text]
tagValues tagset label =
    let rule = head $ filter (matchRule label) $ ruleDefs tagset
    in  ruleValues label rule 

matchRule :: Label -> Rule -> Bool
matchRule label (Rule pred _) =
    let labelMap = Map.fromList label
    in  all id [ Map.lookup (T.pack attr) labelMap == Just (T.pack value)
               | (attr, value) <- pred ]

ruleValues :: Label -> Rule -> [Maybe T.Text]
ruleValues label (Rule pred action) =
    let labelMap = Map.fromList label
        atts = [T.pack attr | (attr, _) <- pred] ++
               [T.pack attr | (attr, _) <- action]
    in  [Map.lookup attr labelMap | attr <- atts]
