module Text.Tagset.Data
( Tagset (Tagset)
, Rule (Rule)
, TagView (NKJPView, MorphosView)
, Label
, tagView
, ruleDefs
, attrDefs
, rulePred
, attsFor
, allAtts
, attrValues
, Optional
, Attr
, AttrValue
) where

import Control.Monad (liftM3)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.List (intercalate)
import Data.Binary (Binary, get, put, putWord8, getWord8)

type Pred = [(Attr, AttrValue)]
type Action = [(Attr, Optional)]
data Rule = Rule Pred Action deriving (Show)

instance Binary Rule where
    put (Rule pred action) = put (pred, action)
    get = get >>= return . uncurry Rule

data TagView = NKJPView
             | MorphosView
             deriving (Eq, Show)

instance Binary TagView where
    put NKJPView = putWord8 0
    put MorphosView = putWord8 1
    get = do
    	x <- getWord8
	case x of
            0 -> return NKJPView
            1 -> return MorphosView

data Tagset = Tagset { attrDefs :: Map.Map Attr [AttrValue]
                     , ruleDefs :: [Rule]
                     , tagView :: TagView
                     } deriving (Show)

instance Binary Tagset where
    put ts = put (attrDefs ts)
          >> put (ruleDefs ts)
          >> put (tagView ts)
    get = liftM3 Tagset get get get

type Optional = Bool
type Attr = String
type AttrValue = String

-- attsFor :: Tagset -> Pos -> [(Attr, Optional)]
-- attsFor tagset pos = (posDefs tagset) Map.! pos

rulePred :: Rule -> Pred
rulePred (Rule pred _) = pred

attsFor :: Rule -> [(Attr, Optional)]
attsFor (Rule _ atts) = atts

attrValues :: Tagset -> Attr -> [AttrValue]
attrValues tagset attr = (attrDefs tagset) Map.! attr

allAtts :: Tagset -> [Attr]
allAtts tagset = Map.keys $ attrDefs tagset

-- [(Attr AttrValue)]; using T.Text for high speed.
type Label = [(T.Text, T.Text)]
