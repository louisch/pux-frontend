module App.AST where


import Prelude ((<>), ($), map, show, bind, pure)
import Pux.Html (Html, div, text)
import Data.Argonaut ((.?), class DecodeJson, decodeJson)


data Tree = Tree (Array Declaration)

data Declaration = Declaration Name AnonFunc

data AnonFunc = AnonFunc ArgSpec Exp

type ArgSpec = Array Name

data Exp = ExpApply AnonFunc (Array Arg)
         | ExpInt Int
         | ExpFunc AnonFunc

type Arg = Exp

type Name = String

class ToHtml a where
    toHtml :: forall ac . a -> Html ac

instance treeToHtml :: ToHtml Tree where
  toHtml (Tree declarations) = div [] (map toHtml declarations)

instance declarationToHtml :: ToHtml Declaration where
  toHtml (Declaration name func) = div [] [text name, toHtml func]

instance anonfuncToHtml :: ToHtml AnonFunc where
  toHtml (AnonFunc argspec exp) = div [] ((map text argspec) <> [toHtml exp])

instance expToHtml :: ToHtml Exp where
  toHtml (ExpApply func args) = div [] ([toHtml func] <> (map toHtml args))
  toHtml (ExpInt int) = text (show int)
  toHtml (ExpFunc func) = toHtml func

instance treeDecodeJson :: DecodeJson Tree where
  decodeJson json = do
    --obj <- decodeJson json :: StrMap Int
    --tree <- obj .? "tree"
    pure $ Tree []
