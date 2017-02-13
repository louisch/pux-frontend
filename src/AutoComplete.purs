module App.AutoComplete where

import Prelude ((<>), (/=), map)
import Data.Maybe (Maybe(..))
import Data.Array (find)
import Data.String (Pattern(..), contains, takeWhile)
import Pux.Html (Html, div, text, textarea, p, h3)
import Pux.Html.Events (FormEvent, onChange)
import Pux.Html.Attributes (value)

data Action = NewText String String

type State = { currentText :: String, completionText :: String }

init :: State
init = {currentText: "", completionText: ""}

update :: Action -> State -> State
update (NewText newText newCompletion) _ =
  {currentText: newText, completionText: newCompletion}

view :: State -> Html Action
view {currentText, completionText} =
  div
    []
    ([ p [] [text "Current List of Completion targets"]] <>
     (map (\str -> p [] [text str]) getTargets) <>
     [ h3 [] [text "Completion box"] ] <>
     [ textarea [value currentText, onChange handleForm] []
     , p [] [text completionText]
     ])

handleForm :: FormEvent -> Action
handleForm { target: {value: ""}, currentTarget: _} = NewText "" ""
handleForm { target: {value: inputText}, currentTarget: _} =
  if contains (Pattern "\n") inputText then
    let strippedText = takeWhile (\c -> c /= '\n') inputText in
    case (find (contains (Pattern strippedText)) getTargets) of
      Just fullText -> NewText strippedText fullText
      Nothing       -> NewText strippedText ""
  else
    NewText inputText ""

getTargets :: Array String
getTargets = ["foo", "bar", "abc", "the tree", "123"]
