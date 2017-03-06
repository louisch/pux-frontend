module App.Layout where


import App.AST (Tree(Tree))
import App.AutoComplete as AutoComplete
import App.Editor (view) as Editor
import App.Effects (AppEffects)
import App.NotFound as NotFound
import App.Routes (Route(Home, NotFound))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Argonaut (Json, jsonParser)
import Data.Either (Either(Left, Right))
import Prelude (Unit, ($), (*>), map, pure, unit, show)
import Pux (CoreEffects, EffModel, noEffects)
import Pux.Html (Html)
import Pux.Html.Elements (button, div, h1, text)
import Pux.Html.Events (MouseEvent, onClick)
import WebSocket (Message(Message))


data Action
  = ACAction (AutoComplete.Action)
  | PageView Route
  | ReceiveWSData String
  | SendWSData Message
  | Connect MouseEvent
  | Noop

type State =
  { route :: Route
  , text :: AutoComplete.State
  , sendFunc :: Message -> Eff (CoreEffects AppEffects) Unit
  , received :: Either String Json
  , ast :: Tree
  }

init :: State
init =
  { route: Home
  , text: AutoComplete.init
  , sendFunc: \_ -> pure unit
  , received: Left ""
  , ast: Tree []
  }

update :: Action -> State -> EffModel State Action AppEffects
update Noop state = noEffects state
update (PageView route) state = noEffects $ state { route = route }
update (ACAction action) state =
  noEffects $ state { text = AutoComplete.update action state.text }
update (ReceiveWSData received) state = noEffects $ handleReceive state received
update (SendWSData toSend) state =
  { state: state
  , effects: [liftEff (state.sendFunc toSend) *> pure Noop]
  }
update (Connect ev) state = update (SendWSData $ Message "iam louis") state

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "Haskell Editor" ]
    , case state.route of
        Home -> mainView state
        NotFound -> NotFound.view state
    ]

mainView :: State -> Html Action
mainView state =
  let textJson = case state.received of
                 Left err -> err
                 Right json -> show json in
  div
    []
    [ map ACAction $ AutoComplete.view state.text
    , button [onClick Connect] [text "Connect"]
    --, Editor.view state.ast
    , div [] [text $ textJson]
    ]

handleReceive :: State -> String -> State
handleReceive state received =
  state { received = jsonParser received }

