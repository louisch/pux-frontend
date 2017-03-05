module App.Layout where


import App.AutoComplete as AutoComplete
import App.Effects (AppEffects)
import App.NotFound as NotFound
import App.Routes (Route(Home, NotFound))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Prelude (Unit, ($), (*>), map, pure, unit)
import Pux (CoreEffects, EffModel, noEffects)
import Pux.Html (Html)
import Pux.Html.Elements (div, h1, text)
import WebSocket (Message)


data Action
  = ACAction (AutoComplete.Action)
  | PageView Route
  | ReceiveWSData String
  | SendWSData Message
  | Noop

type State =
  { route :: Route
  , text :: AutoComplete.State
  , sendFunc :: Message -> Eff (CoreEffects AppEffects) Unit
  , received :: String
  }

init :: State
init =
  { route: Home
  , text: AutoComplete.init
  , sendFunc: \_ -> pure unit
  , received: ""
  }

update :: Action -> State -> EffModel State Action AppEffects
update Noop state = noEffects state
update (PageView route) state = noEffects $ state { route = route }
update (ACAction action) state =
  noEffects $ state { text = AutoComplete.update action state.text }
update (ReceiveWSData received) state = noEffects $ state { received = received }
update (SendWSData toSend) state =
  { state: state
  , effects: [liftEff (state.sendFunc toSend) *> pure Noop]
  }

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
  div
    []
    [ map ACAction $ AutoComplete.view state.text
    ]
