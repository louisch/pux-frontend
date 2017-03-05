module App.Layout where


import App.AutoComplete as AutoComplete
import App.Effects (AppEffects)
import App.NotFound as NotFound
import App.Routes (Route(Home, NotFound))
import Prelude (($), map)
import Pux (EffModel, noEffects)
import Pux.Html (Html, div, h1, text)


data Action
  = ACAction (AutoComplete.Action)
  | PageView Route
  | ReceiveWSData String
  | Noop

type State =
  { route :: Route
  , text :: AutoComplete.State
  }

init :: State
init =
  { route: Home
  , text: AutoComplete.init }

update :: Action -> State -> EffModel State Action AppEffects
update Noop state = noEffects state
update (PageView route) state = noEffects $ state { route = route }
update (ACAction action) state =
  noEffects $ state { text = AutoComplete.update action state.text }
update (ReceiveWSData received) state = noEffects state

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
