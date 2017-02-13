module App.Layout where

import App.Counter as Counter
import App.AutoComplete as AutoComplete
import App.NotFound as NotFound
import App.Routes (Route(Home, NotFound))
import Prelude (($), map)
import Pux.Html (Html, div, h1, p, text)

data Action
  = CounterAction (Counter.Action)
  | ACAction (AutoComplete.Action)
  | PageView Route

type State =
  { route :: Route
  , count :: Counter.State
  , text :: AutoComplete.State
  }

init :: State
init =
  { route: NotFound
  , count: Counter.init
  , text: AutoComplete.init
  }

update :: Action -> State -> State
update (PageView route) state = state { route = route }
update (CounterAction action) state = state { count = Counter.update action state.count }
update (ACAction action) state = state { text = AutoComplete.update action state.text }

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "Test" ]
    , p [] [ text "Hello World" ]
    , case state.route of
        Home -> mainView state
        NotFound -> NotFound.view state
    ]

mainView :: State -> Html Action
mainView state =
  div
    []
    [ map CounterAction $ Counter.view state.count
    , map ACAction $ AutoComplete.view state.text
    ]
