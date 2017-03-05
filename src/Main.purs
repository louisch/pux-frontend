module Main where


import App.Effects (AppEffects)
import App.Routes (match)
import App.Layout (Action(..), State, view, update)
import App.WebSocket (createSocketSignal)
import Control.Bind ((=<<))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)
import Prelude (bind, pure)
import Pux (App, Config, CoreEffects, renderToDOM, start)
import Pux.Devtool (Action, start) as Pux.Devtool
import Pux.Router (sampleUrl)
import Signal ((~>))
import Signal.Channel (CHANNEL)
import WebSocket (WEBSOCKET)


-- | App configuration
config :: forall eff. State -> Eff (channel :: CHANNEL, dom :: DOM, err :: EXCEPTION, ws :: WEBSOCKET | eff) (Config State Action AppEffects)
config state = do
  {wsSignal: wsSignal, sendFunc: sendFunc} <- createSocketSignal "ws://echo.websocket.org"

  -- | Create a signal of URL changes.
  urlSignal <- sampleUrl

  -- | Map a signal of URL changes to PageView actions.
  let routeSignal = urlSignal ~> \r -> PageView (match r)

  pure { initialState: state { sendFunc = sendFunc }
       , update: update
       , view: view
       , inputs: [routeSignal, wsSignal] }

-- | Entry point for the browser.
main :: State -> Eff (CoreEffects AppEffects) (App State Action)
main state = do
  app <- start =<< config state
  renderToDOM "#app" app.html
  -- | Used by hot-reloading code in support/index.js
  pure app

-- | Entry point for the browser with pux-devtool injected.
debug :: State -> Eff (CoreEffects AppEffects) (App State (Pux.Devtool.Action Action))
debug state = do
  app <- Pux.Devtool.start =<< config state
  renderToDOM "#app" app.html
  -- | Used by hot-reloading code in support/index.js
  pure app
