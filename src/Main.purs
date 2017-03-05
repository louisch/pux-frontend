module Main where


import App.Effects (AppEffects)
import App.Routes (match)
import App.Layout (Action(..), State, view, update)
import Control.Bind ((=<<))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Var (($=))
import DOM (DOM)
import Prelude ((<>), Unit, bind, pure)
import Pux (App, Config, CoreEffects, renderToDOM, start)
import Pux.Devtool (Action, start) as Pux.Devtool
import Pux.Router (sampleUrl)
import Signal ((~>))
import Signal (Signal) as S
import Signal.Channel (CHANNEL, Channel, channel, send, subscribe) as S
import WebSocket (WEBSOCKET, Connection(..), URL(URL), newWebSocket, runMessage, runMessageEvent)


connectWS :: S.Channel Action -> String -> forall eff. Eff (channel :: S.CHANNEL, err :: EXCEPTION, ws :: WEBSOCKET | eff) Unit
connectWS channel url = do
  connection@(Connection ws) <- newWebSocket (URL url) []
  ws.onmessage $= \event -> do
      let received = runMessage (runMessageEvent event)
      log ("Connected to " <> url)
      S.send channel ((ReceiveWSData received) :: Action)


-- | App configuration
config :: forall eff. State -> Eff (channel :: S.CHANNEL, dom :: DOM, err :: EXCEPTION, ws :: WEBSOCKET | eff) (Config State Action AppEffects)
config state = do
  wsInput <- S.channel Noop
  connectWS wsInput "ws://echo.websocket.org"
  let wsSignal = S.subscribe wsInput :: S.Signal Action

  -- | Create a signal of URL changes.
  urlSignal <- sampleUrl

  -- | Map a signal of URL changes to PageView actions.
  let routeSignal = urlSignal ~> \r -> PageView (match r)

  pure
    { initialState: state
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
