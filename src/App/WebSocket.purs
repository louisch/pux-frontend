module App.WebSocket where


import App.Layout (Action(Noop, ReceiveWSData))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Var (($=))
import Prelude (Unit, (<<<), bind, pure)
import Signal (Signal)
import Signal.Channel (CHANNEL, channel, send, subscribe)
import WebSocket (WEBSOCKET, Connection(Connection), Message, URL(URL), newWebSocket, runMessageEvent, runMessage)


createSocketSignal :: forall eff1 eff2 .
  String -> Eff ( channel :: CHANNEL, ws :: WEBSOCKET, err :: EXCEPTION | eff1 )
                { wsSignal :: Signal Action,
                  sendFunc :: Message -> Eff ( ws :: WEBSOCKET, err :: EXCEPTION | eff2 ) Unit }
createSocketSignal url = do
  chan <- channel Noop
  Connection socket <- newWebSocket (URL url) []
  socket.onmessage $= send chan <<< ReceiveWSData <<< runMessage <<< runMessageEvent
  pure {wsSignal: subscribe chan, sendFunc: socket.send}
