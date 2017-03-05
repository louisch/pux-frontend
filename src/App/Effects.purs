module App.Effects where


import DOM (DOM)
import WebSocket (WEBSOCKET)


type AppEffects = (dom :: DOM, ws :: WEBSOCKET)

