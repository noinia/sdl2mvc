{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module SDL2MVC
  ( main
  ) where

import           Control.Concurrent.Async (mapConcurrently_, withAsync, uninterruptibleCancel)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as Queue
import           Control.Lens
import           Debug.Trace
import           Diagrams hiding (Render)
import           Diagrams.Backend.Cairo
import           Diagrams.Prelude hiding (Render)
import           Effectful
import           GHC.Natural
import           Linear
import qualified SDL
import           SDL2MVC.App
import           SDL2MVC.Cairo
import           SDL2MVC.Framework
import           SDL2MVC.Reaction
import           SDL2MVC.Render


import qualified GI.Cairo.Render as Cairo

--------------------------------------------------------------------------------
-- * Model


data MyModel = MyModel { _mousePosition :: !(Maybe (Point V2 Int))
                       }
             deriving (Show,Eq)

makeLenses ''MyModel

defaultModel :: MyModel
defaultModel = MyModel { _mousePosition = Nothing
                       }


--------------------------------------------------------------------------------
-- * Controller


data MyAction = RenderAction Render
              | SDLEvent SDL.Event
              | Skip
              deriving (Show,Eq)


----------------------------------------


myHandler app model = \case
  RenderAction renderAct -> handleRender app model renderAct
  SDLEvent e                   -> case SDL.eventPayload e of
    SDL.MouseMotionEvent mouseData -> let p = fromIntegral <$> SDL.mouseMotionEventPos mouseData
                                      in (model&mousePosition ?~ p)
                                         <# do print p
                                               pure $ Continue (RenderAction Render)
    _                              -> noEff model
    -- SDL.WindowShownEvent _         -> model <# (pure $ Continue (RenderAction Render))
    -- SDL.WindowExposedEvent _       -> model <# (pure $ Continue (RenderAction Render))
    -- SDL.WindowGainedKeyboardFocusEvent _  -> model <# (pure $ Continue (RenderAction Render))

  Skip                   -> noEff model



--------------------------------------------------------------------------------
-- * View


headerHeight = 10
footerHeight = 20

paneWidth    = 100
toolBarWidth = 16


diagramDraw            :: MyModel -> V2 Int -> Diagram Cairo
diagramDraw model dims = drawCursor (model^.mousePosition)
                      <> blankCanvas dims

  -- drawCursor (model^.mousePosition)



drawCursor :: Maybe (Point V2 Int) -> Diagram Cairo
drawCursor = \case
  Nothing -> mempty
  Just p  -> traceShow ("cursor",p) $
    unitCircle & fc red
                        -- & moveTo (fromIntegral <$> p)



-- userInterface                 :: model -> V2 Int -> Diagram Cairo
-- userInterface model  (V2 w h) = vcat [ header   & sized (dims $ V2 w headerHeight)
--                                      , mainArea & centerXY
--                                                  & sized (dims $ V2 w h')
--                                       , footer   & sized (dims $ V2 w footerHeight)
--                                       ]
--                                  & centerXY

--   where
--     h' = h - headerHeight - footerHeight
--     w' = w - toolBarWidth - paneWidth
--     canvasSize = V2 w' h'


--     header   = rect 1 1 & fc red
--                         & lcA transparent

--       -- mconcat [ rect w headerHeight & fc red
--       --                                        & lcA transparent
--       --                  -- , text "menu" & fc black
--       --                  ]

--     mainArea = hcat [ toolBar        & centerY
--                                      & sized (mkHeight $ fromIntegral h')
--                     , (canvas model canvasSize) & sized (dims $ fmap fromIntegral canvasSize)
--                                      & showTrace
--                     , showTrace pane
--                     ]

--     footer   = mconcat [ text "footer" & fc black
--                        , rect w (fromIntegral footerHeight) & fc green
--                                                             & lcA transparent
--                        ]

--     toolBar = vsep 2 [ (text (show i) `atop` item)
--                        & sized (mkWidth $ fromIntegral toolBarWidth)
--                      | i <- [1..10]]
--     item = rect 1 1 & fc white
--                     & sc green


--     pane = rect 1 1 & fc yellow


  -- vcat [ header
  --                        , mainArea
  --                        , footer
  --                        ]
  -- where
  --   header = rect (mkHeight 1) & fc blue

  --   footer = rect (mkHeight 1) & fc red


  --   toolBar = rect (mkHeight 1) & fc green
  --   pane    = rect (mkHeight 1) & fc yellow

-- canvas       :: model -> Diagram Cairo
canvas model dims = mconcat [ drawGeometries model
                            , blankCanvas dims
                            ] & bg white

drawGeometries model = vcat [ circle 1 & fc blue
                            , rect 2 1 & fc green
                            ]

blankCanvas                               :: V2 Int -> Diagram Cairo
blankCanvas (fmap fromIntegral -> V2 w h) = rect w h & fc  white
                                                     & lcA transparent


-- -- | draw on SDL texture with Render monad from Cairo
-- withCairoTexture     :: SDL.Texture -> Render () -> IO ()
-- withCairoTexture t m = withCairoTexture' t (\s -> renderWith s m)


--------------------------------------------------------------------------------

-- cairoDraw _ _ =



--------------------------------------------------------------------------------


  -- do SDL.rendererDrawColor renderer SDL.$= V4 0 0 255 255
  --                      SDL.drawRect renderer (Just $ SDL.Rectangle origin (V2 200 100))
  --                      pure Skip


-- withRenderer :: Handler m model action'
--              -> App m model action -> model -> Either Render action -> Handler m model action'
-- withRenderer handler

-- withDefaultRender :: Handler m model action -> App m model action -> Handler m model action
-- withDefaultRender handler app model =


--------------------------------------------------------------------------------

--------------------------------------------------------------------------------


myDraw               :: MyModel -> View IO MyAction
myDraw model texture =
  -- do
  --   renderDiagramTo texture $ diagramDraw model
  --   pure Skip
  Skip <$ case fmap fromIntegral <$> model^.mousePosition of
    Nothing -> print "cursor outside screen"
    Just (P (V2 x y)) -> do print ("mousepos",x,y)
                            SDL.TextureInfo _ _ w h <- SDL.queryTexture texture
                            withCairoTexture texture $ do
                                    Cairo.setSourceRGB 1 1 1
                                    Cairo.rectangle 0 0 (fromIntegral w) (fromIntegral h)
                                    Cairo.fill
                                    Cairo.setSourceRGB 1 0.5 0
                                    Cairo.arc x y 2 0 (2*pi)
                                    -- Cairo.rectangle x y 20 20
                                    Cairo.fill


--------------------------------------------------------------------------------

main :: IO ()
main = runApp $
       AppConfig
         { _appModel        = defaultModel
         , _handler         = myHandler
         , _startupAction   = Skip
         , _liftSDLEvent    = withDefaultSDLEvents SDLEvent
         , _liftRenderEvent = RenderAction
         , _appRender       = myDraw
         }
