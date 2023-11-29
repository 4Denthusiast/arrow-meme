{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Main (
  main
) where

import Control.Monad.Trans
import Data.Maybe
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Language.Haskell.TH
import Prelude hiding (Left,Right)
import System.Environment

rawInputs :: [(String,String)]
rawInputs = mapMaybe process $ lines $(fmap (LitE .StringL) $ runIO $ readFile "config.txt")
  where process s = let (s0,s1) = break (=='=') s in
          case s1 of
            ('=':v) -> Just (s0,v)
            [] -> Nothing

memeText :: IO String
memeText = return $ fromMaybe "Default text" $ lookup "text" rawInputs

data Direction = Up | Right | Down | Left deriving (Eq)

direction :: IO Direction
direction = return $ case lookup "direction" rawInputs of
  Just "up" -> Up
  Just "right" -> Right
  Just "down" -> Down
  Just "left" -> Left
  _ -> Right

main :: IO ()
main = do
  initGUI
  window <- windowNew
  on window deleteEvent (liftIO mainQuit >> return False)
  windowSetTitle window ""
  d <- direction
  let vertical = d == Up || d == Down
  let (w,h) = if vertical then (500,500) else (600,100)
  windowSetDefaultSize window w h
  windowPresent window 
  box <- if vertical
    then toBox <$> vBoxNew False 0
    else toBox <$> hBoxNew False 0
  containerAdd window box
  label <- labelNew . Just =<< memeText
  arrow <- drawingAreaNew
  (if d == Right || d == Down then boxPackStart else boxPackEnd) box label PackNatural 5
  containerAdd box arrow
  canvas <- toDrawable <$> drawingAreaGetDrawWindow arrow
  on arrow exposeEvent (liftIO $ paintArrow canvas d >> return False)
  widgetShowAll window
  mainGUI

paintArrow :: Drawable -> Direction -> IO ()
paintArrow canvas d = do
  (w,h) <- drawableGetSize canvas
  let (w2,h2) = (div w 2,div h 2)
  let reverse = not $ elem (lookup "reverse" rawInputs) [Nothing, Just "", Just "false"]
  let d' = if not reverse then d else case d of
        Up -> Down
        Down -> Up
        Left -> Right
        Right -> Left
  gc <- gcNew canvas
  gcSetValues gc newGCValues{lineWidth=10}
  if d == Up || d == Down
    then drawLine canvas gc (w2,0) (w2,h)
    else drawLine canvas gc (0,h2) (w,h2)
  drawLines canvas gc $ case d' of
    Up -> [(w2-50,50),(w2,0),(w2+50,50)]
    Right -> [(w-50,h2-50),(w,h2),(w-50,h2+50)]
    Down -> [(w2-50,h-50),(w2,h),(w2+50,h-50)]
    Left -> [(50,h2-50),(0,h2),(50,h2+50)]
