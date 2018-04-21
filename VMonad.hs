import XMonad
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Actions.CopyWindow
import XMonad.Config.Mate
import System.IO

-------------------
-- * Eye Candy * --
-------------------

myFocusedColor   = "#E9B96E"
myUnfocusedColor = "#768CA6" 
myBgColor        = "#2F343F"
myTextColor      = "#D3DAE3"

-------------------------
-- * Custom Commands * --
-------------------------

myRofiCmd :: String
myRofiCmd = unwords [ "rofi" 
          , "-show"     , "run" 
          , "-lines"    , "7" 
          , "-matching" , "fuzzy"
          , "-theme"    , "/home/victor/.xmonad/myrofi.rasi"
          ]

------------------------
-- * My Keybindings * --
------------------------

myKeys = [ 
  -- It is VERY important to override the restart command.
    ((myMod , xK_q), spawn "cd /home/victor/.xmonad && stack install && xmonad --restart" )
  , ((myMod , xK_d), spawn myRofiCmd)
  ]

-----------------------
-- * My Workspaces * --
-----------------------

myWorkspaces :: [String]
myWorkspaces
  = [ "1:aux"
    , "2:term"
    , "3:dev"
    , "4:web"
    , "6:doc"
    , "7:cal"
    , "8:mail"
    , "9:tmp"
    , "0:media"
    ]

ws :: Int -> String
ws i = myWorkspaces !! (i-1)

wsMedia :: String
wsMedia = ws 9

wsEmacs :: String
wsEmacs = ws 3

wsTerm :: String
wsTerm = ws 2

-------------------------
-- * My Manage Hooks * --
-------------------------

myManageHook = composeAll
    [ manageGimp
    , manageFF
    , manageMedia
    , manageEmacs
    , manageTerms
    ]
  where
    manageGimp :: ManageHook
    manageGimp = className =? "Gimp" --> doFloat

    manageFF :: ManageHook
    manageFF = (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat

    manageMedia :: ManageHook
    manageMedia = composeAll
      [ className =? c --> doShift wsMedia
      | c <- ["VLC" , "Spotify"]
      ]
   
    manageEmacs :: ManageHook
    manageEmacs = className =? "Emacs"
              --> (ask >>= doF . flip copyWindow wsEmacs)

    manageTerms :: ManageHook
    manageTerms = composeOne
      [ className =? c -?> (ask >>= doF . flip copyWindow wsTerm)
      | c <- ["Mate-terminal" , "URxvt"] 
      ]

----------------------------
-- * My modifier, Alt_R * --
----------------------------

-- We later use xmodmap to map Caps_Lock
-- to Alt_R and live happily ever after.
myMod = mod1Mask

-------------------
-- * My Config * --
-------------------

-- We receive a handle as parameter since on the main
-- function we spawn the xmobar process and need
-- to do some wiring here.
myConfig xmproc = mateConfig 
  { modMask            = myMod
  , terminal           = "mate-terminal"
  , focusedBorderColor = myFocusedColor
  , manageHook         =   manageDocks
                       <+> myManageHook
                       <+> manageHook def
  , layoutHook         = avoidStruts 
                       $ layoutHook def
  , logHook            = dynamicLogWithPP xmobarPP
                          { ppOutput  = hPutStrLn xmproc
                          , ppTitle   = xmobarColor myFocusedColor "" . shorten 70
                          , ppCurrent = xmobarColor myFocusedColor "" . wrap "[" "]"
                          , ppVisible = wrap "[" "]"
                          , ppHidden  = xmobarColor myTextColor ""
                          , ppHiddenNoWindows = xmobarColor myUnfocusedColor ""
                          }
  , handleEventHook    = handleEventHook def
                       <+> docksEventHook
  , workspaces         = myWorkspaces
  } `additionalKeys` myKeys

----------------------------
-- * Running everything * --
----------------------------

main = do mateRegister
          xmproc <- spawnPipe "xmobar"
          xmonad (myConfig xmproc)
