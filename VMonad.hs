import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Config.Mate
import System.IO

-------------------
-- * Eye Candy * --
-------------------

myFocusedColor = "#E9B96E"
myBgColor      = "#2F343F"
myTextColor    = "#D3DAE3"

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

-------------------------
-- * My Manage Hooks * --
-------------------------

myManageHook = composeAll
    [ className =? "Gimp"      --> doFloat
    , className =? "Vncviewer" --> doFloat
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
                          { ppOutput = hPutStrLn xmproc
                          , ppTitle  = xmobarColor myFocusedColor "" . shorten 70
                          }
  , handleEventHook    = handleEventHook def
                       <+> docksEventHook
  } `additionalKeys` myKeys

----------------------------
-- * Running everything * --
----------------------------

main = do mateRegister
          xmproc <- spawnPipe "xmobar"
          xmonad (myConfig xmproc)

