import XMonad
import qualified XMonad.StackSet as SS
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys, removeKeys)
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


------------------------------
-- * Custom Functionality * --
------------------------------

-- Send focus to the next screen. We do so by grabing the window
-- stack and checking whether there is a next screen to look at.
myCycleScreen :: (WorkspaceId -> X ()) -> X ()
myCycleScreen act = do
  visibleWs <- gets (SS.visible . windowset)
  case visibleWs of
    []       -> return ()
    (next:_) -> act (screenId next)
 where
   screenId :: SS.Screen i l a sid sd -> i
   screenId = SS.tag . SS.workspace
            

------------------------
-- * My Keybindings * --
------------------------

myKeys :: [((KeyMask, KeySym), X ())] 
myKeys = [ 
  -- It is VERY important to override the restart command.
    ((myMod , xK_q), spawn "cd /home/victor/.xmonad && stack install && xmonad --restart" )
  , ((myMod , xK_d), spawn myRofiCmd)
  -- Swap focused physical screens
  , ((myMod , xK_e),              myCycleScreen (windows . SS.view))
  , ((myMod .|. shiftMask, xK_e), myCycleScreen (windows . SS.shift))
  ]

myRemovedKeys :: [(KeyMask, KeySym)]
myRemovedKeys = [
  -- I like to use M-n and M-p on my emacs!
    (myMod , xK_n)
  , (myMod , xK_p)
  -- I don't like 'switch to screen id x', I much rather have
  -- switch to the next one.
  , (myMod , xK_w)
  , (myMod , xK_e)
  , (myMod , xK_r)
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
    , "5:doc"
    , "6:cal"
    , "7:mail"
    , "8:tmp"
    , "9:media"
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
    , manageBlueman
    , manageVolumeCtl
    , manageMedia
    , manageEmacs
    , manageTerms
    ]
  where
    manageGimp :: ManageHook
    manageGimp = className =? "Gimp" --> doFloat

    manageFF :: ManageHook
    manageFF = (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat

    manageBlueman :: ManageHook
    manageBlueman = (className =? "Blueman-manager") --> doFloat

    manageVolumeCtl :: ManageHook
    manageVolumeCtl = (className =? "Mate-volume-control") --> doFloat

    manageMedia :: ManageHook
    manageMedia = composeAll $
      [ className =? c --> doShift wsMedia
      | c <- ["vlc"]] ++
      [ title =? "Spotify" --> doShift wsMedia ]
   
    manageEmacs :: ManageHook
    manageEmacs = className =? "Emacs"
              --> (ask >>= doF . flip copyWindow wsEmacs)

    manageTerms :: ManageHook
    manageTerms = composeAll
      [ className =? c --> (ask >>= doF . flip copyWindow wsTerm)
      | c <- ["Mate-terminal" , "URxvt"] 
      ]

----------------------------
-- * My modifier, Alt_R * --
----------------------------

-- We later use xmodmap to map Caps_Lock
-- to Alt_R and live happily ever after.
--
-- My ThinkPad came with ISO_Level3_Shift (aka AltGR) in the
-- mod5 group;
-- Seems like this was a problem with the layout. 
-- (English US with euro on 5) has altgr mapped to ISO_Level3_Shift
-- (English US) has altgr maped to Alt_R
--
myMod = mod1Mask

-------------------
-- * My Config * --
-------------------

-- We receive a handle as parameter since on the main
-- function we spawn the xmobar process and need
-- to do some wiring here.
myConfig xmproc 
  = fullscreenSupport 
  $ (mateConfig 
    { modMask            = myMod
    , terminal           = "mate-terminal"
    , focusedBorderColor = myFocusedColor
    , manageHook         =   manageDocks
                         <+> myManageHook
                         <+> manageHook def
    , layoutHook         = avoidStruts
                         . smartBorders
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
    } `removeKeys`     myRemovedKeys
    ) `additionalKeys` myKeys

----------------------------
-- * Running everything * --
----------------------------

main = do mateRegister
          xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
          xmonad (myConfig xmproc)
