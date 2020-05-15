import XMonad
import qualified XMonad.StackSet as SS
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns

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
          , "-theme"    , "Arc-Dark"
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
    ((myMod , xK_q), spawn "cd /home/victor/.xmonad && stack install && stack exec xmonad-x86_64-linux -- --restart" )
  -- Launch rofi
  , ((myMod , xK_d), spawn myRofiCmd)
  -- Swap focused physical screens
  , ((myMod , xK_e),              myCycleScreen (windows . SS.view))
  , ((myMod .|. shiftMask, xK_e), myCycleScreen (windows . SS.shift))
  -- Toggle struts
  , ((myMod , xK_b) , sendMessage ToggleStruts)
  -- Swap keyboard layouts
  , ((myMod , xK_i) , spawn "/home/victor/repos/vsr/scripts/kbd-layout-switch")
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
  = [ "1"
    , "2"
    , "3"
    , "4"
    , "5"
    , "6"
    , "7"
    , "8"
    , "9"
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
    [ manageFF
    , manageBlueman
    , manageVolumeCtl
    , manageMedia
    ]
  where
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
   
    -- Shifting emacs and terminals automatically is more confusing
    -- than it is helpful; nevermind.
    -- 
    -- manageEmacs :: ManageHook
    -- manageEmacs = className =? "Emacs"
    --           --> (ask >>= doF . flip copyWindow wsEmacs)

    -- manageTerms :: ManageHook
    -- manageTerms = composeAll
    --   [ className =? c --> (ask >>= doF . flip copyWindow wsTerm)
    --   | c <- ["Mate-terminal" , "URxvt"] 
    --   ]

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
myMod = mod4Mask

--------------------
-- * My Layouts * --
--------------------

myLayouts = smartBorders . avoidStruts
          $ tiled ||| Mirror tiled ||| threecol ||| Full
  where 
    nmaster = 1     -- default number of windows on master pane
    delta   = 3/100 -- percentage to increment when resizing panes
    ratio   = 1/2   -- proportion of screen ocupied by master

    tiled    = Tall nmaster delta ratio
    threecol = ThreeColMid nmaster delta ratio 
    

-------------------
-- * My Config * --
-------------------

-- We receive a handle as parameter since on the main
-- function we spawn the xmobar process and need
-- to do some wiring here.
myConfig xmproc = (mateConfig 
    { modMask            = myMod
    , terminal           = "mate-terminal"
    , focusedBorderColor = myFocusedColor
    , manageHook         =   manageDocks
                         <+> myManageHook
                         <+> manageHook def
    , layoutHook         = myLayouts
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
