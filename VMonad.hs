import XMonad
import XMonad.Config.Mate
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Actions.CopyWindow
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers

---------------------------
-- * My modifier, Alt_R
---------------------------

myMod = mod1Mask

--------------------------
-- * My config
--------------------------

myConfig = mateConfig 
  { modMask            = myMod
  , focusedBorderColor = "#00FFAA"
  , manageHook         = myManageHook
  , workspaces         = myWorkspaces
  } `additionalKeys` myKeys

----------------------------------
-- * Workspaces and ManageHooks
---------------------------------

myWorkspaces :: [WorkspaceId]
myWorkspaces = [ "1:main" 
               , "2:web"
               , "3:emacs"
               , "4:term"
               , "5:"
               , "6:"
               , "7:"
               , "8:"
               , "9:spotify"
               ]

data MyWorkspaces = Main | Web | Emacs | Term | Spotify

myGetWorkspace :: MyWorkspaces -> WorkspaceId
myGetWorkspace Main    = myWorkspaces !! 0
myGetWorkspace Web     = myWorkspaces !! 1
myGetWorkspace Emacs   = myWorkspaces !! 2
myGetWorkspace Term    = myWorkspaces !! 3
myGetWorkspace Spotify = myWorkspaces !! 8


myManageHook :: ManageHook
myManageHook = composeAll
  [ manageEmacs
  , manageTerm
  , manageFirefox
  , manageSpotify
  ]

manageEmacs :: ManageHook
manageEmacs = className =? "Emacs"
          --> (ask >>= doF . \w -> (copyWindow w (myGetWorkspace Emacs)))

manageTerm :: ManageHook
manageTerm = composeOne
  [ className =? c -?> doShift (myGetWorkspace Term)
  | c <- [ "mate-terminal"
         , "URxvt"
         ]]

manageFirefox :: ManageHook
manageFirefox = className =? "Firefox"
            --> (ask >>= doF . \w -> (copyWindow w (myGetWorkspace Web)))

manageSpotify :: ManageHook
manageSpotify = className =? "Spotify"
            --> doShift (myGetWorkspace Spotify)

-------------------------------------
-- * Keybindings
-------------------------------------

myKeys = [ 
  -- It is VERY important to override the restart command.
    ((myMod , xK_q), spawn "cd /home/victor/.xmonad && stack install && xmonad --restart" )
  -- Configure mate-run on xK_d
  , ((myMod , xK_d) , mateRun )
  ]


-- Registering and launching
main = mateRegister 
    >> xmonad myConfig -- we need 'xmonad' here because we want CLI argument parsing.
