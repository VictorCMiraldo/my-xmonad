import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Config.Mate
import System.IO

-- My modifier, Alt_R
myMod = mod1Mask

-- My config
myConfig xmproc = mateConfig 
  { modMask            = myMod
  , terminal           = "mate-terminal"
  , focusedBorderColor = "#729FCF"
  , manageHook         =   manageDocks
                       <+> myManageHook
                       <+> manageHook def
  , layoutHook         = avoidStruts 
                       $ layoutHook def
  , logHook            = dynamicLogWithPP xmobarPP
                          { ppOutput = hPutStrLn xmproc
                          , ppTitle  = xmobarColor "#729FCF" "" . shorten 70
                          }
  , handleEventHook    = handleEventHook def
                       <+> docksEventHook
  } `additionalKeys` myKeys

myKeys = [ 
  -- It is VERY important to override the restart command.
    ((myMod , xK_q), spawn "cd /home/victor/.xmonad && stack install && xmonad --restart" )
  , ((myMod , xK_d), mateRun)
  ]

myManageHook = composeAll
    [ className =? "Gimp"      --> doFloat
    , className =? "Vncviewer" --> doFloat
    ]

main = do -- mateRegister
          xmproc <- spawnPipe "xmobar"
          xmonad (myConfig xmproc)

