import XMonad
import XMonad.Config.Mate
import XMonad.Util.EZConfig (additionalKeys)

-- My modifier, Alt_R
myMod = mod1Mask

-- My config
myConfig = mateConfig 
  { modMask = myMod
  , focusedBorderColor = "#00FFAA"
  } `additionalKeys` myKeys


myKeys = [ 
  -- It is VERY important to override the restart command.
    ((myMod , xK_q), spawn "cd /home/victor/.xmonad && stack install && xmonad --restart" )
  ]


-- Registering and launching
main = mateRegister 
    >> xmonad myConfig -- we need 'xmonad' here because we want CLI argument parsing.
