import XMonad
import XMonad.Config.Mate
import XMonad.Util.EZConfig (additionalKeys)

-- My modifier, Alt_R
myMod = mod1Mask

-- My config
myConfig = mateConfig 
  { modMask = myMod
  } `additionalKeys` myKeys


myKeys 
  = [ ((myMod , xK_q), spawn "cd /home/victor/.xmonad && stack install && vmonad --restart" )
    ]
-- Registering and launching
main = mateRegister 
    >> xmonad myConfig 
