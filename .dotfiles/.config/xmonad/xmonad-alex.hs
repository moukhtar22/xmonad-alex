import XMonad
import XMonad.StackSet (swapMaster, greedyView, shift)
import System.Exit

import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.SimpleFloat
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Tabbed
import XMonad.Layout.Master

import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar

import XMonad.Util.EZConfig (additionalKeysP, removeKeysP, additionalKeys, additionalMouseBindings)
import XMonad.Util.SpawnOnce

import XMonad.Actions.Submap (visualSubmap, subName)

import Data.Map (fromList)
import XMonad.Actions.GridSelect (goToSelected)


myManageHook = composeAll
    [ className =? "pavucontrol" --> doCenterFloat
    , className =? "Qemu-system-x86_64" --> doCenterFloat
    , className =? "feh" --> doCenterFloat
    -- For Jubin's 8085simulator
    , stringProperty "WM_NAME" =? "Save Mnemonics" --> doCenterFloat
    , stringProperty "WM_NAME" =? "Warning ! " --> doCenterFloat
    --
    , stringProperty "WM_NAME" =? "Quit GIMP" --> doCenterFloat
    ]

-- This is more or less a hack
-- Couldn't remove the text, so had the fg color match the bg
myTabConfig = def { decoHeight = 1 
                  , activeBorderWidth = 0
                  , inactiveBorderWidth = 0
                  , urgentBorderWidth = 0
                  , decoWidth = 1
                  , activeColor = "#FFFFFF"
                  , inactiveColor = "#AAAAAA"
                  , urgentColor = "#AAAAAA"
                  , activeTextColor = "#FFFFFF"
                  , inactiveTextColor = "#AAAAAA"
                  , urgentTextColor = "#FFFFFF"}

myLayoutHook =
    onWorkspace "10" simpleFloat $
    onWorkspace "9" (avoidStruts Full ||| tiled) $
    onWorkspace "8" (avoidStruts Full ||| tiled) $
    avoidStruts $ spacingWithEdge 4 $ gaps [(U, 5), (R, 3), (D, 3), (L, 3)] $
            tiled
        ||| mastered (1/100) (1/2) (tabbed shrinkText myTabConfig)
        ||| Full
        ||| Mirror tiled
    where
        tiled = Tall nmaster delta ratio
        nmaster = 1
        ratio = 1/2
        delta = 3/100

-- XMobar workspaces; copied from the wiki, works fine, so will not bother modifying further
myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = white . wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppOrder           = \(ws:_) -> [ws]
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    }
    where
        blue, lowWhite, magenta, red, white, yellow :: String -> String
        magenta  = xmobarColor "#ff79c6" ""
        blue     = xmobarColor "#bd93f9" ""
        white    = xmobarColor "#f8f8f2" ""
        yellow   = xmobarColor "#f1fa8c" ""
        red      = xmobarColor "#ff5555" ""
        lowWhite = xmobarColor "#bbbbbb" ""

myStartupHook :: X ()
myStartupHook = do
    spawnOnce "~/.cabal/bin/xmobar"
    spawnOnce "redshift"
    spawnOnce "picom -b"
    spawnOnce "conky.sh"
    spawnOnce "/usr/libexec/notification-daemon"
    spawnOnce "emacs --daemon"

myExtraWorkspaces = [ (xK_0, "10"), (xK_minus, "11"), (xK_equal, "12") ]
myWorkspaces = map show [ 1 .. 9 :: Int ]

main :: IO ()
main = xmonad
    . ewmh
    . docks
    . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
    $ def
        { modMask = mod4Mask
        , workspaces = myWorkspaces ++ map snd myExtraWorkspaces
        , layoutHook = myLayoutHook
        , manageHook = myManageHook <+> manageHook def
        , startupHook = myStartupHook
        , terminal = "alacritty"
        , borderWidth = 0
        }
        `removeKeysP`
        [ "M-p"
        , "M-S-q"
        , "M-S-c"
        ]
        `additionalMouseBindings`
        [ ((0, 8), const $ spawn "xdotool type -")
        , ((0, 9), const $ spawn "xdotool type +")
        ]
        `additionalKeys`
        [
          ((mod4Mask, key), windows $ greedyView ws)
          | (key, ws) <- myExtraWorkspaces
        ] ++ [
          ((mod4Mask .|. shiftMask, key), windows $ shift ws)
          | (key, ws) <- myExtraWorkspaces
        ]
        `additionalKeysP`
        [ ("M-S-d", spawn "~/.config/rofi/implements/launcher.sh")
        -- The Application submap
        , ("M-d", visualSubmap def $ fromList
                [ ((0, xK_z), subName "Zathura -- Resume" $ spawn "~/.config/rofi/implements/fzathura.sh --resume")
                , ((shiftMask, xK_z), subName "Zathura" $ spawn "~/.config/rofi/implements/fzathura.sh --menu")
                , ((controlMask, xK_z), subName "Zathura -- No Save" $ spawn "~/.config/rofi/implements/fzathura.sh --no-save-recent --menu")
                , ((0, xK_w), subName "URxvt" $ spawn "urxvt")
                , ((0, xK_g), subName "Gimp" $ spawn "gimp")
                , ((0, xK_e), subName "Emacs" $ spawn "emacsclient --create-frame")
                , ((0, xK_v), subName "Volume" $ spawn "pavucontrol")
                , ((0, xK_p), subName "Private Window" $ spawn "~/.local/bin/firefox --private-window")
                ])
        -- The Games submap
        , ("M-a", visualSubmap def $ fromList
                [ ((0, xK_r), subName "RPCS3" $ spawn "~/.local/bin/rpcs3-v0.0.33-17020-d51d5ce8_linux64.AppImage")
                , ((0, xK_d), subName "Dolphin" $ spawn "dolphin-emu")
                , ((0, xK_c), subName "Citra" $ spawn "~/.local/bin/citra-qt.AppImage")
                ]
        )
        , ("M-q", kill)
        , ("M-S-r", spawn "xmonad --recompile&& xmonad --restart")
        , ("M-S-n", spawn "nettoggle.sh")
        , ("M-<Return>", spawn "alacritty")
        , ("M-S-<Return>", spawn "~/.local/bin/firefox")
        , ("M-C-<Print>", spawn "flameshot full")
        , ("M-<Print>", spawn "flameshot gui")
        , ("M-S-p", spawn "if [ `pgrep picom` ]; then pkill picom; else picom -b; fi")
        , ("M-g", goToSelected def)
        , ("M-m", windows swapMaster)
        , ("M-S-<Escape>", io exitSuccess)
        ]
