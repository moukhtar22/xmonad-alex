# XMonad-Alex :: XMonad
These are the dots for my XMonad installation, xmonad-alex; so that I can reproduce it easily when
changing systems.

## Screenshots
![Empty Screen](../Screenshots/SS1-Empty.png)
![Emacs](../Screenshots/SS2-Emacs.png)
![Fetch](../Screenshots/SS3-Fetch.png)

## Installation Dependencies, for the WM
- **git**: If you're here, you probably already have it installed.
- **Haskell**: I recommend you install all the tools provided under `ghcup`. This configuration
  uses `cabal`, rather than using the `xmonad` package provided by any particular Linux distribution.
  
## UI Applications
- **XMobar**: for the top/status bar
- **Picom**: as the compositor; I use the one available in the official Arch Linux *extra* repository.
- **Conky**: for resource monitoring
- **Nitrogen**: for the wallpaper
- **dunst**: For notifications
- **Redshift**: to not make my eyes hurt
- **Unclutter**: to not have to stare at the mouse pointer
- **Rofi**: as the application launcher

## Other Applications
- **emacs**: as the text editor
- **Alacritty**: as the primary terminal emulator
- **Wezterm**: when I wish to see the light
- **Firefox**: as the web browser
- **Zathura**: as the primary pdf viewer
- **zsh**: as the shell
- **Kanata**: QMK like keyboard (and mouse) management

## Usage Directions
1. `git clone` this repository to your `$HOME` directory.
2. In the root folder, run `stow .dotfiles` from the command line. This will create the necessary
   symlinks (*yes, you need to install `stow` separately).  
   *Note, you should probably create most of the directories yourself, especially ones
   like `.config`, `.config/xmoand`, and `.emacs` since a lot of the files in these directories
   really don't need to be tracked(and adding those files to `.gitignore` is a pain).*

### XMonad Installation
1. The first thing to do is getting a working Haskell installation. Check
   [here](https://www.haskell.org/ghcup/install/) for the required script. Also add `~/.cabal/bin`
   and `~/.ghcup/bin` to your `$PATH`(maybe using `.zshenv`).
2. Look [here](https://xmonad.org/INSTALL.html) for the packages required for installation. Last I
   checked, the command you need for Arch was,
   ```sh
   sudo pacman -Syu \
   git \
   xorg-server xorg-apps xorg-xinit xorg-xmessage \
   libx1 libxft libxinerama libxrandr libxss \
   pkgconf
   ```
3. `cd` to `.config/xmoand`, and run
   ```sh
   cabal update
   cabal build
   cabal install
   ```
   to build the package. This should add the binary's symlink in `~/.cabal/bin`.  
4. Install `xmobar` from Hackage: `cabal install xmobar`.
5. Symlink the executable, since the recompile keybinds assume that your executable is named
   `xmoand`, and I never really found out how to change that(if it can be changed).
   ```sh
   xmonad-alex --recompile
   ln -s ~/.cache/xmonad/xmonad-x86_64-linux ~/.local/bin/xmonad
   ```
   
### Xinitrc Setup
The first time around, run `startx` with a bare bones `.xinitrc`, with just the line
`exec ~/.cache/xmonad/xmonad-x86_64-linux`. Once inside an Xorg session, you can run the
`xinitrc.sh` script to auto build the `.xinitrc` file.  
*This is a script to find the touchpad xinput id, and set the acceleration and tapping properties
accordingly. Additionally, it also sets the keyboard repeat rate and delay, and changes the
keymaps.*

### Kanata
You can grab a statically linked binary [here](https://github.com/jtroo/kanata/releases).
