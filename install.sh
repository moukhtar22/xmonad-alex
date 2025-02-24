#!/bin/bash

# ================================
# Automatic Installation Script for Arch-Based Systems
# Repository: https://github.com/moukhtar22/xmonad-alex
# Author: Moukhtar Morsy
# ================================

# Exit on error
set -e

# Colors for better readability
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

# Function to print messages
print_message() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
    exit 1
}

# Check if the script is run as root
if [[ $EUID -ne 0 ]]; then
    print_error "This script must be run as root. Use sudo."
fi

# Update the system
print_message "Updating the system..."
pacman -Syu --noconfirm || print_error "Failed to update the system."

# Install essential tools
print_message "Installing essential tools..."
pacman -S --needed --noconfirm git base-devel || print_error "Failed to install essential tools."

# Install Haskell (ghcup)
print_message "Installing Haskell via ghcup..."
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
source ~/.ghcup/env || print_error "Failed to source ghcup environment."

# Install cabal and GHC
print_message "Installing cabal and GHC..."
ghcup install ghc latest
ghcup set ghc latest
ghcup install cabal latest
cabal update || print_error "Failed to update cabal."

# Clone the repository
REPO_URL="https://github.com/moukhtar22/xmonad-alex.git"
INSTALL_DIR="$HOME/xmonad-alex"

if [[ -d "$INSTALL_DIR" ]]; then
    print_warning "Directory $INSTALL_DIR already exists. Skipping clone."
else
    print_message "Cloning repository from $REPO_URL..."
    git clone "$REPO_URL" "$INSTALL_DIR" || print_error "Failed to clone the repository."
fi

# Install dependencies
print_message "Installing dependencies..."
DEPENDENCIES=(
    xorg-server xorg-xinit xorg-xrandr xorg-xsetroot
    xmobar picom conky nitrogen notification-daemon redshift unclutter rofi
    emacs alacritty wezterm firefox zathura zsh kmonad
)

pacman -S --needed --noconfirm "${DEPENDENCIES[@]}" || print_error "Failed to install dependencies."

# Copy configuration files
print_message "Copying configuration files..."
CONFIG_DIR="$HOME/.config"
mkdir -p "$CONFIG_DIR"

cp -r "$INSTALL_DIR/config/xmonad" "$CONFIG_DIR/" || print_error "Failed to copy XMonad config."
cp -r "$INSTALL_DIR/config/xmobar" "$CONFIG_DIR/" || print_error "Failed to copy Xmobar config."
cp "$INSTALL_DIR/.xinitrc" "$HOME/" || print_error "Failed to copy .xinitrc."

# Set up XMonad as the default session
print_message "Setting up XMonad as the default session..."
echo "exec xmonad" >> "$HOME/.xinitrc"

# Install XMonad using cabal
print_message "Building and installing XMonad with cabal..."
cd "$INSTALL_DIR" || print_error "Failed to navigate to installation directory."
cabal install xmonad xmonad-contrib || print_error "Failed to install XMonad with cabal."

# Post-installation message
print_message "Installation completed successfully!"
print_message "You can now start XMonad by running 'startx'."
