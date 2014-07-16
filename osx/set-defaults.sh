# Sets reasonable OS X defaults.
#
# The original idea (and a couple settings) were grabbed from:
#   https://github.com/mathiasbynens/dotfiles/blob/master/.osx
#   https://github.com/holman/dotfiles/blob/master/osx/set-defaults.sh
#
# Run ./set-defaults.sh and you'll be good to go.

# Ask for the administrator password upfront
echo "Administrator privileges are required to set up your OSX defaults."
sudo -v

# Keep-alive: update existing `sudo` time stamp until `.osx` has finished
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

# Disable the sound effects on boot
printf "> Disabling sound effects on boot..."
sudo nvram SystemAudioVolume=" "
printf "...done\n"

# Show the ~/Library folder.
printf "> Showing ~/Library folder..."
chflags nohidden ~/Library
printf "...done\n"

# Increase window resize speed for Cocoa applications
printf "> Increase window resize speed..."
defaults write NSGlobalDomain NSWindowResizeTime -float 0.001
printf "...done\n"

# Automatically quit printer app once the print jobs complete
printf "> Automatically quit printer app when jobs are complete..."
defaults write com.apple.print.PrintingPrefs "Quit When Finished" -bool true
printf "...done\n"

# Save to disk (not to iCloud) by default
printf "> Save to disk instead of iCloud by default..."
defaults write NSGlobalDomain NSDocumentSaveNewDocumentsToCloud -bool false
printf "...done\n"

# Expand save panel by default
printf "> Save panel by default..."
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode2 -bool true
printf "...done\n"

# Expand print panel by default
printf "> Expand print panel by default..."
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint -bool true
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint2 -bool true
printf "...done\n"

# Reveal IP address, hostname, OS version, etc. when clicking the clock
# in the login window
printf "> Reveal more info when clicking the clock in the login window..."
sudo defaults write /Library/Preferences/com.apple.loginwindow AdminHostInfo HostName
printf "...done\n"

# Trackpad: enable tap to click for this user and for the login screen
printf "> Enable tap to click by default..."
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
defaults write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
printf "...done\n"

# Trackpad: map bottom right corner to right-click
printf "> Map bottom right corner of trackpad to right-click..."
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadCornerSecondaryClick -int 2
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadRightClick -bool true
defaults -currentHost write NSGlobalDomain com.apple.trackpad.trackpadCornerClickBehavior -int 1
defaults -currentHost write NSGlobalDomain com.apple.trackpad.enableSecondaryClick -bool true
printf "...done\n"

# Enable “natural” (Lion-style) scrolling
printf "> Enable 'natural' scrolling..."
defaults write NSGlobalDomain com.apple.swipescrolldirection -bool true
printf "...done\n"

# Increase sound quality for Bluetooth headphones/headsets
printf "> Increase sound quality for Bluetooth headphones..."
defaults write com.apple.BluetoothAudioAgent "Apple Bitpool Min (editable)" -int 40
printf "...done\n"

# Enable full keyboard access for all controls
# (e.g. enable Tab in modal dialogs)
printf "> Enable full keyboard access for all controls..."
defaults write NSGlobalDomain AppleKeyboardUIMode -int 3
printf "...done\n"

# Use scroll gesture with the Ctrl (^) modifier key to zoom
printf "> Use Ctrl and scroll to zoom..."
defaults write com.apple.universalaccess closeViewScrollWheelToggle -bool true
defaults write com.apple.universalaccess HIDScrollZoomModifierMask -int 262144
printf "...done\n"

# Follow the keyboard focus while zoomed in
printf "> Follow keyboard focus while zoomed in..."
defaults write com.apple.universalaccess closeViewZoomFollowsFocus -bool true
printf "...done\n"

# Disable press-and-hold for keys in favor of key repeat
printf "> Disable press-and-hold for keys in favor of key repeat..."
defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false
printf "...done\n"

# Set a blazingly fast keyboard repeat rate
printf "> Set a fast keyboard repeat rate..."
defaults write NSGlobalDomain KeyRepeat -int 0
printf "...done\n"

# Oh yeah - Metric units by default!
printf "> Set metric units by default..."
defaults write NSGlobalDomain AppleMetricUnits -bool true
printf "...done\n"

# Disable auto-correct
printf "> Disable auto correct..."
defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false
printf "...done\n"

# Require password immediately after sleep or screen saver begins
printf "> Ask for password after sleep or screen saver..."
defaults write com.apple.screensaver askForPassword -int 1
defaults write com.apple.screensaver askForPasswordDelay -int 0
printf "...done\n"

# Save screenshots to the ${HOME}/screenshots
printf "> Save screenshots to ${HOME}/screenshots..."
defaults write com.apple.screencapture location -string "${HOME}/screenshots"
printf "...done\n"

# Save screenshots in PNG format (other options: BMP, GIF, JPG, PDF, TIFF)
printf "> Save screenshots as PNG..."
defaults write com.apple.screencapture type -string "png"
printf "...done\n"

# Disable shadow in screenshots
printf "> Disable shadow in screenshots..."
defaults write com.apple.screencapture disable-shadow -bool true
printf "...done\n"

# Finder: allow quitting via ⌘ + Q; doing so will also hide desktop icons
printf "> Allow quitting of Finder..."
defaults write com.apple.finder QuitMenuItem -bool true
printf "...done\n"

# Finder: disable window animations and Get Info animations
printf "> Disable window animations in Finder..."
defaults write com.apple.finder DisableAllAnimations -bool true
printf "...done\n"

# Set $HOME as the default location for new Finder windows
# For other paths, use `PfLo` and `file:///full/path/here/`
printf "> Set ${HOME} as default location for new Finder windows..."
defaults write com.apple.finder NewWindowTarget -string "PfLo"
defaults write com.apple.finder NewWindowTargetPath -string "file://${HOME}/"
printf "...done\n"

# Don't show these on the desktop - we're not animals
printf "> Don't show icons on the desktop..."
defaults write com.apple.finder ShowExternalHardDrivesOnDesktop -bool false
defaults write com.apple.finder ShowHardDrivesOnDesktop -bool false
defaults write com.apple.finder ShowMountedServersOnDesktop -bool false
defaults write com.apple.finder ShowRemovableMediaOnDesktop -bool false
printf "...done\n"

# Finder: show hidden files by default
printf "> Show hidden files in Finder by default..."
defaults write com.apple.finder AppleShowAllFiles -bool true
printf "...done\n"

# Finder: show status bar
printf "> Show status bar in Finder..."
defaults write com.apple.finder ShowStatusBar -bool true
printf "...done\n"

# Finder: show path bar
printf "> Show path bar in Finder..."
defaults write com.apple.finder ShowPathbar -bool true
printf "...done\n"

# Finder: allow text selection in Quick Look
printf "> Allow text selection in Quick Look..."
defaults write com.apple.finder QLEnableTextSelection -bool true
printf "...done\n"

# Display full POSIX path as Finder window title
printf "> Disable full path as Finder window title..."
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true
printf "...done\n"

# When performing a search, search the current folder by default
printf "> Seach current folder by default..."
defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"
printf "...done\n"

# Disable the warning when changing a file extension
printf "> Disable warning when changing a file extension..."
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false
printf "...done\n"

# Avoid creating .DS_Store files on network volumes
printf "> Don't create .DS_Store files on network volumes..."
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
printf "...done\n"

# Disable disk image verification
printf "> Disable disk image verification..."
defaults write com.apple.frameworks.diskimages skip-verify -bool true
defaults write com.apple.frameworks.diskimages skip-verify-locked -bool true
defaults write com.apple.frameworks.diskimages skip-verify-remote -bool true
printf "...done\n"

# Automatically open a new Finder window when a volume is mounted
printf "> Open a new Finder window when a volume is mounted..."
defaults write com.apple.frameworks.diskimages auto-open-ro-root -bool true
defaults write com.apple.frameworks.diskimages auto-open-rw-root -bool true
defaults write com.apple.finder OpenWindowForNewRemovableDisk -bool true
printf "...done\n"

# Use list view in all Finder windows by default
# Four-letter codes for the other view modes: `icnv`, `clmv`, `Flwv`
printf "> Use list view in Finder by default..."
defaults write com.apple.finder FXPreferredViewStyle -string "Nlsv"
printf "...done\n"

# Disable the warning before emptying the Trash
printf "> Disable warning before emptying Trash..."
defaults write com.apple.finder WarnOnEmptyTrash -bool false
printf "...done\n"

# Empty Trash securely by default
printf "> Empty Trash securely by default..."
defaults write com.apple.finder EmptyTrashSecurely -bool true
printf "...done\n"

# Enable AirDrop over Ethernet
printf "> Enable Airdrop over Ethernet......"
defaults write com.apple.NetworkBrowser BrowseAllInterfaces -bool true
printf "...done\n"

# Expand the following File Info panes:
# “General”, “Open with”, and “Sharing & Permissions”
printf "> Enable File Info panes......"
defaults write com.apple.finder FXInfoPanesExpanded -dict \
	General -bool true \
	OpenWith -bool true \
	Privileges -bool true
printf "...done\n"

# Don’t animate opening applications from the Dock
printf "> Don't animate opening Dock applications..."
defaults write com.apple.dock launchanim -bool false
printf "...done\n"

# Disable Dashboard
printf "> Disable Dashboard..."
defaults write com.apple.dashboard mcx-disabled -bool true
printf "...done\n"

# Don’t show Dashboard as a Space
printf "> Don't show Dashboard as a Space..."
defaults write com.apple.dock dashboard-in-overlay -bool true
printf "...done\n"

# Don’t automatically rearrange Spaces based on most recent use
printf "> Don't rearrange spaces based on recent use..."
defaults write com.apple.dock mru-spaces -bool false
printf "...done\n"

###############################################################################
# Terminal & iTerm 2                                                          #
###############################################################################

# Only use UTF-8 in Terminal.app
printf "> Only use UTF-8 in Terminal..."
defaults write com.apple.terminal StringEncodings -array 4
printf "...done\n"

# Don’t display the annoying prompt when quitting iTerm
printf "> Don't display prompt when quitting iTerm..."
defaults write com.googlecode.iterm2 PromptOnQuit -bool false
printf "...done\n"

###############################################################################
# Time Machine                                                                #
###############################################################################

# Prevent Time Machine from prompting to use new hard drives as backup volume
printf "> Prevent Time Machine from prompting on new hard drives..."
defaults write com.apple.TimeMachine DoNotOfferNewDisksForBackup -bool true
printf "...done\n"

# Disable local Time Machine backups
printf "> Disable local Time Machine backups..."
hash tmutil &> /dev/null && sudo tmutil disablelocal
printf "...done\n"

###############################################################################
# Activity Monitor                                                            #
###############################################################################

# Show the main window when launching Activity Monitor
printf "> Show main window in Activity Monitor..."
defaults write com.apple.ActivityMonitor OpenMainWindow -bool true
printf "...done\n"

# Visualize CPU usage in the Activity Monitor Dock icon
printf "> Visualize CPU Usage in Activity Monitor Dock icon..."
defaults write com.apple.ActivityMonitor IconType -int 5
printf "...done\n"

# Show all processes in Activity Monitor
printf "> Show all processes in Activity Monitor..."
defaults write com.apple.ActivityMonitor ShowCategory -int 0
printf "...done\n"

# Sort Activity Monitor results by CPU usage
printf "> Sort Activity Monitor by CPU..."
defaults write com.apple.ActivityMonitor SortColumn -string "CPUUsage"
defaults write com.apple.ActivityMonitor SortDirection -int 0
printf "...done\n"

###############################################################################
# Address Book, Dashboard, iCal, TextEdit, and Disk Utility                   #
###############################################################################

# Enable the debug menu in Address Book
printf "> Enable debug menu in Address Book..."
defaults write com.apple.addressbook ABShowDebugMenu -bool true
printf "...done\n"

# Enable Dashboard dev mode (allows keeping widgets on the desktop)
printf "> Enable Dashboard dev mode..."
defaults write com.apple.dashboard devmode -bool true
printf "...done\n"

# Enable the debug menu in iCal (pre-10.8)
printf "> Enable iCal debug menu..."
defaults write com.apple.iCal IncludeDebugMenu -bool true
printf "...done\n"

# Use plain text mode for new TextEdit documents
printf "> Use plain text mode for new TextEdit documents..."
defaults write com.apple.TextEdit RichText -int 0
printf "...done\n"

# Open and save files as UTF-8 in TextEdit
printf "> Open and save files as UTF-8 in TextEdit..."
defaults write com.apple.TextEdit PlainTextEncoding -int 4
defaults write com.apple.TextEdit PlainTextEncodingForWrite -int 4
printf "...done\n"

# Enable the debug menu in Disk Utility
printf "> Enable debug menu in Disk Utility..."
defaults write com.apple.DiskUtility DUDebugMenuEnabled -bool true
defaults write com.apple.DiskUtility advanced-image-options -bool true
printf "...done\n"

###############################################################################
# Google Chrome & Google Chrome Canary                                        #
###############################################################################

# Allow installing user scripts via GitHub Gist or Userscripts.org
printf "> Allow user scripts in Chrome..."
defaults write com.google.Chrome ExtensionInstallSources -array "https://gist.githubusercontent.com/" "http://userscripts.org/*"
defaults write com.google.Chrome.canary ExtensionInstallSources -array "https://gist.githubusercontent.com/" "http://userscripts.org/*"
printf "...done\n"

echo "Done. Note that some of these changes require a logout/restart to take effect."
