# Hyprland

## Installation
hyprland included with archinstall  


## Waybar
Waybar is a highly customizable status bar for Wayland compositors

### Installation
```bash
scripts/install_packages.sh  
scipts/link_config.sh
```

### Key Features
- Workspaces: Shows the current workspace and allows switching between workspaces
- Application Launcher: Allows launching applications from the status bar
- Battery: Displays battery status with icons and percentage 
- Network: Shows network status, including Wi-Fi signal strength and Ethernet IP address
- Power profiles: Allows switching between power profiles
- Pulseaudio: Manages audio volume and displays current volume level with icons
- Clock: Displays the current date and time
- CPU: Shows CPU usage
- Memory: Displays memory usage
- Custom Scripts: Allows the execution of custom scripts for additional functionality
- Power: Allows shutting down, restarting, or logging out of the system

### Key Bindings
- Super + Shift + Q: Open terminal
- Super + E: Open file manager
- Super + R: Open application launcher
- Super + Shift + C: Close the focused window
- Super + 1-6: Switch between workspaces
- Super + Shift + 1-6: Move the focused window to the specified workspace
- Super + Shift + B: Toggle the status bar
- Super + Shift + M: Exit the window manager

- Super + Left/Right: Move focus to the left/right window
- Super + Up/Down: Move focus to the up/down window

- Super + Print: Take a screenshot
- Super + Shift + Print: Take a screenshot of the focused window
 
### Wallpaper

Find your favorite wallpaper and convert it to png format
```bash
$ yay -S imagemagick
$ convert fav.jpg wall1.png
```
move it to the wallpaper directory
```bash
$ sudo cp wall1.png /usr/share/hypr
```
this will replace the default wallpaper wall0.png  
hypr uses 3 default wallpapers wall0.png, wall1.png, wall2.png

## Lessons Learned

brightnessctl needed for backlight keys