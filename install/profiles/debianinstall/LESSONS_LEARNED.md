# Lessons Learned

## st font rendering issue on Debian

- If st displays text with unexpected spaces (e.g., 'holmen' as 'ho lmen'), it is likely a font or fontconfig issue.
- Ensure the font specified in config.h (e.g., JetBrainsMono Nerd Font Mono) is installed on the system. Check with:
  fc-list | grep "JetBrainsMono Nerd Font Mono"
- If the font is missing, install it and rebuild st.
- If the font is present, try switching to a different font (e.g., Liberation Mono) in config.h and rebuild st to see if the issue persists.
- Make sure the fontconfig package is installed and up to date.
- Try running st with a different locale (e.g., LANG=C st) to rule out locale issues.
- This issue is not caused by st itself, but by missing or misconfigured fonts on the system.
