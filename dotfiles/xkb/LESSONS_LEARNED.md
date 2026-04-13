# LESSONS LEARNED

## Shift modifier stays active in apps

If a key at level 2 (Shift+key) produces a keysym like `Up`, the app still sees `Shift+Up` instead of bare `Up`. Put frequently-used, modifier-sensitive keys at level 1.

Example: `ö → Up` at L1 scrolls fine; at L2 it would send `Shift+Up`, which most apps don't recognize as scroll.