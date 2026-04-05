# dmenu-wifi FreeBSD connect_network — status

## What works
- `IFACE`, `STATE`, `SSID` detection via `ifconfig` — confirmed working
- `wpa_cli -i wlan0 list_networks` finds known SSIDs correctly
- `wpa_cli -i wlan0 select_network <id>` returns OK

## Problem
Switching networks via `wpa_cli select_network` + `reassociate` leaves `status: no carrier`.
`dhclient` subsequently fails or reports "no link".
Root cause not identified — wpa_supplicant does not complete reassociation.

## Attempted
- `wpa_cli select_network` alone → no carrier
- `wpa_cli select_network` + `wpa_cli reassociate` → no carrier
- kill dhclient pid + restart dhclient → no link
- adding `-i "$IFACE"` to all wpa_cli calls → no carrier

## TODO
- Check wpa_supplicant log: `sudo wpa_cli -i wlan0 log_level DEBUG`
- Try `sudo service netif restart wlan0` as an alternative to dhclient
- Consider `ifconfig wlan0 down && ifconfig wlan0 up` before reassociate
- May need `wpa_supplicant` restarted via rc.d rather than wpa_cli alone
