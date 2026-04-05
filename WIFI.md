# Debug dmenu-wifi FreeBSD status detection

Run on FreeBSD and paste output:

```sh
IFACE=wlan0
holmen1@besk ~$ INFO=$(ifconfig $IFACE 2>/dev/null)
holmen1@besk ~$ printf '%s' "$INFO" | awk '/status:/{print $2}'
associated
holmen1@besk ~$ printf '%s' "$INFO" | awk '/ssid/{print $2}'
a46
```

holmen1@besk ~$ sudo ifconfig wlan0 up list scan
holmen1@besk ~$ sudo ifconfig wlan0 ssid AA46
holmen1@besk ~$ sudo dhclient wlan0
wlan0: no link .............. giving up
holmen1@besk ~$ ping dn.se
ping: cannot resolve dn.se: Name could not be resolved at this time
---
$ sudo service netif restart



$ sudo cat /etc/wpa_supplicant.conf
ctrl_interface=/var/run/wpa_supplicant
eapol_version=2
ap_scan=1
fast_reauth=1

network={
        ssid="AA46"
        scan_ssid=0
        psk="xx"
        priority=4
}
network={
        ssid="BB46"
        scan_ssid=0
        psk="xx"
        priority=5
}
network={
        ssid="a46"
        scan_ssid=0
        psk="xx"
        priority=6
}
network={
        ssid="b46"
        scan_ssid=0
        psk="xx"
        priority=5
}
