# Debug dmenu-wifi FreeBSD status detection

Run on FreeBSD and paste output:

holmen1@besk ~$ wpa_cli list_networks | awk -F'\t' -v s="AA46" '$2==s{print $1}'
0
holmen1@besk ~$ wpa_cli select_network 0
Selected interface 'wlan0'
OK
holmen1@besk ~$ ping dn.se
ping: cannot resolve dn.se: Name could not be resolved at this time

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
