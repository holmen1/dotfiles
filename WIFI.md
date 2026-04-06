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

## test
holmen1@besk log$ wpa_cli list_networks
Selected interface 'wlan0'
network id / ssid / bssid / flags
0       AA46    any     [DISABLED]
1       BB46    any     [DISABLED]
2       a46     any     [DISABLED]
3       b46     any     [CURRENT]
holmen1@besk log$ ping dn.se
PING dn.se (34.117.105.189): 56 data bytes
64 bytes from 34.117.105.189: icmp_seq=0 ttl=116 time=2.348 ms
^C
--- dn.se ping statistics ---
1 packets transmitted, 1 packets received, 0.0% packet loss
round-trip min/avg/max/stddev = 2.348/2.348/2.348/0.000 ms
holmen1@besk log$ wpa_cli list_networks
Selected interface 'wlan0'
network id / ssid / bssid / flags
0       AA46    any     [DISABLED]
1       BB46    any     [DISABLED]
2       a46     any     [CURRENT]
3       b46     any     [DISABLED]
holmen1@besk log$ ping dn.se
ping: cannot resolve dn.se: Name could not be resolved at this time



## sudo service netif restart wlan0
Apr  6 08:32:59 besk wpa_supplicant[485]: wlan0: CTRL-EVENT-DISCONNECTED bssid=32:20:9f:2e:f6:62 reason=3 locally_generated=1
Apr  6 08:32:59 besk wpa_supplicant[485]: wlan0: CTRL-EVENT-DSCP-POLICY clear_all
Apr  6 08:32:59 besk syslogd: last message repeated 1 times
Apr  6 08:32:59 besk dhclient[2074]: connection closed
Apr  6 08:32:59 besk dhclient[2074]: exiting.
Apr  6 08:32:59 besk wpa_supplicant[485]: wlan0: CTRL-EVENT-TERMINATING
Apr  6 08:33:00 besk wpa_supplicant[4252]: Successfully initialized wpa_supplicant
Apr  6 08:33:00 besk wpa_supplicant[4252]: ioctl[SIOCS80211, op=20, val=0, arg_len=7]: Invalid argument
Apr  6 08:33:00 besk syslogd: last message repeated 1 times
Apr  6 08:33:03 besk wpa_supplicant[4253]: MBO: Disable MBO/OCE due to misbehaving AP not having enabled PMF
Apr  6 08:33:03 besk wpa_supplicant[4253]: wlan0: Trying to associate with 32:20:9f:2e:f6:65 (SSID='a46' freq=5240 MHz)
Apr  6 08:33:03 besk wpa_supplicant[4253]: wlan0: Associated with 32:20:9f:2e:f6:65
Apr  6 08:33:03 besk wpa_supplicant[4253]: MBO: Disable MBO/OCE due to misbehaving AP not having enabled PMF
Apr  6 08:33:03 besk wpa_supplicant[4253]: wlan0: WPA: Key negotiation completed with 32:20:9f:2e:f6:65 [PTK=CCMP GTK=CCMP]
Apr  6 08:33:03 besk wpa_supplicant[4253]: wlan0: CTRL-EVENT-CONNECTED - Connection to 32:20:9f:2e:f6:65 completed [id=2 id_str=]
Apr  6 08:33:03 besk dhclient[4309]: DHCPREQUEST on wlan0 to 255.255.255.255 port 67
Apr  6 08:33:03 besk dhclient[4309]: DHCPACK from 192.168.1.1
Apr  6 08:33:03 besk dhclient[4309]: bound to 192.168.1.85 -- renewal in 43200 seconds.







## status
holmen1@besk log$ wpa_cli status
Selected interface 'wlan0'
bssid=32:20:9f:2e:f6:65
freq=0
ssid=a46
id=2
mode=station
pairwise_cipher=CCMP
group_cipher=CCMP
key_mgmt=WPA2-PSK
wpa_state=COMPLETED
ip_address=192.168.1.85
address=a4:02:b9:a7:ce:25
uuid=1860de16-1d93-5667-bb44-8394d37854e0


holmen1@besk log$ wpa_cli list_networks
Selected interface 'wlan0'
network id / ssid / bssid / flags
0       AA46    any     [DISABLED]
1       BB46    any     [DISABLED]
2       a46     any     [CURRENT]
3       b46     any     [DISABLED]
