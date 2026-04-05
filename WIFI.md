# Debug dmenu-wifi FreeBSD status detection

Run on FreeBSD and paste output:

```sh
IFACE=wlan0
INFO=$(ifconfig $IFACE 2>/dev/null)
printf '%s' "$INFO" | awk '/status:/{print $2}'
printf '%s' "$INFO" | awk '/ssid/{print $2}'
```

---

holmen1@besk dotfiles (dmenu)$ ifconfig wlan0
wlan0: flags=8843<UP,BROADCAST,RUNNING,SIMPLEX,MULTICAST> metric 0 mtu 1500
        options=0
        ether a4:02:b9:a7:ce:25
        inet 192.168.1.85 netmask 0xffffff00 broadcast 192.168.1.255
        inet6 fe80::a602:b9ff:fea7:ce25%wlan0 prefixlen 64 scopeid 0x3
        inet6 fd22:e107:56fb:0:a602:b9ff:fea7:ce25 prefixlen 64 autoconf pltime 3600 vltime 7200
        groups: wlan
        ssid a46 channel 48 (5240 MHz 11a) bssid 32:20:9f:2e:f6:65
        regdomain ETSI country SE authmode WPA2/802.11i privacy ON
        deftxkey UNDEF AES-CCM 2:128-bit AES-CCM 3:128-bit
        AES-CCM ucast:128-bit txpower 17 bmiss 10 mcastrate 6 mgmtrate 6
        scanvalid 60 wme roaming MANUAL
        parent interface: iwm0
        media: IEEE 802.11 Wireless Ethernet OFDM/54Mbps mode 11a
        status: associated
        nd6 options=23<PERFORMNUD,ACCEPT_RTADV,AUTO_LINKLOCAL>
