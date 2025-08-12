#!/bin/bash

clean_up() {
    # hostapd is the controlling program, this process ends with it
    [[ -n "$(pgrep 'dnsmasq')" ]]&& killall dnsmasq
    systemctl start dhcpcd.service
    systemctl start iwd.service
}

hotspot_preconfigure() {
    # Stop DHCPD and IWD
    systemctl stop dhcpcd.service
    systemctl stop iwd.service

    # Flush the devices
    ip addr flush wlan0
    ifconfig wlan0 192.168.0.101 netmask 255.255.255.0
    # Some sources recommend waiting a bit, for some reason (IP assignment?)
    sleep 2

    [[ -n "$(pgrep 'dnsmasq')" ]]&& killall dnsmasq
    dnsmasq
}

TOGGLE=0

GETOPTS_OUT=$(getopt -o '' -l toggle -n "hotspot.sh" -- "$@")
[[ $? != 0 ]] && exit 1
eval set -- "$GETOPTS_OUT"

while true; do
	case "$1" in
		--toggle )
            TOGGLE=1
            shift ;;
        * )
            shift; break ;;
	esac
done


if [[ $TOGGLE -eq 1 ]]; then
    # We work under the assumption that only one of iwd and hostapd can be running at a time
    if [[ -z "$(pgrep hostapd)" ]]; then
        hotspot_preconfigure
        hostapd -B /etc/hostapd/hostapd.conf.vht
    else
        killall hostapd
        clean_up
    fi
else
    trap clean_up EXIT
    hotspot_preconfigure
    [[ -n "$(pgrep 'hostapd')" ]]&& killall hostapd
    hostapd /etc/hostapd/hostapd.conf.vht
fi
