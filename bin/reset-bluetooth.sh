#!/usr/bin/env bash

modprobe -r btusb
modprobe -r btintel
sleep 2
modprobe btintel
modprobe btusb
sleep 1
systemctl restart bluetooth
