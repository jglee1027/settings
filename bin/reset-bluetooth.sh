#!/usr/bin/env bash

modprobe -r btusb
modprobe -r btintel
echo Initialize bluetooth modules && sleep 3
modprobe btintel
modprobe btusb
echo Restart bluetooth && sleep 3
systemctl restart bluetooth
