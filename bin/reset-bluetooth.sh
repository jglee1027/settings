#!/usr/bin/env bash

sudo modprobe -r btusb
sudo modprobe -r btintel
echo Initialize bluetooth modules && sleep 3
sudo modprobe btintel
sudo modprobe btusb
echo Restart bluetooth && sleep 3
sudo systemctl restart bluetooth
