#!/usr/bin/env python

import os
import sys
import unicodedata

sys.stdout.reconfigure(encoding='utf-8')

directory = "."
for filename in os.listdir(directory):
    nfc_name = unicodedata.normalize('NFC', filename)
    if filename != nfc_name:
        print(f"rename: {filename} → {nfc_name}")
        os.rename(os.path.join(directory, filename), os.path.join(directory, nfc_name))
