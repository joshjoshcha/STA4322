#!/bin/bash
cd ~/Desktop/experimental_design

git add .
git commit -m "Auto-sync: $(date '+%Y-%m-%d %H:%M:%S')"
git push origin main
