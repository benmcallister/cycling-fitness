#!/bin/bash
# Double-click this in Finder (or run ./ride.command) to analyze the latest ride.
cd "$(dirname "$0")"
/Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/bin/Rscript analyze-ride.R
