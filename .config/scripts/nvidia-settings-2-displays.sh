#!/bin/bash

nvidia-settings -a CurrentMetaMode="DP-4: 1920x1080 +0+1080 { ForceFullCompositionPipeline = On }, DP-1: $1 +0+0 { ForceFullCompositionPipeline = On }"
