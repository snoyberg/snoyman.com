#!/usr/bin/env sh

exec env PORT=3000 DEV=1 stack runghc main.hs
