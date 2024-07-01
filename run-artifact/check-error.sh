#!/bin/zsh

if grep -q $'Status:\n\n\tNo error found.' test-HP-arm-io-it/armv8-HP-IT-amd1-dmb-sy.out.run; then
  echo "Unobserved Ordering"
elif grep -q $'Result:\n\n\tInvariant ".*" failed.' test-HP-arm-io-it/armv8-HP-IT-amd1-dmb-sy.out.run; then
  echo "Observed Ordering"
else
  echo "Unexpected error occured."
fi
