#!/bin/zsh

if grep -q $'Status:\n\n\tNo error found.' armv8-HP-IT-amd1.out.run; then
  echo "Unobserved Ordering"
elif grep -q $'Result:\n\n\tInvariant ".*" failed.' armv8-HP-IT-amd1.out.run; then
  echo "Observed Ordering"
else
  echo "Unexpected error occured."
fi
