#!/bin/bash



if grep -q "No error found." test-HP-arm-io-it/armv8-HP-IT-amd1.out.run; then
  echo Unobserved Ordering
elif grep -q "Invariant .* failed." test-HP-arm-io-it/armv8-HP-IT-amd1.out.run; then
  echo Observed Ordering
else
  echo Unexpected error occured.
fi
