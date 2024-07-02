#!/bin/zsh



if grep -q "No error found." armv8-HP-IT-amd1.out.run; then
  echo Unobserved Ordering
elif grep -q "Invariant .* failed." armv8-HP-IT-amd1.out.run; then
  echo Observed Ordering
else
  echo Unexpected error occured.
fi
