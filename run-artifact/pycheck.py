

import subprocess

# result = subprocess.run("grep -q \"No error found.\" armv8-HP-IT-amd1-dmb-sy.out.run", shell=True)
result = subprocess.run("if grep -q \"No error found.\" armv8-HP-IT-amd1.out.run; then exit 0; else exit 1; fi", shell=True)
# result = subprocess.run("if grep -q $'Status:\n\n\tNo error found.' armv8-HP-IT-amd1-dmb-sy.out.run; then exit 0; else exit 1; fi", shell=True)

print(result.returncode)
if result.returncode == 0:
    print("return 0!")
else:
    print("return 1!")
