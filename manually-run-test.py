import sys
import os
import subprocess

def run_test(test_directory, test_to_run, memory_in_MB):
    test_output_file = f"{test_to_run}.run"
    test_command = f"./{test_to_run} -b32 -d murphi-trace -vdfs -td -m {memory_in_MB} >& {test_output_file}"
    check_result_command = f"if grep -q \"\tNo error found.\" {test_output_file}; then exit 0; elif grep -q \"\tInvariant .* failed.\" {test_output_file};  then exit 1; else exit 2; fi"

    curr_dir = os.getcwd()

    os.chdir(test_directory)

    subprocess.run(["zsh", "-c", test_command], shell=False)
    result = subprocess.run(["zsh", "-c", check_result_command], shell=False)

    os.chdir(curr_dir)

    if result == 0:
        print(f"{test_to_run} is Disallowed")
    elif result == 1:
        print(f"{test_to_run} is Allowed")
    elif result == 2:
        print(f"Unrecognized result, try re-running, or examine the output. Murphi may have ran out of memory.")

### Example usage:
##test_directory = sys.argv[1]  # Replace with the actual test directory
##run_test(test_directory)


"./armv8-nosq-replay-n7.out -b64 -d murphi-trace -vdfs -tf -m 2048 >& armv8-nosq-replay-n7.out.run"

"if grep -q \"\tNo error found.\" {litmus_test_log_name}; then exit 0; elif grep -q \"\tInvariant .* failed.\" {litmus_test_log_name};  then exit 1; else exit 2; fi"

def main():
    if len(sys.argv) != 4:
        print("Usage: python main.py <test_directory> <test_to_run> <memory_in_MB>")
        return

    test_directory = sys.argv[1]
    test_to_run = sys.argv[2]
    memory_in_MB = int(sys.argv[3])

    # Rest of your code here

if __name__ == "__main__":
    main()

