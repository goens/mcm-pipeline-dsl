

# Artifact Evaluation

## Running all experiments with the run-litmus-on-lsqs.py script

To run all the experiments from the paper, and the corresponding litmus tests, first cd to the run-artifact directory.
```
cd run-artifact
```
Then run the python script run-litmus-on-lsqs.py with the following arguments.
```
python run-litmus-on-lsqs.py <murphi_src> <num_parallel_litmus_test_processes> <memory_in_MB_per_litmus_test> <timeout_in_seconds>
```

The murphi\_src argument is the directory where cmurphi5.5.0 has been downloaded and extracted.

The num\_parallel\_litmus\_test\_processes argument is the number of processes to use for running multiple litmus tests in parallel. Use 1 for just running 1 litmus test at a time.

The memory\_in\_MB\_per\_litmus\_test argument is the amount of memory in MB to allocate per litmus test process. This may start using more than the specified amount if the particular litmus test results in a large state space for Murphi to explore. NOTE: remember that this is the amount of memory per process. About 10-15 GB should be enough for most tests. If a test fails because there isn't enough memory, you can re-run the test manually with the manually-run-test.py script.

The timeout\_in\_seconds argument specifies the amount of time to allow each litmus test to run for. If the specified amount of time is not enough, the litmus test will return with "Timeout". The test can be manually re-run with the manually-run-test.py script.

At the end, this script will print the results of the experiments and litmus tests, and save them to files. These tables can be compared with the tables in the paper.

Note that the LB + ARM + IO with the N7 litmus test is Disallowed, and will be updated in the paper.

## Manually re-running a test

To manually re-run a test with the manually-run-test.py script, provide the path to the specific test.out file, the test.out file name, and the amount of memory to run the test with in MB:
```
manually-run-test.py <test_directory> <test_to_run> <memory_in_MB>
```

