

from enum import Enum

import os
import subprocess
import shutil

import multiprocessing as mp
from multiprocessing import Pool
import sys

from subprocess import TimeoutExpired
import pandas as pd
import tabulate
import pprint

MURPHI_MEM_NUM = 6500 # about 6.5 GB of mem
TIMEOUT = 1 # 5 * 60 # 5 min

class LSQ(Enum):
    HP = "HP"
    LB = "LB"
    Unified = "Unified"

class Transformation(Enum):
    IO = "IO"
    LR = "LR"
    IT = "IT"

class MemoryModel(Enum):
    TSO = "TSO"
    ARM = "ARM"

class Experiment:
    def __init__(self,
            memory_model : MemoryModel,
            lsq : LSQ,
            transformation : Transformation
            ):
        self.lsq = lsq
        self.transformation = transformation
        self.memory_model = memory_model

def CreateOutputDirName(experiment: Experiment):
    # Convert experiment attributes to command arguments
    ## memory_model_arg = f"--memory-model {experiment.memory_model.value}"
    lsq_name = str(experiment.lsq.value)
    transform_name = str(experiment.transformation.value)
    mm_name = str(experiment.memory_model.value)
    out_dir_name = f"eval-{lsq_name}-{mm_name}-{transform_name}"
    return out_dir_name

def execute_aqlc(experiment: Experiment):
    # Convert experiment attributes to command arguments
    ## memory_model_arg = f"--memory-model {experiment.memory_model.value}"
    lsq_arg = f"-lsq {experiment.lsq.value}"
    transform_arg = f"-tfsm {experiment.transformation.value}"
    #mm_name = str(experiment.memory_model.value)
    out_dir_arg = f"-D {CreateOutputDirName(experiment)}"
    model_check_arg = "-m"

    transform_name = None
    match (experiment.transformation):
        case Transformation.IO:
            transform_name = "LR"
        case Transformation.LR:
            transform_name = "LR"
        case Transformation.IT:
            transform_name = "IT"
    lsq_dir_name = None
    match experiment.lsq:
        case LSQ.HP:
            lsq_dir_name = "lsq-henn-patt/o3"
        case LSQ.LB:
            lsq_dir_name = "lsq-nosq"
        case LSQ.Unified:
            lsq_dir_name = "lsq-load-replay"
    lsq_file_name = f"{experiment.lsq.value}-for-{transform_name}.aql"

    aql_file_arg = f"Examples/graph-prototype/operational-axiomatic/{lsq_dir_name}/{lsq_file_name}"

    command = "lake exe aqlc"
    # Construct the full command
    full_command = f"{command} {lsq_arg} {transform_arg} {out_dir_arg} {model_check_arg} {aql_file_arg}"

    # Execute the shell command
    subprocess.run(full_command, shell=True)

class LitmusResult(Enum):
    Allowed = "Allowed"
    Disallowed = "allowed"
    Timeout = "Timeout"
    UnexpectedResult = "UnexpectedResult"
    UnexpectedReturnCode = "UnexpectedReturnCode"

def check_litmus_output(litmus_test_log_name : str) -> LitmusResult:
    '''
    grep -q "\tNo error found." test-HP-arm-io-it/armv8-HP-IT-amd1-dmb-sy.out.run
    '''
    # TODO NOTE: make this command check the output, like the grep above.
    check_result_cmd = f"if grep -q \"\tNo error found.\" {litmus_test_log_name}; then exit 0; elif grep -q \"\tInvariant .* failed.\" {litmus_test_log_name};  then exit 1 else exit 2; fi"
    # Exit 0 is ordering not observed,
    # Exit 1 is ordering observed,
    # Exit 2 is an unexpected error.

    # Execute the second shell command and capture the return code
    result = subprocess.run(["zsh", "-c", check_result_cmd], shell=False)

    # Check the return code
    if result.returncode == 0:
        # TODO: Mark this entry for this litmus test for a LSQ + Transform Combo
        # as Ordering "Disallowed"
        return LitmusResult.Disallowed.value
    elif result.returncode == 1:
        # TODO: Mark this entry for this litmus test for a LSQ + Transform Combo
        # as Ordering "Allowed"
        return LitmusResult.Allowed.value
    elif result.returncode == 2:
        # TODO: This is unexpected, add a ? or something
        return LitmusResult.UnexpectedResult.value
    else:
        # TODO: Very unexpected shouldn't be able to get another value?
        # Should throw an exception here
        return LitmusResult.UnexpectedReturnCode.value

def run_litmus_test(
        a_litmus_test : str,
        murphi_src : str,
        experiment : Experiment,
        trace_dir_name : str) -> tuple[str, LitmusResult]:
    generated_litmus = f"generated-{a_litmus_test}"
    murphi_to_cpp_cmd = f"{murphi_src}/src/mu -c {generated_litmus}.m"

    mm_name = str(experiment.memory_model.value)
    lsq_name = str(experiment.lsq.value)
    tfsm_name = str(experiment.transformation.value)
    litmus_test_exe_name = f"{mm_name}-{lsq_name}-{tfsm_name}.out"
    include_murphi_path = f"CPLUS_INCLUDE_PATH={murphi_src}/include"
    compile_cpp_cmd = f"{include_murphi_path} g++ {generated_litmus}.cpp -o {litmus_test_exe_name}"

    litmus_test_log_name = f"{litmus_test_exe_name}.run"
    execute_litmus_cmd = f"./{litmus_test_exe_name} -b32 -d {trace_dir_name} -vdfs -td -m {MURPHI_MEM_NUM} >& {litmus_test_log_name}"

    run_test = f"{murphi_to_cpp_cmd} && {compile_cpp_cmd} && {execute_litmus_cmd}"
    '''
    mu -c generated-n7.m &&
    CPLUS_INCLUDE_PATH=~/documents_local/cmurphi5.5.0/include g++ generated-n7.cpp -o armv8-nosq-replay-n7.out &&
    ./armv8-nosq-replay-n7.out -b64 -d murphi-trace -vdfs -tf -m 2048 >&
    armv8-nosq-replay-n7.out.run
    '''

    try:
        # Execute the first shell command
        subprocess.run(run_test, shell=True, timeout=TIMEOUT)
    except TimeoutExpired:
        # Handle the TimeoutExpired exception here
        # You can log the timeout or perform any necessary actions
        return (a_litmus_test, LitmusResult.Timeout.value)

    return (a_litmus_test, check_litmus_output(litmus_test_log_name))

# assume for now i have all specific tests enumerated here...
TSO_TESTS = ["amd2"]
ARM_TESTS = []

# NOTE: Change this to actually execute a murphi file.
def execute_command_with_file_check(
        experiment: Experiment,
        murphi_src: str,
        parallel_batch : int,
        test_names: list[str]) -> tuple[
            # 'identifier' for this experiment.
            tuple[MemoryModel,LSQ,Transformation],
            # Litmus test results for this experiment
            dict[str,LitmusResult]
            ]:

    current_directory = os.getcwd()
    print(os.getcwd())
    execute_aqlc(experiment)

    litmus_test_dir = CreateOutputDirName(experiment)
    if not os.path.exists(litmus_test_dir):
        os.mkdir(litmus_test_dir)
    # Change to the specified directory
    os.chdir(litmus_test_dir)
    # Create the "trace" directory for murphi
    trace_dir_name = "trace"
    if not os.path.exists(trace_dir_name):
        os.mkdir(trace_dir_name)

    # ----------- NOTE: Separate the changing directories and creating the trace dir from the running.

    # Use map function to create a list of tuples with test name, murphi_src, and experiment
    tests_to_run = list(map(lambda test_name: [test_name, murphi_src, experiment, trace_dir_name], test_names))

    pool = Pool(parallel_batch)
    litmus_result_dict = dict()
    result = pool.starmap(run_litmus_test, tests_to_run)
    for (litmus, litmus_result) in result:
        litmus_result_dict[litmus] = litmus_result

    os.chdir(current_directory)

    mm_lsq_tfsm = (experiment.memory_model, experiment.lsq, experiment.transformation)
    # Return a tuple of MM and LSQ, with the litmus test results.
    return (mm_lsq_tfsm, litmus_result_dict)

def copy_file(source_path: str, destination_path: str):
    shutil.copy2(source_path, destination_path)

def MemoryModelToLower(mm : MemoryModel):
    return str.lower(str(mm.value))

def CreateTransformConfigPath(mm: MemoryModel, tfsm: Transformation, lsq: LSQ):
    # Construct a dir like: artifact-tso-lsq-tfsm-configs/IO/TransformsToApply_HP_IO.lean
    mm_name = MemoryModelToLower(mm)
    tfsm_name = str(tfsm.value)
    lsq_name = None
    match lsq:
        case LSQ.HP:
            lsq_name = str(lsq.value)
        case LSQ.LB:
            lsq_name = "LB_and_Unified"
        case LSQ.Unified:
            lsq_name = "LB_and_Unified"

    config_path = f"artifact-{mm_name}-lsq-tfsm-configs/{tfsm_name}/TransformsToApply_{lsq_name}_{tfsm_name}.lean"
    return config_path

def CreateTransformDestPath():
    return "PipelineDsl/TransformsToApply.lean"

def GetApplicableTestNames(memory_model : MemoryModel) -> list[str]:
    match memory_model:
        case MemoryModel.TSO:
            return TSO_TESTS
        case MemoryModel.ARM:
            return ARM_TESTS

def Execute(experiment : Experiment,
            murphi_src : str,
            parallel_batch : int):
    # Copy in the transformations to apply file, and execute.
    transforms_src  = CreateTransformConfigPath(
        experiment.memory_model,
        experiment.transformation,
        experiment.lsq)
    transforms_dest = CreateTransformDestPath()
    copy_file(transforms_src, transforms_dest)

    test_names = GetApplicableTestNames(experiment.memory_model)
    return execute_command_with_file_check(
        experiment,
        murphi_src,
        parallel_batch,
        test_names)

# Example usage
# experiment = Experiment(MemoryModel.TSO, LSQ.HP, Transformation.IO)
# experiment.Execute()
        

def CreateExperiments():
    return [Experiment(mm, lsq, transform) for mm in MemoryModel for transform in Transformation for lsq in LSQ]

def main():
    # experiments = CreateExperiments()
    experiments = [ Experiment(MemoryModel.TSO, LSQ.HP, Transformation.IO),
                   Experiment(MemoryModel.TSO, LSQ.LB, Transformation.IO) ]

    os.chdir("../")
    
    results_list = list()
    for exp in experiments:
        results_list.append(Execute(exp, murphi_src, parallel_batch))

    # TODO: Should be able to finish this on Thursday.
    # (1) Group results into "tables" (dict entries in python)
    #   by MM and LSQ. Columns are a specific transformation
        # i.e. IO, IO+IR, and IO+LR.
    # (2) Finish the list of litmus tests in the Lean litmustests file,
        # and here by adding the names to the list of litmus tests to run
        # per MM (TSO or ARM.)
    paper_tables_dict = dict()
    for (mm, lsq, tfsm), litmus_result in results_list:
        # print(f"MM: ({mm}), LSQ: ({lsq}), TFSM: ({tfsm})")
        if (mm.value, lsq.value) not in paper_tables_dict:
            paper_tables_dict[(mm.value, lsq.value)] = {}
        if tfsm.value not in paper_tables_dict[(mm.value, lsq.value)]:
            paper_tables_dict[(mm.value, lsq.value)][tfsm.value] = {}

        # print("Litmus Results:")
        for test_name, result in litmus_result.items():
            # print(f"Test: {test_name}, Result: {result}")
            paper_tables_dict[(mm.value, lsq.value)][tfsm.value][test_name] = result

        # # Convert paper_tables_dict into a DataFrame
        # df = pd.DataFrame.from_dict(paper_tables_dict, index=test_names)
        
        # # Transpose the DataFrame to have tfsm entries as columns
        # df = df.transpose()
    # pprint.pprint(paper_tables_dict)
    
    # Print the table
    for (mm, lsq), tfsm_dict in paper_tables_dict.items():
        print(f"=== MM: {mm}, LSQ: {lsq} ===")
        test_names = GetApplicableTestNames(mm)
        #for tfsm_val, test_result_dict in tfsm_dict.items():
            #print(f"TFSM: {tfsm_val}")
            #def make_pretty(styler):
            #    styler.set_caption(f"MM: {mm}, LSQ: {lsq}")
            #    return styler

        df = pd.DataFrame(tfsm_dict, index=test_names)
        #df.style.pipe(make_pretty)
        table = tabulate.tabulate(df, headers='keys', tablefmt='fancy_grid')
        print(table)
        # print(df.to_csv())
        # print(df.to_markdown())

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python run-litmus-on-lsqs.py <murphi_src> <parallel_batch>")
        sys.exit(1)

    murphi_src = sys.argv[1]
    parallel_batch = int(sys.argv[2])

    main()
