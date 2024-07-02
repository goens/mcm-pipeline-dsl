

from enum import Enum

import os
import subprocess
import shutil
import os

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
    match experiment.transform:
        case Transformation.IO, Transformation.LR:
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
    lsq_file_name = f"{experiment.lsq}-for-{transform_name}.aql"

    aql_file_arg = f"Examples/graph-prototype/operational-axiomatic/{lsq_dir_name}/{lsq_file_name}"

    command = "lake exe aqlc"
    # Construct the full command
    full_command = f"{command} {lsq_arg} {transform_arg} {out_dir_arg} {model_check_arg} {aql_file_arg}"

    # Execute the shell command
    subprocess.run(full_command, shell=True)

GENERATED_TEST_FILE = "generated-test.m"
# assume for now i have all specific tests enumerated here...
TSO_TESTS = []
ARM_TESTS = []

# NOTE: Change this to actually execute a murphi file.
def execute_command_with_file_check(experiement: Experiment, command1: str, command2: str,
                                    murphi_src: str) -> bool:
    current_directory = os.getcwd()

    litmus_test_dir = CreateOutputDirName(experiement)
    # Change to the specified directory
    os.chdir(litmus_test_dir)
    # Create the "trace" directory for murphi
    trace_dir_name = "trace"
    os.mkdir(trace_dir_name)

    # ----------- NOTE: Separate the changing directories and creating the trace dir from the running.

    generated_litmus = f"generated-{a_litmus_test}"
    murphi_to_cpp_cmd = f"{murphi_src}/mu -c {generated_litmus}.m"

    mm_name = str(experiement.memory_model)
    lsq_name = str(experiement.lsq)
    tfsm_name = str(experiement.transformation)
    litmus_test_exe_name = f"{mm_name}-{lsq_name}-{tfsm_name}.out"
    include_murphi_path = f"CPLUS_INCLUDE_PATH={murphi_src}/cmurphi5.5.0/include"
    compile_cpp_cmd = f"{include_murphi_path} g++ {generated_litmus}.cpp -o {litmus_test_exe_name}"

    litmus_test_log_name = f"{litmus_test_exe_name}.run"
    execute_litmus_cmd = f"./{litmus_test_exe_name} -b32 -d {trace_dir_name} -vdfs -td -m 12480 >& {litmus_test_log_name}"

    run_test = f"{murphi_to_cpp_cmd} && {compile_cpp_cmd} && {execute_litmus_cmd}"
    '''
    mu -c generated-n7.m &&
    CPLUS_INCLUDE_PATH=~/documents_local/cmurphi5.5.0/include g++ generated-n7.cpp -o armv8-nosq-replay-n7.out &&
    ./armv8-nosq-replay-n7.out -b64 -d murphi-trace -vdfs -tf -m 2048 >&
    armv8-nosq-replay-n7.out.run
    '''

    # Execute the first shell command
    subprocess.run(run_test, shell=True)

    '''
    grep -q "\tNo error found." test-HP-arm-io-it/armv8-HP-IT-amd1-dmb-sy.out.run
    '''
    # TODO NOTE: make this command check the output, like the grep above.
    check_result_cmd = "if grep -q \"\tNo error found.\" armv8-HP-IT-amd1.out.run; then exit 0; elifgrep -q \"\tInvariant .* failed.\" armv8-HP-IT-amd1.out.run;  then exit 1 else exit 2; fi"

    # Execute the second shell command and capture the return code
    result = subprocess.run(check_result_cmd, shell=True)

    # Check the return code
    if result.returncode == 0:
        return True  # Command executed successfully
    else:
        return False  # Command returned an error

# Example usage
# directory = "/path/to/subdirectory"
# command1 = "command1"
# command2 = "command2"
# success = execute_command_with_file_check(directory, command1, command2)
# if success:
#     print("Command executed successfully")
# else:
#     print("Command returned an error")

def copy_file(source_path: str, destination_path: str):
    shutil.copy2(source_path, destination_path)

def MemoryModelToLower(mm : MemoryModel):
    return str(mm).lower

def CreateTransformConfigPath(mm: MemoryModel, tfsm: Transformation, lsq: LSQ):
    # Construct a dir like: artifact-tso-lsq-tfsm-configs/IO/TransformsToApply_HP_IO.lean
    mm_name = MemoryModelToLower(mm)
    tfsm_name = str(tfsm)
    lsq_name = None
    match lsq:
        case LSQ.HP:
            lsq_name = str(lsq)
        case LSQ.LB, LSQ.Unified:
            lsq_name = "LB_and_Unified"

    config_path = f"artifact-{mm_name}-lsq-tfsm-configs/{tfsm_name}/TransformsToApply_{lsq_name}_{tfsm_name}.lean"
    return config_path

def CreateTransformDestPath():
    return "PipelineDsl/TransformsToApply.lean"

def Execute(self):
    # Copy in the transformations to apply file, and execute.
    transforms_src  = CreateTransformConfigPath(self.memory_model, self.transformation, self.lsq)
    transforms_dest = CreateTransformDestPath()
    copy_file(transforms_src, transforms_dest)

    match self.memory_model:
        case MemoryModel.TSO:
            # Handle TSO case
            pass
        case MemoryModel.ARM:
            # Handle ARM case
            pass

# Example usage
# experiment = Experiment(MemoryModel.TSO, LSQ.HP, Transformation.IO)
# experiment.Execute()
        

def CreateExperiments():
    return [Experiment(mm, lsq, transform) for mm in MemoryModel for transform in Transformation for lsq in LSQ]

def main():
    # experiments = CreateExperiments()
    experiments = [ Experiment(MemoryModel.TSO, LSQ.HP, Transformation.IO) ]

    # Perform other operations with the experiments list

if __name__ == "__main__":
    main()
