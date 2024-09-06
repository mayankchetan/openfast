import pytest
import os.path as osp
import subprocess, sys


from openfast_io.FAST_reader import InputReader_OpenFAST
from openfast_io.FAST_writer import InputWriter_OpenFAST
from openfast_io.FAST_output_reader import FASTOutputFile

from openfast_io.FileTools import check_rtest_cloned
from pathlib import Path

from conftest import REPOSITORY_ROOT, BUILD_DIR, OF_PATH



# Exercising the  various OpenFAST modules
FOLDERS_TO_RUN = [
    "5MW_Land_BD_DLL_WTurb"                  , # "openfast;beamdyn;aerodyn;servodyn")
    "5MW_Land_BD_Init"                       , # "openfast;beamdyn;aerodyn;servodyn")"5MW_Land_DLL_WTurb"                     , # "openfast;elastodyn;aerodyn;servodyn")
    "5MW_Land_DLL_WTurb_wNacDrag"            , # "openfast;elastodyn;aerodyn;servodyn")
    "5MW_OC3Mnpl_DLL_WTurb_WavesIrr"         , # "openfast;elastodyn;aerodyn;servodyn;hydrodyn;subdyn;offshore")
    "5MW_OC3Mnpl_DLL_WTurb_WavesIrr_Restart" , # "openfast;elastodyn;aerodyn;servodyn;hydrodyn;subdyn;offshore;restart")
    "5MW_OC3Trpd_DLL_WSt_WavesReg"           , # "openfast;elastodyn;aerodyn;servodyn;hydrodyn;subdyn;offshore")
    "5MW_OC4Jckt_DLL_WTurb_WavesIrr_MGrowth" , # "openfast;elastodyn;aerodyn;servodyn;hydrodyn;subdyn;offshore")
    "5MW_ITIBarge_DLL_WTurb_WavesIrr"        , # "openfast;elastodyn;aerodyn;servodyn;hydrodyn;map;offshore")
    "5MW_TLP_DLL_WTurb_WavesIrr_WavesMulti"  , # "openfast;elastodyn;aerodyn;servodyn;hydrodyn;map;offshore")
    "5MW_OC3Spar_DLL_WTurb_WavesIrr"         , # "openfast;elastodyn;aerodyn;servodyn;hydrodyn;map;offshore")
    "5MW_OC4Semi_WSt_WavesWN"                , # "openfast;elastodyn;aerodyn;servodyn;hydrodyn;moordyn;offshore")
    "5MW_OC4Jckt_ExtPtfm"                    , # "openfast;elastodyn;extptfm")
]



def getPaths(OF_PATH = OF_PATH, REPOSITORY_ROOT = REPOSITORY_ROOT, BUILD_DIR = BUILD_DIR):

    return {
        "executable": OF_PATH,
        "source_dir": REPOSITORY_ROOT,
        "build_dir": BUILD_DIR, # Location of the reg_tests directory inside the build directory created by the user
        "rtest_dir": osp.join(REPOSITORY_ROOT, "reg_tests", "r-test"),
        "test_data_dir": osp.join(REPOSITORY_ROOT, "reg_tests", "r-test", "glue-codes", "openfast"),
        "discon_dir": osp.join(BUILD_DIR, "glue-codes", "openfast", "5MW_Baseline", "ServoData"),
    }

def read_action(folder, path_dict = getPaths()):
    print(f"Reading from {folder}")

    # Read input deck
    fast_reader = InputReader_OpenFAST()
    fast_reader.FAST_InputFile =  f'{folder}.fst'   # FAST input file (ext=.fst)
    fast_reader.FAST_directory = osp.join(path_dict['test_data_dir'], folder)   # Path to fst directory files
    fast_reader.execute()

    return fast_reader.fst_vt

def write_action(folder, fst_vt, path_dict = getPaths()):
    print(f"Writing to {folder}, with TMax = 2.0")

    # check if the folder exists, if not, mostly being called not from cmake, so create it
    if not osp.exists(osp.join(path_dict['build_dir'],'openfast_io')):
        Path(path_dict['build_dir']).mkdir(parents=True, exist_ok=True)

    fast_writer = InputWriter_OpenFAST()
    fast_writer.FAST_runDirectory = osp.join(path_dict['build_dir'],'openfast_io',folder)
    Path(fast_writer.FAST_runDirectory).mkdir(parents=True, exist_ok=True)
    fast_writer.FAST_namingOut    = folder

    fast_writer.fst_vt = dict(fst_vt)
    fst_vt = {}
    fst_vt['Fst', 'TMax'] = 2.
    fst_vt['Fst', 'TStart'] = 0.
    fst_vt['Fst','OutFileFmt'] = 3
    # check if the case needs ServoDyn
    if 'DLL_FileName' in fast_writer.fst_vt['ServoDyn']:
        # Copy the DISCON name and add the path to the build location
        fst_vt['ServoDyn', 'DLL_FileName'] = osp.join(path_dict['discon_dir'], osp.basename(fast_writer.fst_vt['ServoDyn']['DLL_FileName'])) 
    fast_writer.update(fst_update=fst_vt)
    fast_writer.execute()

def run_action(folder, path_dict = getPaths()):
    # Placeholder for the actual run action
    print(f"Running simulation for {folder}")
    command = [f"{path_dict['executable']}",  f"{osp.join(path_dict['build_dir'],'openfast_io', folder, f'{folder}.fst')}"]
    with open(osp.join(path_dict['build_dir'],'openfast_io', folder, f'{folder}.log'), 'w') as f:
        subprocess.run(command, check=True, stdout=f, stderr=subprocess.STDOUT)
        f.close()

def check_ascii_out(folder, path_dict = getPaths()):
    # Placeholder for the actual check action
    print(f"Checking ASCII output for {folder}")
    asciiOutput = osp.join(path_dict['build_dir'],'openfast_io', folder, f"{folder}.out")
    fast_outout = FASTOutputFile(filename=asciiOutput)

def check_binary_out(folder, path_dict = getPaths()):
    # Placeholder for the actual check action
    print(f"Checking binary output for {folder}")
    binaryOutput = osp.join(path_dict['build_dir'],'openfast_io', folder, f"{folder}.outb")
    fast_outout = FASTOutputFile(filename=binaryOutput)

# Begining of the test
def test_rtest_cloned(request):

    REPOSITORY_ROOT = osp.join(request.config.getoption("--source_dir"))
    path_dict = getPaths(REPOSITORY_ROOT=REPOSITORY_ROOT)

    if check_rtest_cloned(path_dict['test_data_dir']):
        assert True, "R-tests cloned properly"
    else:# stop the test if the r-tests are not cloned properly
        print("R-tests not cloned properly")
        sys.exit(1)

def test_DLLs_exist(request):

    path_dict = getPaths(OF_PATH=osp.join(request.config.getoption("--build_dir")))

    # Check if the DISCON.dll file exists
    DISCON_DLL = osp.join(path_dict['discon_dir'], "DISCON.dll")
    if osp.exists(DISCON_DLL):
        assert True, f"DISCON.dll found at {DISCON_DLL}"
    else: # stop the test if the DISCON.dll is not found
        print(f"DISCON.dll not found at {DISCON_DLL}. Please build with ''' make regression_test_controllers ''' and try again.")
        sys.exit(1)

def test_openfast_executable_exists(request):

    path_dict = getPaths(OF_PATH=osp.join(request.config.getoption("--executable")))

    if osp.exists(path_dict['executable']):
        assert True, f"OpenFAST executable found at {path_dict['executable']}"
    else: # stop the test if the OpenFAST executable is not found
        print(f"OpenFAST executable not found at {path_dict['executable']}. Please build OpenFAST and try again.")
        sys.exit(1)



# Parameterize the test function to run for each folder and action
@pytest.mark.parametrize("folder", FOLDERS_TO_RUN)
# @pytest.mark.parametrize("action_name, action_func", actions)
def test_openfast_io_read_write_run_outRead(folder, request):

    path_dict = getPaths(OF_PATH=osp.join(request.config.getoption("--executable")), 
                         REPOSITORY_ROOT=osp.join(request.config.getoption("--source_dir")), 
                         BUILD_DIR=osp.join(request.config.getoption("--build_dir")))


    try:
        # action_func(folder)
        action_name = "read"
        fst_vt = read_action(folder, path_dict = path_dict)

        action_name = "write"
        write_action(folder, fst_vt, path_dict = path_dict)

        action_name = "run"
        run_action(folder, path_dict = path_dict)

        action_name = "check ASCII"
        check_ascii_out(folder, path_dict = path_dict)

        action_name = "check binary"
        check_binary_out(folder, path_dict = path_dict)

    except Exception as e:
        pytest.fail(f"Action '{action_name}' for folder '{folder}' failed with exception: {e}")

def main():
    # Initialize any necessary setup here

    for folder in FOLDERS_TO_RUN:
        print(" ")
        print(f"Processing folder: {folder}")

        # Assuming read_action, write_action, run_action, and check_action are defined elsewhere
        data = read_action(folder)
        write_action(folder, data)
        run_action(folder)
        check_ascii_out(folder)
        check_binary_out(folder)
        print(f"Successfully processed folder: {folder}")

if __name__ == "__main__":
    main()