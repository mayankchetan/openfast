"""
    YAML-equivalence regression test: prove that a case produces identical results
    whether its module input file is in text or YAML format.

    The text inputs are converted to YAML on the fly (reg_tests/lib/yamlDeckConverter.py),
    both variants are run, and the outputs are compared. Because both formats parse
    values through the same list-directed READ, the outputs are required to be
    bit-identical — the standard rtol/atol tolerance is only a diagnostic fallback
    used to characterize a failure, never to excuse one.

    Supported modules:
      inflowwind - standalone InflowWind driver case.
      openfast   - full glue-code (.fst) case; requires a mode (per-file | all-yaml |
                   single-file) selecting how convert_fst() should transform
                   input_files:InflowFile.

    Usage: `executeYamlEquivalenceCase.py -h`
"""

import os
import sys
basepath = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.sep.join([basepath, "lib"]))
import argparse
import glob
import shutil
import numpy as np
import rtestlib as rtl
import openfastDrivers
import pass_fail
import yamlDeckConverter

parser = argparse.ArgumentParser(description="Runs a case in text and YAML input formats and verifies identical output.")
parser.add_argument("module", metavar="Module", type=str, nargs=1, help="Module under test (inflowwind | openfast).")
parser.add_argument("caseName", metavar="Case-Name", type=str, nargs=1, help="The name of the test case.")
parser.add_argument("executable", metavar="Driver", type=str, nargs=1, help="The path to the driver executable.")
parser.add_argument("sourceDirectory", metavar="path/to/openfast_repo", type=str, nargs=1, help="The path to the OpenFAST repository.")
parser.add_argument("buildDirectory", metavar="path/to/openfast_repo/build", type=str, nargs=1, help="The path to the build directory.")
parser.add_argument("mode", metavar="Mode", type=str, nargs='?', default=None,
                     help="Required for module 'openfast': per-file | all-yaml | single-file.")

args = parser.parse_args()
module = args.module[0]
caseName = args.caseName[0]
executable = args.executable[0]
sourceDirectory = args.sourceDirectory[0]
buildDirectory = args.buildDirectory[0]
mode = args.mode

rtl.validateExeOrExit(executable)
rtl.validateDirOrExit(sourceDirectory)

if module not in ("inflowwind", "openfast"):
    rtl.exitWithError("executeYamlEquivalenceCase.py: unsupported module '{}'".format(module))

if module == "openfast":
    if mode not in ("per-file", "all-yaml", "single-file"):
        rtl.exitWithError("executeYamlEquivalenceCase.py: module 'openfast' requires a mode of "
                           "per-file | all-yaml | single-file (got {!r}).".format(mode))

    #### openfast (.fst) glue-code case #############################################
    moduleDirectory = os.path.join(sourceDirectory, "reg_tests", "r-test", "glue-codes", "openfast")
    inputsDirectory = os.path.join(moduleDirectory, caseName)
    if not os.path.isdir(inputsDirectory):
        rtl.exitWithError("The test data inputs directory, {}, does not exist.".format(inputsDirectory))

    # exclude any pre-existing reference/echo/summary artifacts from the staged
    # copies: each variant must produce its own fresh output, never compare against
    # (or be confused by) a checked-in baseline
    CASE_EXCLUDE_EXT = ['.ech', '.yaml', '.sum', '.log', '.out', '.outb']
    TURBINE_DATA_DIRS = ["5MW_Baseline", "AOC", "AWT27", "_DummyTurbineData", "SWRT", "UAE_VI", "WP_Baseline"]

    # shared turbine-data directories (e.g. AWT27, referenced via "../AWT27/..." from
    # the case files) staged once directly under buildDirectory -- siblings of the
    # per-variant case directories below, mirroring executeOpenfastRegressionCase.py
    for data in TURBINE_DATA_DIRS:
        srcDataDir = os.path.join(moduleDirectory, data)
        dataDir = os.path.join(buildDirectory, data)
        if os.path.isdir(srcDataDir) and not os.path.isdir(dataDir):
            rtl.copyTree(srcDataDir, dataDir, excludeExt=CASE_EXCLUDE_EXT)

    def stageOpenfastVariant(variant):
        """Copy the case inputs into <build>/<case>_yamleq_<variant>; return the dir.
        Variant dirs sit directly under buildDirectory -- the SAME depth as the shared
        turbine-data dirs above -- so relative paths like "../AWT27/..." resolve."""
        d = os.path.join(buildDirectory, caseName + "_yamleq_" + variant)
        if os.path.isdir(d):
            shutil.rmtree(d)
        rtl.copyTree(inputsDirectory, d, excludeExt=CASE_EXCLUDE_EXT)
        return d

    PRIMARY = caseName + ".fst"
    OUTPUT = caseName + ".outb"

    ### text variant (baseline)
    textDir = stageOpenfastVariant("text")

    ### yaml variant: convert the primary file (and, for all-yaml, the referenced
    ### InflowWind file) per the requested mode
    yamlDir = stageOpenfastVariant(mode)
    yamlPrimary = caseName + ".yaml"
    yamlText, extraFiles = yamlDeckConverter.convert_fst(os.path.join(yamlDir, PRIMARY), mode)
    with open(os.path.join(yamlDir, yamlPrimary), "w") as f:
        f.write(yamlText)
    os.remove(os.path.join(yamlDir, PRIMARY))
    for relName, content in extraFiles.items():
        extraPath = os.path.join(yamlDir, relName)
        os.makedirs(os.path.dirname(extraPath) or yamlDir, exist_ok=True)
        with open(extraPath, "w") as f:
            f.write(content)

    ### run both
    for d, primaryName in ((textDir, PRIMARY), (yamlDir, yamlPrimary)):
        caseInputFile = os.path.join(d, primaryName)
        returnCode = openfastDrivers.runOpenfastCase(caseInputFile, executable)
        if returnCode != 0:
            rtl.exitWithError("Case failed to run in '{}' (exit {}).".format(d, returnCode))

    ### compare: bit-identical required
    textOut = os.path.join(textDir, OUTPUT)
    yamlOut = os.path.join(yamlDir, OUTPUT)
    rtl.validateFileOrExit(textOut)
    rtl.validateFileOrExit(yamlOut)

    textData, textInfo, _ = pass_fail.readFASTOut(textOut)
    yamlData, yamlInfo, _ = pass_fail.readFASTOut(yamlOut)

    if textData.shape != yamlData.shape:
        rtl.exitWithError("Output shapes differ: text {} vs yaml {}.".format(textData.shape, yamlData.shape))

    if np.array_equal(textData, yamlData):
        sys.exit(0)

    # not bit-identical: report the magnitude of the divergence, then fail
    diff = np.abs(textData - yamlData)
    worst = np.unravel_index(np.argmax(diff), diff.shape)
    print("YAML-equivalence FAILURE: outputs are not bit-identical.")
    print("  max |text - yaml| = {} at row {}, channel '{}'".format(
        diff[worst], worst[0], textInfo["attribute_names"][worst[1]]))
    passing = pass_fail.passing_channels(textData.T, yamlData.T, 2.0, 1.9)
    print("  channels within standard regression tolerance: {}/{}".format(np.sum(passing), passing.size))
    sys.exit(1)

#### inflowwind (standalone driver) case #########################################
moduleDirectory = os.path.join(sourceDirectory, "reg_tests", "r-test", "modules", module)
inputsDirectory = os.path.join(moduleDirectory, caseName)
if not os.path.isdir(inputsDirectory):
    rtl.exitWithError("The test data inputs directory, {}, does not exist.".format(inputsDirectory))

INPUT_GLOBS = ("*.inp", "*.bts", "*.bin", "*.wnd", "*.hh", "*.sum")
PRIMARY = "ifw_primary.inp"
DRIVER = "ifw_driver.inp"
OUTPUT = "Points.Velocity.dat"


def stage(variant):
    """Copy the case inputs into <build>/<case>_yamleq_<variant>; return the dir.
    Variant dirs sit at the same depth as a normally-staged case so that relative
    paths in the inputs (e.g. ../../../glue-codes/...) resolve identically."""
    d = os.path.join(buildDirectory, caseName + "_yamleq_" + variant)
    if os.path.isdir(d):
        shutil.rmtree(d)
    os.makedirs(d)
    for pattern in INPUT_GLOBS:
        for f in glob.glob(os.path.join(inputsDirectory, pattern)):
            shutil.copy(f, os.path.join(d, os.path.basename(f)))
    return d


### text variant
textDir = stage("text")

### yaml variant: convert the primary file, repoint the driver at it
yamlDir = stage("yaml")
yamlPrimary = PRIMARY.replace(".inp", ".yaml")
yamlText = yamlDeckConverter.convert_inflowwind(os.path.join(yamlDir, PRIMARY))
with open(os.path.join(yamlDir, yamlPrimary), "w") as f:
    f.write(yamlText)
os.remove(os.path.join(yamlDir, PRIMARY))

driverFile = os.path.join(yamlDir, DRIVER)
with open(driverFile) as f:
    driverText = f.read()
if PRIMARY not in driverText:
    rtl.exitWithError("Driver file {} does not reference {}.".format(driverFile, PRIMARY))
with open(driverFile, "w") as f:
    f.write(driverText.replace(PRIMARY, yamlPrimary))

### run both
for d in (textDir, yamlDir):
    returnCode = openfastDrivers.runInflowwindDriverCase(os.path.join(d, DRIVER), executable)
    if returnCode != 0:
        rtl.exitWithError("Case failed to run in '{}' (exit {}).".format(d, returnCode))

### compare: bit-identical required
textOut = os.path.join(textDir, OUTPUT)
yamlOut = os.path.join(yamlDir, OUTPUT)
rtl.validateFileOrExit(textOut)
rtl.validateFileOrExit(yamlOut)

textData, textInfo, _ = pass_fail.readFASTOut(textOut)
yamlData, yamlInfo, _ = pass_fail.readFASTOut(yamlOut)

if textData.shape != yamlData.shape:
    rtl.exitWithError("Output shapes differ: text {} vs yaml {}.".format(textData.shape, yamlData.shape))

if np.array_equal(textData, yamlData):
    sys.exit(0)

# not bit-identical: report the magnitude of the divergence, then fail
diff = np.abs(textData - yamlData)
worst = np.unravel_index(np.argmax(diff), diff.shape)
print("YAML-equivalence FAILURE: outputs are not bit-identical.")
print("  max |text - yaml| = {} at row {}, channel '{}'".format(
    diff[worst], worst[0], textInfo["attribute_names"][worst[1]]))
passing = pass_fail.passing_channels(textData.T, yamlData.T, 2.0, 1.9)
print("  channels within standard regression tolerance: {}/{}".format(np.sum(passing), passing.size))
sys.exit(1)
