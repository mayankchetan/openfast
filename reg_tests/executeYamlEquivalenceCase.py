"""
    YAML-equivalence regression test: prove that a case produces identical results
    whether its module input file is in text or YAML format.

    The text inputs are converted to YAML on the fly (reg_tests/lib/yamlDeckConverter.py),
    both variants are run, and the outputs are compared. Because both formats parse
    values through the same list-directed READ, the outputs are required to be
    bit-identical — the standard rtol/atol tolerance is only a diagnostic fallback
    used to characterize a failure, never to excuse one.

    Supported modules:
      inflowwind      - standalone InflowWind driver case.
      aerodisk        - standalone AeroDisk driver case. Optional mode "driver"
                        additionally converts the .dvr driver file itself to YAML
                        (convert_aerodisk_driver) and runs a full-yaml case (yaml
                        driver -> yaml primary); omitted (default), only the
                        primary is converted and the still-text driver is repointed
                        at it. Later Wave-4 (module-driver) additions reuse this same
                        "driver" mode convention.
      simple-elastodyn - standalone Simplified ElastoDyn (SED) driver case. Optional
                        mode "driver" additionally converts the .dvr driver file
                        itself to YAML (convert_sed_driver), same convention as
                        aerodisk's driver mode above.
      seastate        - standalone SeaState driver case.
      hydrodyn        - standalone HydroDyn driver case.
      aerodyn         - standalone AeroDyn driver case.
      moordyn         - standalone MoorDyn driver case.
      beamdyn         - standalone BeamDyn driver case.
      subdyn          - standalone SubDyn driver case.
      openfast        - full glue-code (.fst) case; requires a mode (per-file |
                        all-yaml | single-file) selecting how convert_fst() should
                        transform input_files:InflowFile / input_files:AeroFile /
                        input_files:EDFile / input_files:SeaStFile / input_files:HydroFile /
                        input_files:SubFile.

    Usage: `executeYamlEquivalenceCase.py -h`
"""

import os
import re
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
parser.add_argument("module", metavar="Module", type=str, nargs=1, help="Module under test (inflowwind | aerodisk | openfast).")
parser.add_argument("caseName", metavar="Case-Name", type=str, nargs=1, help="The name of the test case.")
parser.add_argument("executable", metavar="Driver", type=str, nargs=1, help="The path to the driver executable.")
parser.add_argument("sourceDirectory", metavar="path/to/openfast_repo", type=str, nargs=1, help="The path to the OpenFAST repository.")
parser.add_argument("buildDirectory", metavar="path/to/openfast_repo/build", type=str, nargs=1, help="The path to the build directory.")
parser.add_argument("mode", metavar="Mode", type=str, nargs='?', default=None,
                     help="Required for module 'openfast': per-file | all-yaml | single-file. "
                          "Optional for standalone-driver modules (e.g. 'aerodisk'): 'driver' "
                          "additionally converts the driver file itself to YAML.")

args = parser.parse_args()
module = args.module[0]
caseName = args.caseName[0]
executable = args.executable[0]
sourceDirectory = args.sourceDirectory[0]
buildDirectory = args.buildDirectory[0]
mode = args.mode

rtl.validateExeOrExit(executable)
rtl.validateDirOrExit(sourceDirectory)

if module not in ("inflowwind", "aerodisk", "simple-elastodyn", "seastate", "hydrodyn", "aerodyn", "moordyn", "beamdyn", "subdyn", "openfast"):
    rtl.exitWithError("executeYamlEquivalenceCase.py: unsupported module '{}'".format(module))


def compareBitIdentical(textOut, yamlOut):
    """Require bit-identical output between the text and YAML variants; on a mismatch,
    report the magnitude of the divergence (diagnostic only) before failing.

    Returns (falls through) on success rather than exiting the process -- every
    module branch below used to call this as its own final statement (where exiting
    here or falling through and letting the script end were equivalent), but the
    aerodisk driver-conversion mode runs a second compareBitIdentical after the
    first, so a hard exit on success would silently skip it."""
    rtl.validateFileOrExit(textOut)
    rtl.validateFileOrExit(yamlOut)

    textData, textInfo, _ = pass_fail.readFASTOut(textOut)
    yamlData, yamlInfo, _ = pass_fail.readFASTOut(yamlOut)

    if textData.shape != yamlData.shape:
        rtl.exitWithError("Output shapes differ: text {} vs yaml {}.".format(textData.shape, yamlData.shape))

    if np.array_equal(textData, yamlData):
        return

    # not bit-identical: report the magnitude of the divergence, then fail
    diff = np.abs(textData - yamlData)
    worst = np.unravel_index(np.argmax(diff), diff.shape)
    print("YAML-equivalence FAILURE: outputs are not bit-identical.")
    print("  max |text - yaml| = {} at row {}, channel '{}'".format(
        diff[worst], worst[0], textInfo["attribute_names"][worst[1]]))
    passing = pass_fail.passing_channels(textData.T, yamlData.T, 2.0, 1.9)
    print("  channels within standard regression tolerance: {}/{}".format(np.sum(passing), passing.size))
    sys.exit(1)


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

    # Bladed-style DISCON DLLs (ServoDyn PCMode/VSContrl=5 cases, e.g.
    # 5MW_OC3Spar_DLL_WTurb_WavesIrr) are built once and staged into
    # glue-codes/openfast/5MW_Baseline/ServoData/ by the CMake "regression_test_controllers"
    # target (reg_tests/CMakeLists.txt) -- the same built-DLL location
    # executeOpenfastRegressionCase.py's own case-execution tree relies on (that tree is a
    # sibling of this one under CTEST_BINARY_DIR, populated once at build time). The
    # TURBINE_DATA_DIRS staging above copies 5MW_Baseline from the SOURCE r-test tree
    # (text/source files only, no built binaries) into this equivalence test's own build
    # tree, so any built DISCON DLL must be copied in here explicitly before a
    # DLL-controlled case can run.
    DLL_EXTS = ('.dll', '.dylib', '.so')
    builtServoDataDir = os.path.join(buildDirectory, os.pardir, os.pardir,
                                      "glue-codes", "openfast", "5MW_Baseline", "ServoData")
    localServoDataDir = os.path.join(buildDirectory, "5MW_Baseline", "ServoData")
    if os.path.isdir(builtServoDataDir) and os.path.isdir(localServoDataDir):
        for f in glob.glob(os.path.join(builtServoDataDir, "*")):
            if os.path.isfile(f) and os.path.splitext(f)[1].lower() in DLL_EXTS:
                shutil.copy(f, os.path.join(localServoDataDir, os.path.basename(f)))

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

    ### text variant (baseline); staged per mode so the three modes of one case
    ### can run concurrently under ctest -j without racing on a shared directory
    textDir = stageOpenfastVariant("text_" + mode)

    ### yaml variant: convert the primary file (and, for all-yaml, the referenced
    ### InflowWind/AeroDisk files) per the requested mode
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
    compareBitIdentical(os.path.join(textDir, OUTPUT), os.path.join(yamlDir, OUTPUT))

elif module == "inflowwind":
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
    compareBitIdentical(os.path.join(textDir, OUTPUT), os.path.join(yamlDir, OUTPUT))

elif module == "aerodisk":
    #### aerodisk (standalone driver) case ###########################################
    moduleDirectory = os.path.join(sourceDirectory, "reg_tests", "r-test", "modules", module)
    inputsDirectory = os.path.join(moduleDirectory, caseName)
    if not os.path.isdir(inputsDirectory):
        rtl.exitWithError("The test data inputs directory, {}, does not exist.".format(inputsDirectory))

    # *.inp is the ADsk primary file; *.dvr is the driver file; *.csv covers the
    # @-included rotor-performance table and time-series files the r-test case uses
    INPUT_GLOBS = ("*.inp", "*.dvr", "*.csv")
    PRIMARY = "adsk_primary.inp"
    DRIVER = "adsk_driver.dvr"
    OUTPUT = "adsk_driver.out"

    def stage(variant):
        """Copy the case inputs into <build>/<case>_yamleq_<variant>; return the dir.
        Variant dirs sit at the same depth as a normally-staged case so that relative
        paths in the inputs resolve identically."""
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
    yamlText = yamlDeckConverter.convert_aerodisk(os.path.join(yamlDir, PRIMARY))
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
        returnCode = openfastDrivers.runAerodiskDriverCase(os.path.join(d, DRIVER), executable)
        if returnCode != 0:
            rtl.exitWithError("Case failed to run in '{}' (exit {}).".format(d, returnCode))

    ### compare: bit-identical required
    compareBitIdentical(os.path.join(textDir, OUTPUT), os.path.join(yamlDir, OUTPUT))

    ### driver-conversion mode (opt-in, mode == "driver"): additionally convert the
    ### .dvr itself to YAML (convert_aerodisk_driver) and run a full-yaml case (yaml
    ### driver -> yaml primary), still checked bit-identical against the same text
    ### baseline run above. Selection convention every later Wave-4 driver reuses:
    ### the module's yaml_equiv ctest registration passes "driver" as the (otherwise
    ### module-'openfast'-only) optional positional `mode` arg; omitting it keeps the
    ### existing text-driver+yaml-primary behavior above completely unchanged.
    if mode == "driver":
        yamlDvrDir = stage("yaml_driver")

        yamlDvrPrimary = PRIMARY.replace(".inp", ".yaml")
        yamlDvrPrimaryText = yamlDeckConverter.convert_aerodisk(os.path.join(yamlDvrDir, PRIMARY))
        with open(os.path.join(yamlDvrDir, yamlDvrPrimary), "w") as f:
            f.write(yamlDvrPrimaryText)
        os.remove(os.path.join(yamlDvrDir, PRIMARY))

        yamlDriverFile = DRIVER.replace(".dvr", ".yaml")
        yamlDriverText = yamlDeckConverter.convert_aerodisk_driver(os.path.join(yamlDvrDir, DRIVER))
        with open(os.path.join(yamlDvrDir, yamlDriverFile), "w") as f:
            f.write(yamlDriverText)
        os.remove(os.path.join(yamlDvrDir, DRIVER))

        returnCode = openfastDrivers.runAerodiskDriverCase(os.path.join(yamlDvrDir, yamlDriverFile), executable)
        if returnCode != 0:
            rtl.exitWithError("Case failed to run in '{}' (exit {}).".format(yamlDvrDir, returnCode))

        compareBitIdentical(os.path.join(textDir, OUTPUT), os.path.join(yamlDvrDir, OUTPUT))

elif module == "simple-elastodyn":
    #### simple-elastodyn (standalone SED driver) case #############################
    moduleDirectory = os.path.join(sourceDirectory, "reg_tests", "r-test", "modules", module)
    inputsDirectory = os.path.join(moduleDirectory, caseName)
    if not os.path.isdir(inputsDirectory):
        rtl.exitWithError("The test data inputs directory, {}, does not exist.".format(inputsDirectory))

    # *.inp is the SED primary file; *.dvr is the driver file; *.csv covers the
    # time-series input files the r-test cases use (e.g. Free.csv, HSSBrk.csv)
    INPUT_GLOBS = ("*.inp", "*.dvr", "*.csv")
    PRIMARY = "sed_primary.inp"
    DRIVER = "sed_driver.dvr"
    OUTPUT = "sed_driver.out"

    def stage(variant):
        """Copy the case inputs into <build>/<case>_yamleq_<variant>; return the dir.
        Variant dirs sit at the same depth as a normally-staged case so that relative
        paths in the inputs resolve identically."""
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
    yamlText = yamlDeckConverter.convert_sed(os.path.join(yamlDir, PRIMARY))
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
        returnCode = openfastDrivers.runSimpleElastodynDriverCase(os.path.join(d, DRIVER), executable)
        if returnCode != 0:
            rtl.exitWithError("Case failed to run in '{}' (exit {}).".format(d, returnCode))

    ### compare: bit-identical required
    compareBitIdentical(os.path.join(textDir, OUTPUT), os.path.join(yamlDir, OUTPUT))

    ### driver-conversion mode (opt-in, mode == "driver"): additionally convert the
    ### .dvr itself to YAML (convert_sed_driver) and run a full-yaml case (yaml
    ### driver -> yaml primary), still checked bit-identical against the same text
    ### baseline run above. Same convention as aerodisk's driver mode above (Wave 4's
    ### first instance).
    if mode == "driver":
        yamlDvrDir = stage("yaml_driver")

        yamlDvrPrimary = PRIMARY.replace(".inp", ".yaml")
        yamlDvrPrimaryText = yamlDeckConverter.convert_sed(os.path.join(yamlDvrDir, PRIMARY))
        with open(os.path.join(yamlDvrDir, yamlDvrPrimary), "w") as f:
            f.write(yamlDvrPrimaryText)
        os.remove(os.path.join(yamlDvrDir, PRIMARY))

        yamlDriverFile = DRIVER.replace(".dvr", ".yaml")
        yamlDriverText = yamlDeckConverter.convert_sed_driver(os.path.join(yamlDvrDir, DRIVER))
        with open(os.path.join(yamlDvrDir, yamlDriverFile), "w") as f:
            f.write(yamlDriverText)
        os.remove(os.path.join(yamlDvrDir, DRIVER))

        returnCode = openfastDrivers.runSimpleElastodynDriverCase(os.path.join(yamlDvrDir, yamlDriverFile), executable)
        if returnCode != 0:
            rtl.exitWithError("Case failed to run in '{}' (exit {}).".format(yamlDvrDir, returnCode))

        compareBitIdentical(os.path.join(textDir, OUTPUT), os.path.join(yamlDvrDir, OUTPUT))

elif module == "seastate":
    #### seastate (standalone driver) case ###########################################
    moduleDirectory = os.path.join(sourceDirectory, "reg_tests", "r-test", "modules", module)
    inputsDirectory = os.path.join(moduleDirectory, caseName)
    if not os.path.isdir(inputsDirectory):
        rtl.exitWithError("The test data inputs directory, {}, does not exist.".format(inputsDirectory))

    # Unlike inflowwind/aerodisk/simple-elastodyn, SeaState r-test cases do not share a
    # fixed primary-file name (it varies per case: NRELOffshrBsline5MW_..._SeaState.dat,
    # seastate_input.dat, seastate.dat, ...); it is referenced from the driver file via
    # the "SeaStateInputFile" keyword, so the primary filename is discovered rather
    # than hardcoded. *.Comp covers the extra user-defined wave-frequency-components
    # file used by the WaveMod7 cases.
    INPUT_GLOBS = ("*.dat", "*.inp", "*.Comp")
    DRIVER = "seastate_driver.inp"
    OUTPUT = "seastate.SeaSt.out"

    def stage(variant):
        """Copy the case inputs into <build>/<case>_yamleq_<variant>; return the dir.
        Variant dirs sit at the same depth as a normally-staged case so that relative
        paths in the inputs resolve identically."""
        d = os.path.join(buildDirectory, caseName + "_yamleq_" + variant)
        if os.path.isdir(d):
            shutil.rmtree(d)
        os.makedirs(d)
        for pattern in INPUT_GLOBS:
            for f in glob.glob(os.path.join(inputsDirectory, pattern)):
                shutil.copy(f, os.path.join(d, os.path.basename(f)))
        return d

    def findPrimaryBaseName(driverText, driverPath):
        m = re.search(r'(?im)^\s*("[^"]*"|\'[^\']*\'|\S+)\s+SeaStateInputFile\b', driverText)
        if m is None:
            rtl.exitWithError("Could not find 'SeaStateInputFile' entry in {}.".format(driverPath))
        return os.path.basename(yamlDeckConverter._unquote(m.group(1)))

    ### text variant
    textDir = stage("text")

    ### yaml variant: discover the primary filename from the driver file, convert it,
    ### repoint the driver at the new .yaml file
    yamlDir = stage("yaml")
    driverFile = os.path.join(yamlDir, DRIVER)
    with open(driverFile) as f:
        driverText = f.read()

    primaryBase = findPrimaryBaseName(driverText, driverFile)
    yamlPrimaryBase = os.path.splitext(primaryBase)[0] + ".yaml"
    yamlText = yamlDeckConverter.convert_seastate(os.path.join(yamlDir, primaryBase))
    with open(os.path.join(yamlDir, yamlPrimaryBase), "w") as f:
        f.write(yamlText)
    os.remove(os.path.join(yamlDir, primaryBase))

    if primaryBase not in driverText:
        rtl.exitWithError("Driver file {} does not reference {}.".format(driverFile, primaryBase))
    with open(driverFile, "w") as f:
        f.write(driverText.replace(primaryBase, yamlPrimaryBase))

    ### run both
    for d in (textDir, yamlDir):
        returnCode = openfastDrivers.runSeaStateDriverCase(os.path.join(d, DRIVER), executable)
        if returnCode != 0:
            rtl.exitWithError("Case failed to run in '{}' (exit {}).".format(d, returnCode))

    ### compare: bit-identical required
    compareBitIdentical(os.path.join(textDir, OUTPUT), os.path.join(yamlDir, OUTPUT))

elif module == "hydrodyn":
    #### hydrodyn (standalone driver) case ###########################################
    moduleDirectory = os.path.join(sourceDirectory, "reg_tests", "r-test", "modules", module)
    inputsDirectory = os.path.join(moduleDirectory, caseName)
    if not os.path.isdir(inputsDirectory):
        rtl.exitWithError("The test data inputs directory, {}, does not exist.".format(inputsDirectory))

    # Like SeaState, HydroDyn r-test cases do not share a fixed primary-file name (it
    # varies per case: NRELOffshrBsline5MW_..._HydroDyn.dat, MCF_HydroDyn.dat,
    # HydroDyn.dat, ...); it is referenced from the driver file via the "HDInputFile"
    # keyword, so the primary filename is discovered rather than hardcoded. The
    # driver file's "SeaStateInputFile" entry keeps pointing at its (unconverted,
    # text-format) SeaState file -- SeaState YAML conversion is out of scope here.
    # *.1/*.3/*.hst are WAMIT potential-flow data (added mass/damping, wave excitation,
    # hydrostatic restoring), *.ss/*.ssexctn are state-space potential-flow data --
    # staged here for cases (e.g. hd_NBodyMod2) whose PotFile rootname is case-local
    # rather than pointing into the shared 5MW_Baseline/HydroData directory above.
    INPUT_GLOBS = ("*.dat", "*.inp", "*.1", "*.3", "*.hst", "*.ss", "*.ssexctn")
    DRIVER = "hd_driver.inp"
    OUTPUT = "driver.out"

    # some cases (e.g. WAMIT ones) reference potential-flow data files by a relative
    # rootname that climbs out of the case directory into the shared
    # glue-codes/openfast/5MW_Baseline/HydroData turbine-data directory; stage it once
    # at the same relative depth hd_regression uses, so PotFile paths keep resolving
    CASE_EXCLUDE_EXT = ['.ech', '.yaml', '.sum', '.log', '.out', '.outb']
    dirToCopy = os.path.join("glue-codes", "openfast", "5MW_Baseline", "HydroData")
    buildDirectoryGlue = os.path.join(buildDirectory, os.pardir, os.pardir, dirToCopy)
    if not os.path.isdir(buildDirectoryGlue):
        srcDataDir = os.path.join(sourceDirectory, "reg_tests", "r-test", dirToCopy)
        if os.path.isdir(srcDataDir):
            rtl.copyTree(srcDataDir, buildDirectoryGlue, excludeExt=CASE_EXCLUDE_EXT)

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

    def findPrimaryBaseName(driverText, driverPath):
        m = re.search(r'(?im)^\s*("[^"]*"|\'[^\']*\'|\S+)\s+HDInputFile\b', driverText)
        if m is None:
            rtl.exitWithError("Could not find 'HDInputFile' entry in {}.".format(driverPath))
        return os.path.basename(yamlDeckConverter._unquote(m.group(1)))

    ### text variant
    textDir = stage("text")

    ### yaml variant: discover the primary filename from the driver file, convert it,
    ### repoint the driver at the new .yaml file
    yamlDir = stage("yaml")
    driverFile = os.path.join(yamlDir, DRIVER)
    with open(driverFile) as f:
        driverText = f.read()

    primaryBase = findPrimaryBaseName(driverText, driverFile)
    yamlPrimaryBase = os.path.splitext(primaryBase)[0] + ".yaml"
    yamlText = yamlDeckConverter.convert_hydrodyn(os.path.join(yamlDir, primaryBase))
    with open(os.path.join(yamlDir, yamlPrimaryBase), "w") as f:
        f.write(yamlText)
    os.remove(os.path.join(yamlDir, primaryBase))

    if primaryBase not in driverText:
        rtl.exitWithError("Driver file {} does not reference {}.".format(driverFile, primaryBase))
    with open(driverFile, "w") as f:
        f.write(driverText.replace(primaryBase, yamlPrimaryBase))

    ### run both
    for d in (textDir, yamlDir):
        returnCode = openfastDrivers.runHydrodynDriverCase(os.path.join(d, DRIVER), executable)
        if returnCode != 0:
            rtl.exitWithError("Case failed to run in '{}' (exit {}).".format(d, returnCode))

    ### compare: bit-identical required
    compareBitIdentical(os.path.join(textDir, OUTPUT), os.path.join(yamlDir, OUTPUT))

elif module == "beamdyn":
    #### beamdyn (standalone driver) case #############################################
    moduleDirectory = os.path.join(sourceDirectory, "reg_tests", "r-test", "modules", module)
    inputsDirectory = os.path.join(moduleDirectory, caseName)
    if not os.path.isdir(inputsDirectory):
        rtl.exitWithError("The test data inputs directory, {}, does not exist.".format(inputsDirectory))

    # BeamDyn r-test cases all use the same fixed file names (bd_driver.inp,
    # bd_primary.inp, beam_props.inp); the primary's own name is still discovered from
    # the driver's "InputFile" entry (rather than hardcoded) for robustness. beam_props.inp
    # (BldFile, a second-order file) is staged as-is and never converted.
    INPUT_GLOBS = ("*.inp",)
    DRIVER = "bd_driver.inp"
    OUTPUT = "bd_driver.out"

    CASE_EXCLUDE_EXT = ['.ech', '.yaml', '.sum', '.log', '.out', '.outb']

    def stage(variant):
        d = os.path.join(buildDirectory, caseName + "_yamleq_" + variant)
        if os.path.isdir(d):
            shutil.rmtree(d)
        os.makedirs(d)
        for pattern in INPUT_GLOBS:
            for f in glob.glob(os.path.join(inputsDirectory, pattern)):
                shutil.copy(f, os.path.join(d, os.path.basename(f)))
        return d

    def findPrimaryBaseName(driverText, driverPath):
        m = re.search(r'(?im)^\s*("[^"]*"|\'[^\']*\'|\S+)\s+InputFile\b', driverText)
        if m is None:
            rtl.exitWithError("Could not find 'InputFile' entry in {}.".format(driverPath))
        return os.path.basename(yamlDeckConverter._unquote(m.group(1)))

    ### text variant
    textDir = stage("text")

    ### yaml variant: discover the primary filename from the driver file, convert it,
    ### repoint the driver at the new .yaml file
    yamlDir = stage("yaml")
    driverFile = os.path.join(yamlDir, DRIVER)
    with open(driverFile) as f:
        driverText = f.read()

    primaryBase = findPrimaryBaseName(driverText, driverFile)
    yamlPrimaryBase = os.path.splitext(primaryBase)[0] + ".yaml"
    yamlText = yamlDeckConverter.convert_beamdyn(os.path.join(yamlDir, primaryBase))
    with open(os.path.join(yamlDir, yamlPrimaryBase), "w") as f:
        f.write(yamlText)
    os.remove(os.path.join(yamlDir, primaryBase))

    if primaryBase not in driverText:
        rtl.exitWithError("Driver file {} does not reference {}.".format(driverFile, primaryBase))
    with open(driverFile, "w") as f:
        f.write(driverText.replace(primaryBase, yamlPrimaryBase))

    ### run both
    for d in (textDir, yamlDir):
        returnCode = openfastDrivers.runBeamdynDriverCase(os.path.join(d, DRIVER), executable)
        if returnCode != 0:
            rtl.exitWithError("Case failed to run in '{}' (exit {}).".format(d, returnCode))

    ### compare: bit-identical required
    compareBitIdentical(os.path.join(textDir, OUTPUT), os.path.join(yamlDir, OUTPUT))

elif module == "subdyn":
    #### subdyn (standalone driver) case ##############################################
    moduleDirectory = os.path.join(sourceDirectory, "reg_tests", "r-test", "modules", module)
    inputsDirectory = os.path.join(moduleDirectory, caseName)
    if not os.path.isdir(inputsDirectory):
        rtl.exitWithError("The test data inputs directory, {}, does not exist.".format(inputsDirectory))

    # Unlike BeamDyn/MoorDyn/HydroDyn, SubDyn r-test driver files are not all named
    # alike (each case's driver is "<CaseName>.dvr"), so it is discovered by glob
    # rather than hardcoded. The primary filename ("SDInputFile") and output rootname
    # ("OutRootName") are both discovered from the driver file too, for the same
    # robustness reason. *.dat/*.csv covers the primary file plus any sub-files kept
    # as paths (interface/TP motion time series, applied-load unsteady-force files).
    INPUT_GLOBS = ("*.dat", "*.dvr", "*.csv")

    def stage(variant):
        d = os.path.join(buildDirectory, caseName + "_yamleq_" + variant)
        if os.path.isdir(d):
            shutil.rmtree(d)
        os.makedirs(d)
        for pattern in INPUT_GLOBS:
            for f in glob.glob(os.path.join(inputsDirectory, pattern)):
                shutil.copy(f, os.path.join(d, os.path.basename(f)))
        return d

    def findDriverBaseName():
        drivers = glob.glob(os.path.join(inputsDirectory, "*.dvr"))
        if len(drivers) != 1:
            rtl.exitWithError("Expected exactly one *.dvr file in {} (found {}).".format(
                inputsDirectory, len(drivers)))
        return os.path.basename(drivers[0])

    def findPrimaryBaseName(driverText, driverPath):
        m = re.search(r'(?im)^\s*("[^"]*"|\'[^\']*\'|\S+)\s+SDInputFile\b', driverText)
        if m is None:
            rtl.exitWithError("Could not find 'SDInputFile' entry in {}.".format(driverPath))
        return os.path.basename(yamlDeckConverter._unquote(m.group(1)))

    def findOutRootName(driverText, driverPath):
        m = re.search(r'(?im)^\s*("[^"]*"|\'[^\']*\'|\S+)\s+OutRootName\b', driverText)
        if m is None:
            rtl.exitWithError("Could not find 'OutRootName' entry in {}.".format(driverPath))
        return yamlDeckConverter._unquote(m.group(1))

    DRIVER = findDriverBaseName()

    ### text variant
    textDir = stage("text")
    with open(os.path.join(textDir, DRIVER)) as f:
        OUTPUT = findOutRootName(f.read(), DRIVER) + ".SD.out"

    ### yaml variant: discover the primary filename from the driver file, convert it,
    ### repoint the driver at the new .yaml file
    yamlDir = stage("yaml")
    driverFile = os.path.join(yamlDir, DRIVER)
    with open(driverFile) as f:
        driverText = f.read()

    primaryBase = findPrimaryBaseName(driverText, driverFile)
    yamlPrimaryBase = os.path.splitext(primaryBase)[0] + ".yaml"
    yamlText = yamlDeckConverter.convert_subdyn(os.path.join(yamlDir, primaryBase))
    with open(os.path.join(yamlDir, yamlPrimaryBase), "w") as f:
        f.write(yamlText)
    os.remove(os.path.join(yamlDir, primaryBase))

    if primaryBase not in driverText:
        rtl.exitWithError("Driver file {} does not reference {}.".format(driverFile, primaryBase))
    with open(driverFile, "w") as f:
        f.write(driverText.replace(primaryBase, yamlPrimaryBase))

    ### run both
    for d in (textDir, yamlDir):
        returnCode = openfastDrivers.runSubdynDriverCase(os.path.join(d, DRIVER), executable)
        if returnCode != 0:
            rtl.exitWithError("Case failed to run in '{}' (exit {}).".format(d, returnCode))

    ### compare: bit-identical required
    compareBitIdentical(os.path.join(textDir, OUTPUT), os.path.join(yamlDir, OUTPUT))

elif module == "aerodyn":
    #### aerodyn (standalone driver) case #############################################
    moduleDirectory = os.path.join(sourceDirectory, "reg_tests", "r-test", "modules", module)
    inputsDirectory = os.path.join(moduleDirectory, caseName)
    if not os.path.isdir(inputsDirectory):
        rtl.exitWithError("The test data inputs directory, {}, does not exist.".format(inputsDirectory))

    # Unlike SeaState/HydroDyn, every r-test AeroDyn driver case shares the same driver
    # file name (ad_driver.dvr) and primary-file keyword ("AeroFile"), but the primary
    # filename itself still varies per case, so it is discovered rather than hardcoded
    # (mirrors executeAerodynRegressionCase.py's own handling).
    # *.csv covers cases (e.g. ad_BAR_RNAMotion) that prescribe rotor/pitch/yaw motion
    # via CreateMotion.py-generated CSV files referenced from the driver file.
    INPUT_GLOBS = ("*.dat", "*.inp", "*.dvr", "*.csv")
    DRIVER = "ad_driver.dvr"
    OUTPUT = "ad_driver.outb"

    # BAR-baseline cases reference shared airfoil/blade files by a relative path that
    # climbs out of the case directory ("../BAR_Baseline/..."); stage it once at the
    # same relative depth executeAerodynRegressionCase.py uses (a sibling of each
    # staged case directory under buildDirectory), so those paths keep resolving.
    dirToCopy = "BAR_Baseline"
    buildDirectoryBAR = os.path.join(buildDirectory, dirToCopy)
    if not os.path.isdir(buildDirectoryBAR):
        srcDataDir = os.path.join(moduleDirectory, dirToCopy)
        if os.path.isdir(srcDataDir):
            rtl.copyTree(srcDataDir, buildDirectoryBAR)

    def stage(variant):
        """Copy the case inputs into <build>/<case>_yamleq_<variant>; return the dir.
        Variant dirs sit at the same depth as a normally-staged case so that relative
        paths in the inputs (e.g. ../BAR_Baseline/...) resolve identically."""
        d = os.path.join(buildDirectory, caseName + "_yamleq_" + variant)
        if os.path.isdir(d):
            shutil.rmtree(d)
        os.makedirs(d)
        for pattern in INPUT_GLOBS:
            for f in glob.glob(os.path.join(inputsDirectory, pattern)):
                shutil.copy(f, os.path.join(d, os.path.basename(f)))
        return d

    def findPrimaryBaseName(driverText, driverPath):
        m = re.search(r'(?im)^\s*("[^"]*"|\'[^\']*\'|\S+)\s+AeroFile\b', driverText)
        if m is None:
            rtl.exitWithError("Could not find 'AeroFile' entry in {}.".format(driverPath))
        return os.path.basename(yamlDeckConverter._unquote(m.group(1)))

    def findNumTurbines(driverText):
        m = re.search(r'(?im)^\s*(\d+)\s+NumTurbines\b', driverText)
        return int(m.group(1)) if m is not None else 1

    def findNumBladesTotal(driverText, nTurbines):
        """Sum the driver's per-turbine NumBlades(i) entries (the same values the
        driver passes to AD_Init as InitInp NumBlades -- rotors may have fewer than 3
        blades, e.g. ad_QuadRotor_OLAF's 0-bladed 5th turbine). Basic-format or
        single-rotor drivers may carry a plain NumBlades or none at all; default 3 per
        turbine then (all such r-test cases are 3-bladed)."""
        vals = re.findall(r'(?im)^\s*(\d+)\s+NumBlades(?:\(\d+\))?\b', driverText)
        if len(vals) == nTurbines:
            return sum(int(v) for v in vals)
        return 3 * nTurbines

    ### text variant
    textDir = stage("text")

    ### yaml variant: discover the primary filename from the driver file, convert it,
    ### repoint the driver at the new .yaml file
    yamlDir = stage("yaml")
    driverFile = os.path.join(yamlDir, DRIVER)
    with open(driverFile) as f:
        driverText = f.read()

    nTurbines = findNumTurbines(driverText)
    nBladesTotal = findNumBladesTotal(driverText, nTurbines)
    primaryBase = findPrimaryBaseName(driverText, driverFile)
    yamlPrimaryBase = os.path.splitext(primaryBase)[0] + ".yaml"
    yamlText = yamlDeckConverter.convert_aerodyn(os.path.join(yamlDir, primaryBase),
                                                 n_rotors=nTurbines, num_blades_total=nBladesTotal)
    with open(os.path.join(yamlDir, yamlPrimaryBase), "w") as f:
        f.write(yamlText)
    os.remove(os.path.join(yamlDir, primaryBase))

    if primaryBase not in driverText:
        rtl.exitWithError("Driver file {} does not reference {}.".format(driverFile, primaryBase))
    with open(driverFile, "w") as f:
        f.write(driverText.replace(primaryBase, yamlPrimaryBase))

    ### run both
    for d in (textDir, yamlDir):
        returnCode = openfastDrivers.runAerodynDriverCase(os.path.join(d, DRIVER), executable)
        if returnCode != 0:
            rtl.exitWithError("Case failed to run in '{}' (exit {}).".format(d, returnCode))

    ### compare: bit-identical required
    compareBitIdentical(os.path.join(textDir, OUTPUT), os.path.join(yamlDir, OUTPUT))

elif module == "moordyn":
    #### moordyn (standalone driver) case ############################################
    moduleDirectory = os.path.join(sourceDirectory, "reg_tests", "r-test", "modules", module)
    inputsDirectory = os.path.join(moduleDirectory, caseName)
    if not os.path.isdir(inputsDirectory):
        rtl.exitWithError("The test data inputs directory, {}, does not exist.".format(inputsDirectory))

    # Every r-test MoorDyn driver case shares the same driver file name (md_driver.inp)
    # and primary-file keyword ("MDInputFile"), but the primary filename itself is still
    # discovered from the driver file for robustness. *.dat covers the primary file plus
    # any sub-files kept as paths (platform-motion time series, WaterKin files, Syrope
    # working-curve files, bathymetry grids, EA/BA lookup tables).
    INPUT_GLOBS = ("*.dat", "*.inp", "*.txt")
    DRIVER = "md_driver.inp"
    OUTPUT = "driver.MD.out"

    def stage(variant):
        """Copy the case inputs into <build>/<case>_yamleq_<variant>; return the dir.
        Variant dirs sit at the same depth as a normally-staged case so that relative
        paths in the inputs resolve identically."""
        d = os.path.join(buildDirectory, caseName + "_yamleq_" + variant)
        if os.path.isdir(d):
            shutil.rmtree(d)
        os.makedirs(d)
        for pattern in INPUT_GLOBS:
            for f in glob.glob(os.path.join(inputsDirectory, pattern)):
                shutil.copy(f, os.path.join(d, os.path.basename(f)))
        return d

    def findPrimaryBaseName(driverText, driverPath):
        m = re.search(r'(?im)^\s*("[^"]*"|\'[^\']*\'|\S+)\s+MDInputFile\b', driverText)
        if m is None:
            rtl.exitWithError("Could not find 'MDInputFile' entry in {}.".format(driverPath))
        return os.path.basename(yamlDeckConverter._unquote(m.group(1)))

    ### text variant
    textDir = stage("text")

    ### yaml variant: discover the primary filename from the driver file, convert it,
    ### repoint the driver at the new .yaml file
    yamlDir = stage("yaml")
    driverFile = os.path.join(yamlDir, DRIVER)
    with open(driverFile) as f:
        driverText = f.read()

    primaryBase = findPrimaryBaseName(driverText, driverFile)
    yamlPrimaryBase = os.path.splitext(primaryBase)[0] + ".yaml"
    yamlText = yamlDeckConverter.convert_moordyn(os.path.join(yamlDir, primaryBase))
    with open(os.path.join(yamlDir, yamlPrimaryBase), "w") as f:
        f.write(yamlText)
    os.remove(os.path.join(yamlDir, primaryBase))

    if primaryBase not in driverText:
        rtl.exitWithError("Driver file {} does not reference {}.".format(driverFile, primaryBase))
    with open(driverFile, "w") as f:
        f.write(driverText.replace(primaryBase, yamlPrimaryBase))

    ### run both
    for d in (textDir, yamlDir):
        returnCode = openfastDrivers.runMoordynDriverCase(os.path.join(d, DRIVER), executable)
        if returnCode != 0:
            rtl.exitWithError("Case failed to run in '{}' (exit {}).".format(d, returnCode))

    ### compare: bit-identical required
    compareBitIdentical(os.path.join(textDir, OUTPUT), os.path.join(yamlDir, OUTPUT))
