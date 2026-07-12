#
# Copyright 2017 National Renewable Energy Laboratory
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

#===============================================================================
# Generic test functions
#===============================================================================

function(regression TEST_SCRIPT EXECUTABLE SOURCE_DIRECTORY BUILD_DIRECTORY STEADYSTATE_FLAG TESTNAME LABEL OTHER_FLAGS)

  file(TO_NATIVE_PATH "${EXECUTABLE}" EXECUTABLE)
  file(TO_NATIVE_PATH "${TEST_SCRIPT}" TEST_SCRIPT)
  file(TO_NATIVE_PATH "${SOURCE_DIRECTORY}" SOURCE_DIRECTORY)
  file(TO_NATIVE_PATH "${BUILD_DIRECTORY}" BUILD_DIRECTORY)

  string(REPLACE "\\" "\\\\" EXECUTABLE ${EXECUTABLE})
  string(REPLACE "\\" "\\\\" TEST_SCRIPT ${TEST_SCRIPT})
  string(REPLACE "\\" "\\\\" SOURCE_DIRECTORY ${SOURCE_DIRECTORY})
  string(REPLACE "\\" "\\\\" BUILD_DIRECTORY ${BUILD_DIRECTORY})

  set(PLOT_FLAG "")
  if(CTEST_PLOT_ERRORS)
    set(PLOT_FLAG "-p")
  endif()

  set(RUN_VERBOSE_FLAG "")
  if(CTEST_RUN_VERBOSE_FLAG)
    set(RUN_VERBOSE_FLAG "-v")
  endif()

  set(TESTDIR ${TESTNAME})

  set(extra_args ${ARGN})
  list(LENGTH extra_args n_args)
  if(n_args EQUAL 1)
    set(TESTDIR ${extra_args})
  endif()

  set(NO_RUN_FLAG "")
  if(CTEST_NO_RUN_FLAG)
    set(NO_RUN_FLAG "-n")
  endif()

  if(STEADYSTATE_FLAG STREQUAL " ")
    set(STEADYSTATE_FLAG "")
  endif()

  if(OTHER_FLAGS STREQUAL " ")
    set(OTHER_FLAGS "")
  endif()

  add_test(
    ${TESTNAME} ${Python_EXECUTABLE}
       ${TEST_SCRIPT}
       ${TESTDIR}
       ${EXECUTABLE}
       ${SOURCE_DIRECTORY}              # openfast source directory
       ${BUILD_DIRECTORY}               # build directory for test
       ${CTEST_RTEST_RTOL}
       ${CTEST_RTEST_ATOL}
       ${PLOT_FLAG}                     # empty or "-p"
       ${RUN_VERBOSE_FLAG}              # empty or "-v"
       ${NO_RUN_FLAG}                   # empty or "-n"
       ${STEADYSTATE_FLAG}              # empty or "-steadystate"
       ${OTHER_FLAGS}
  )
  # limit each test to 90 minutes: 5400s
  set_tests_properties(${TESTNAME} PROPERTIES TIMEOUT 5400 WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}" LABELS "${LABEL}")
endfunction(regression)

#===============================================================================
# Module specific regression test calls
#===============================================================================

# openfast
function(of_regression TESTNAME LABEL)
  set(TEST_SCRIPT "${CMAKE_CURRENT_LIST_DIR}/executeOpenfastRegressionCase.py")
  set(OPENFAST_EXECUTABLE "${CTEST_OPENFAST_EXECUTABLE}")
  set(SOURCE_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}/..")
  set(BUILD_DIRECTORY "${CTEST_BINARY_DIR}/glue-codes/openfast")
  regression(${TEST_SCRIPT} ${OPENFAST_EXECUTABLE} ${SOURCE_DIRECTORY} ${BUILD_DIRECTORY} " " ${TESTNAME} "${LABEL}" " ")
endfunction(of_regression)

function(of_aeromap_regression TESTNAME LABEL)
  set(TEST_SCRIPT "${CMAKE_CURRENT_LIST_DIR}/executeOpenfastRegressionCase.py")
  set(OPENFAST_EXECUTABLE "${CTEST_OPENFAST_EXECUTABLE}")
  set(SOURCE_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}/..")
  set(BUILD_DIRECTORY "${CTEST_BINARY_DIR}/glue-codes/openfast")
  set(STEADYSTATE_FLAG "-steadystate")
  regression(${TEST_SCRIPT} ${OPENFAST_EXECUTABLE} ${SOURCE_DIRECTORY} ${BUILD_DIRECTORY} ${STEADYSTATE_FLAG} ${TESTNAME} "${LABEL}" " ")
endfunction(of_aeromap_regression)

function(of_fastlib_regression TESTNAME LABEL)
  set(TEST_SCRIPT "${CMAKE_CURRENT_LIST_DIR}/executeOpenfastRegressionCase.py")
  set(OPENFAST_EXECUTABLE "${CMAKE_BINARY_DIR}/glue-codes/openfast/openfast_lib_driver")
  set(SOURCE_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}/..")
  set(BUILD_DIRECTORY "${CTEST_BINARY_DIR}/glue-codes/openfast")
  # extra flag in call to "regression" on next line sets the ${TESTDIR}
  regression(${TEST_SCRIPT} ${OPENFAST_EXECUTABLE} ${SOURCE_DIRECTORY} ${BUILD_DIRECTORY} " " "${TESTNAME}_fastlib" "${LABEL}" " " ${TESTNAME})
endfunction(of_fastlib_regression)

# openfast aeroacoustic
function(of_regression_aeroacoustic TESTNAME LABEL)
  set(TEST_SCRIPT "${CMAKE_CURRENT_LIST_DIR}/executeOpenfastAeroAcousticRegressionCase.py")
  set(OPENFAST_EXECUTABLE "${CTEST_OPENFAST_EXECUTABLE}")
  set(SOURCE_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}/..")
  set(BUILD_DIRECTORY "${CTEST_BINARY_DIR}/glue-codes/openfast")
  regression(${TEST_SCRIPT} ${OPENFAST_EXECUTABLE} ${SOURCE_DIRECTORY} ${BUILD_DIRECTORY} " " ${TESTNAME} "${LABEL}" " ")
endfunction(of_regression_aeroacoustic)

# FAST Farm
function(ff_regression TESTNAME OTHER_FLAGS LABEL)
  set(TEST_SCRIPT "${CMAKE_CURRENT_LIST_DIR}/executeFASTFarmRegressionCase.py")
  set(FASTFARM_EXECUTABLE "${CTEST_FASTFARM_EXECUTABLE}")
  set(SOURCE_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}/..")
  set(BUILD_DIRECTORY "${CTEST_BINARY_DIR}/glue-codes/fast-farm")
  set(OTHER_FLAGS "${OTHER_FLAGS}")    # Set name of file to compare, otherwise default
  regression(${TEST_SCRIPT} ${FASTFARM_EXECUTABLE} ${SOURCE_DIRECTORY} ${BUILD_DIRECTORY} " " ${TESTNAME} "${LABEL}" "${OTHER_FLAGS}")
endfunction(ff_regression)

# openfast linearized
function(of_regression_linear TESTNAME OTHER_FLAGS LABEL)
  set(TEST_SCRIPT "${CMAKE_CURRENT_LIST_DIR}/executeOpenfastLinearRegressionCase.py")
  set(OPENFAST_EXECUTABLE "${CTEST_OPENFAST_EXECUTABLE}")
  set(SOURCE_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}/..")
  set(BUILD_DIRECTORY "${CTEST_BINARY_DIR}/glue-codes/openfast")
  set(OTHER_FLAGS "${OTHER_FLAGS}")
  regression(${TEST_SCRIPT} ${OPENFAST_EXECUTABLE} ${SOURCE_DIRECTORY} ${BUILD_DIRECTORY} " " ${TESTNAME} "${LABEL}" "${OTHER_FLAGS}")
endfunction(of_regression_linear)

# openfast C++ interface
function(of_cpp_interface_regression TESTNAME LABEL)
  set(TEST_SCRIPT "${CMAKE_CURRENT_LIST_DIR}/executeOpenfastCppRegressionCase.py")
  set(OPENFAST_CPP_EXECUTABLE "${CTEST_OPENFASTCPP_EXECUTABLE}")
  set(SOURCE_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}/..")
  set(BUILD_DIRECTORY "${CTEST_BINARY_DIR}/glue-codes/openfast-cpp")
  regression(${TEST_SCRIPT} ${OPENFAST_CPP_EXECUTABLE} ${SOURCE_DIRECTORY} ${BUILD_DIRECTORY} " " ${TESTNAME} "${LABEL}" " ")
endfunction(of_cpp_interface_regression)

# openfast Python-interface
function(of_regression_py TESTNAME LABEL)
  set(TEST_SCRIPT "${CMAKE_CURRENT_LIST_DIR}/executePythonRegressionCase.py")
  set(EXECUTABLE "None")
  set(SOURCE_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}/..")
  set(BUILD_DIRECTORY "${CTEST_BINARY_DIR}/glue-codes/python")
  regression(${TEST_SCRIPT} ${EXECUTABLE} ${SOURCE_DIRECTORY} ${BUILD_DIRECTORY} " " ${TESTNAME} "${LABEL}" " ")
endfunction(of_regression_py)

# aerodyn
function(ad_regression TESTNAME LABEL)
  set(TEST_SCRIPT "${CMAKE_CURRENT_LIST_DIR}/executeAerodynRegressionCase.py")
  set(AERODYN_EXECUTABLE "${CTEST_AERODYN_EXECUTABLE}")
  set(SOURCE_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}/..")
  set(BUILD_DIRECTORY "${CTEST_BINARY_DIR}/modules/aerodyn")
  regression(${TEST_SCRIPT} ${AERODYN_EXECUTABLE} ${SOURCE_DIRECTORY} ${BUILD_DIRECTORY} " " ${TESTNAME} "${LABEL}" " ")
endfunction(ad_regression)

# aerodyn-Py
function(py_ad_regression TESTNAME LABEL)
  set(TEST_SCRIPT "${CMAKE_CURRENT_LIST_DIR}/executeAerodynPyRegressionCase.py")
  set(AERODYN_EXECUTABLE "${Python_EXECUTABLE}")
  set(SOURCE_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}/..")
  set(BUILD_DIRECTORY "${CTEST_BINARY_DIR}/modules/aerodyn")
  regression(${TEST_SCRIPT} ${AERODYN_EXECUTABLE} ${SOURCE_DIRECTORY} ${BUILD_DIRECTORY} " " ${TESTNAME} "${LABEL}" " ")
endfunction(py_ad_regression)


# UnsteadyAero driver
function(ua_regression TESTNAME LABEL)
  set(TEST_SCRIPT "${CMAKE_CURRENT_LIST_DIR}/executeUnsteadyAeroRegressionCase.py")
  set(AERODYN_EXECUTABLE "${CTEST_UADRIVER_EXECUTABLE}")
  set(SOURCE_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}/..")
  set(BUILD_DIRECTORY "${CTEST_BINARY_DIR}/modules/unsteadyaero")
  regression(${TEST_SCRIPT} ${AERODYN_EXECUTABLE} ${SOURCE_DIRECTORY} ${BUILD_DIRECTORY} " " ${TESTNAME} "${LABEL}" " ")
endfunction(ua_regression)


# beamdyn
function(bd_regression TESTNAME LABEL)
  set(TEST_SCRIPT "${CMAKE_CURRENT_LIST_DIR}/executeBeamdynRegressionCase.py")
  set(BEAMDYN_EXECUTABLE "${CTEST_BEAMDYN_EXECUTABLE}")
  set(SOURCE_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}/..")
  set(BUILD_DIRECTORY "${CTEST_BINARY_DIR}/modules/beamdyn")
  regression(${TEST_SCRIPT} ${BEAMDYN_EXECUTABLE} ${SOURCE_DIRECTORY} ${BUILD_DIRECTORY} " " ${TESTNAME} "${LABEL}" " ")
endfunction(bd_regression)

# hydrodyn
function(hd_regression TESTNAME LABEL)
  set(TEST_SCRIPT "${CMAKE_CURRENT_LIST_DIR}/executeHydrodynRegressionCase.py")
  set(HYDRODYN_EXECUTABLE "${CTEST_HYDRODYN_EXECUTABLE}")
  set(SOURCE_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}/..")
  set(BUILD_DIRECTORY "${CTEST_BINARY_DIR}/modules/hydrodyn")
  regression(${TEST_SCRIPT} ${HYDRODYN_EXECUTABLE} ${SOURCE_DIRECTORY} ${BUILD_DIRECTORY} " " ${TESTNAME} "${LABEL}" " ")
endfunction(hd_regression)

# py_hydrodyn
function(py_hd_regression TESTNAME LABEL)
  set(TEST_SCRIPT "${CMAKE_CURRENT_LIST_DIR}/executeHydrodynPyRegressionCase.py")
  set(HYDRODYN_EXECUTABLE "${Python_EXECUTABLE}")
  set(SOURCE_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}/..")
  set(BUILD_DIRECTORY "${CTEST_BINARY_DIR}/modules/hydrodyn")
  regression(${TEST_SCRIPT} ${HYDRODYN_EXECUTABLE} ${SOURCE_DIRECTORY} ${BUILD_DIRECTORY} " " ${TESTNAME} "${LABEL}" " ")
endfunction(py_hd_regression)

# subdyn
function(sd_regression TESTNAME LABEL)
  set(TEST_SCRIPT "${CMAKE_CURRENT_LIST_DIR}/executeSubdynRegressionCase.py")
  set(SUBDYN_EXECUTABLE "${CTEST_SUBDYN_EXECUTABLE}")
  set(SOURCE_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}/..")
  set(BUILD_DIRECTORY "${CTEST_BINARY_DIR}/modules/subdyn")
  regression(${TEST_SCRIPT} ${SUBDYN_EXECUTABLE} ${SOURCE_DIRECTORY} ${BUILD_DIRECTORY} " " ${TESTNAME} "${LABEL}" " ")
endfunction(sd_regression)

# inflowwind
function(ifw_regression TESTNAME LABEL)
  set(TEST_SCRIPT "${CMAKE_CURRENT_LIST_DIR}/executeInflowwindRegressionCase.py")
  set(INFLOWWIND_EXECUTABLE "${CTEST_INFLOWWIND_EXECUTABLE}")
  set(SOURCE_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}/..")
  set(BUILD_DIRECTORY "${CTEST_BINARY_DIR}/modules/inflowwind")
  regression(${TEST_SCRIPT} ${INFLOWWIND_EXECUTABLE} ${SOURCE_DIRECTORY} ${BUILD_DIRECTORY} " " ${TESTNAME} "${LABEL}" " ")
endfunction(ifw_regression)

# yaml-equivalence: run a case with text and YAML input files and require
# bit-identical outputs (the text inputs are converted to YAML on the fly)
function(yaml_equiv MODULE CASENAME EXECUTABLE LABEL)
  set(TEST_SCRIPT "${CMAKE_CURRENT_LIST_DIR}/executeYamlEquivalenceCase.py")
  set(SOURCE_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}/..")
  set(BUILD_DIRECTORY "${CTEST_BINARY_DIR}/modules/${MODULE}")
  add_test(
    yaml_equiv_${CASENAME} ${Python_EXECUTABLE}
       ${TEST_SCRIPT}
       ${MODULE}
       ${CASENAME}
       ${EXECUTABLE}
       ${SOURCE_DIRECTORY}
       ${BUILD_DIRECTORY}
  )
  set_tests_properties(yaml_equiv_${CASENAME} PROPERTIES TIMEOUT 5400 WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}" LABELS "${LABEL}")
endfunction(yaml_equiv)

# yaml-equivalence for openfast (.fst) glue-code cases: like yaml_equiv, but the
# primary (.fst) file is converted per MODE (perfile | allyaml | singlefile),
# selecting how convert_fst() / executeYamlEquivalenceCase.py handles the
# InflowFile entry under input_files (path-only, converted-to-YAML sibling file,
# or inlined mapping, respectively -- see reg_tests/lib/yamlDeckConverter.py).
function(yaml_equiv_openfast CASENAME MODE EXECUTABLE LABEL)
  set(TEST_SCRIPT "${CMAKE_CURRENT_LIST_DIR}/executeYamlEquivalenceCase.py")
  set(SOURCE_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}/..")
  set(BUILD_DIRECTORY "${CTEST_BINARY_DIR}/modules/openfast")
  if(MODE STREQUAL "perfile")
    set(MODE_ARG "per-file")
  elseif(MODE STREQUAL "allyaml")
    set(MODE_ARG "all-yaml")
  elseif(MODE STREQUAL "singlefile")
    set(MODE_ARG "single-file")
  else()
    message(FATAL_ERROR "yaml_equiv_openfast: unknown MODE '${MODE}' (expected perfile|allyaml|singlefile)")
  endif()
  add_test(
    yaml_equiv_${CASENAME}_${MODE} ${Python_EXECUTABLE}
       ${TEST_SCRIPT}
       "openfast"
       ${CASENAME}
       ${EXECUTABLE}
       ${SOURCE_DIRECTORY}
       ${BUILD_DIRECTORY}
       ${MODE_ARG}
  )
  set_tests_properties(yaml_equiv_${CASENAME}_${MODE} PROPERTIES TIMEOUT 5400 WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}" LABELS "${LABEL}")
endfunction(yaml_equiv_openfast)

# yaml-equivalence, driver-conversion mode: like yaml_equiv, but additionally converts
# the standalone driver's own input file (.dvr/.inp/...) to YAML and runs a full-yaml
# case (yaml driver -> yaml primary), bit-identical against the same text baseline.
# Selection convention (Wave 4, module drivers): reuses executeYamlEquivalenceCase.py's
# existing (module-'openfast'-only) optional positional MODE arg with the value
# "driver" -- every later Wave-4 driver's converter (convert_<mod>_driver in
# yamlDeckConverter.py) and CTestList.cmake registration follows this same
# yaml_equiv(...) + yaml_equiv_driver(...) pairing (see aerodisk below, the first
# instance). The test name is suffixed "_driver" so it never collides with the
# sibling text-driver+yaml-primary case's yaml_equiv_${CASENAME} test.
function(yaml_equiv_driver MODULE CASENAME EXECUTABLE LABEL)
  set(TEST_SCRIPT "${CMAKE_CURRENT_LIST_DIR}/executeYamlEquivalenceCase.py")
  set(SOURCE_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}/..")
  set(BUILD_DIRECTORY "${CTEST_BINARY_DIR}/modules/${MODULE}")
  add_test(
    yaml_equiv_${CASENAME}_driver ${Python_EXECUTABLE}
       ${TEST_SCRIPT}
       ${MODULE}
       ${CASENAME}
       ${EXECUTABLE}
       ${SOURCE_DIRECTORY}
       ${BUILD_DIRECTORY}
       "driver"
  )
  set_tests_properties(yaml_equiv_${CASENAME}_driver PROPERTIES TIMEOUT 5400 WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}" LABELS "${LABEL}")
endfunction(yaml_equiv_driver)

# smoke test for the curated hand-written single-file YAML example deck
# (comments, !include, anchor/alias, inline InflowWind) in reg_tests/yaml-examples/
function(yaml_example_smoke EXECUTABLE LABEL)
  set(TEST_SCRIPT "${CMAKE_CURRENT_LIST_DIR}/executeYamlExampleSmoke.py")
  set(RTEST_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}/r-test")
  set(BUILD_DIRECTORY "${CTEST_BINARY_DIR}/modules/openfast")
  add_test(
    yaml_example_smoke ${Python_EXECUTABLE}
       ${TEST_SCRIPT}
       ${EXECUTABLE}
       ${RTEST_DIRECTORY}
       ${BUILD_DIRECTORY}
  )
  set_tests_properties(yaml_example_smoke PROPERTIES TIMEOUT 5400 WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}" LABELS "${LABEL}")
endfunction(yaml_example_smoke)

# py_inflowwind
function(py_ifw_regression TESTNAME LABEL)
  set(TEST_SCRIPT "${CMAKE_CURRENT_LIST_DIR}/executeInflowwindPyRegressionCase.py")
  set(INFLOWWIND_EXECUTABLE "${Python_EXECUTABLE}")
  set(SOURCE_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}/..")
  set(BUILD_DIRECTORY "${CTEST_BINARY_DIR}/modules/inflowwind")
  regression(${TEST_SCRIPT} ${INFLOWWIND_EXECUTABLE} ${SOURCE_DIRECTORY} ${BUILD_DIRECTORY} " " ${TESTNAME} "${LABEL}" " ")
endfunction(py_ifw_regression)

# seastate
function(seast_regression TESTNAME LABEL)
  set(TEST_SCRIPT "${CMAKE_CURRENT_LIST_DIR}/executeSeaStateRegressionCase.py")
  set(SEASTATE_EXECUTABLE "${CTEST_SEASTATE_EXECUTABLE}")
  set(SOURCE_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}/..")
  set(BUILD_DIRECTORY "${CTEST_BINARY_DIR}/modules/seastate")
  regression(${TEST_SCRIPT} ${SEASTATE_EXECUTABLE} ${SOURCE_DIRECTORY} ${BUILD_DIRECTORY} " " ${TESTNAME} "${LABEL}" " ")
endfunction(seast_regression)

# py_seastate
function(py_seast_regression TESTNAME LABEL)
  set(TEST_SCRIPT "${CMAKE_CURRENT_LIST_DIR}/executeSeaStatePyRegressionCase.py")
  set(SEASTATE_EXECUTABLE "${Python_EXECUTABLE}")
  set(SOURCE_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}/..")
  set(BUILD_DIRECTORY "${CTEST_BINARY_DIR}/modules/seastate")
  regression(${TEST_SCRIPT} ${SEASTATE_EXECUTABLE} ${SOURCE_DIRECTORY} ${BUILD_DIRECTORY} " " ${TESTNAME} "${LABEL}" " ")
endfunction(py_seast_regression)

# moordyn
function(md_regression TESTNAME LABEL)
  set(TEST_SCRIPT "${CMAKE_CURRENT_LIST_DIR}/executeMoordynRegressionCase.py")
  set(MOORDYN_EXECUTABLE "${CTEST_MOORDYN_EXECUTABLE}")
  set(SOURCE_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}/..")
  set(BUILD_DIRECTORY "${CTEST_BINARY_DIR}/modules/moordyn")
  regression(${TEST_SCRIPT} ${MOORDYN_EXECUTABLE} ${SOURCE_DIRECTORY} ${BUILD_DIRECTORY} " " ${TESTNAME} "${LABEL}" " ")
endfunction(md_regression)

# py_moordyn c-bindings interface
function(py_md_regression TESTNAME LABEL)
  set(TEST_SCRIPT "${CMAKE_CURRENT_LIST_DIR}/executeMoordynPyRegressionCase.py")
  set(MOORDYN_EXECUTABLE "${Python_EXECUTABLE}")
  set(SOURCE_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}/..")
  set(BUILD_DIRECTORY "${CTEST_BINARY_DIR}/modules/moordyn")
  regression(${TEST_SCRIPT} ${MOORDYN_EXECUTABLE} ${SOURCE_DIRECTORY} ${BUILD_DIRECTORY} " " ${TESTNAME} "${LABEL}" " ")
endfunction(py_md_regression)

# aerodisk
function(adsk_regression TESTNAME LABEL)
  set(TEST_SCRIPT "${CMAKE_CURRENT_LIST_DIR}/executeAerodiskRegressionCase.py")
  set(AERODISK_EXECUTABLE "${CTEST_AERODISK_EXECUTABLE}")
  set(SOURCE_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}/..")
  set(BUILD_DIRECTORY "${CTEST_BINARY_DIR}/modules/aerodisk")
  regression(${TEST_SCRIPT} ${AERODISK_EXECUTABLE} ${SOURCE_DIRECTORY} ${BUILD_DIRECTORY} " " ${TESTNAME} "${LABEL}" " ")
endfunction(adsk_regression)

# simple-elastodyn
function(sed_regression TESTNAME LABEL)
  set(TEST_SCRIPT "${CMAKE_CURRENT_LIST_DIR}/executeSimpleElastodynRegressionCase.py")
  set(SED_EXECUTABLE "${CTEST_SED_EXECUTABLE}")
  set(SOURCE_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}/..")
  set(BUILD_DIRECTORY "${CTEST_BINARY_DIR}/modules/simple-elastodyn")
  regression(${TEST_SCRIPT} ${SED_EXECUTABLE} ${SOURCE_DIRECTORY} ${BUILD_DIRECTORY} " " ${TESTNAME} "${LABEL}" " ")
endfunction(sed_regression)

# # Python-based OpenFAST Library tests
# function(py_openfast_library_regression TESTNAME LABEL)
#   set(test_module "${CMAKE_SOURCE_DIR}/modules/openfast-library/tests/test_openfast_library.py")
#   set(input_file "${CMAKE_SOURCE_DIR}/reg_tests/r-test/glue-codes/openfast/5MW_OC4Jckt_ExtPtfm/5MW_OC4Jckt_ExtPtfm.fst")
#   add_test(${TESTNAME} ${Python_EXECUTABLE} ${test_module} ${input_file} )
# endfunction(py_openfast_library_regression)

# Python-based OpenFAST IO Library tests
function(py_openfast_io_library_pytest TESTNAME LABEL)
  set(module "-m")
  set(pytest "pytest")
  set(pytestVerbose "--verbose")
  set(py_test_file "${CMAKE_CURRENT_LIST_DIR}/../openfast_io/openfast_io/tests/test_of_io_pytest.py")
  set(executable "--executable=${CTEST_OPENFAST_EXECUTABLE}")
  set(source_dir "--source_dir=${CMAKE_CURRENT_LIST_DIR}/..")
  set(build_dir "--build_dir=${CTEST_BINARY_DIR}")
  add_test(${TESTNAME} ${Python_EXECUTABLE} ${module} ${pytest} ${pytestVerbose} ${py_test_file} ${executable} ${source_dir} ${build_dir})
  set_tests_properties(${TESTNAME} PROPERTIES TIMEOUT 5400 WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}" LABELS "${LABEL}")
endfunction(py_openfast_io_library_pytest)


# py_wavetank
function(py_wavetank_regression TESTNAME LABEL)
  set(TEST_SCRIPT "${CMAKE_CURRENT_LIST_DIR}/executeWavetankPyRegressionCase.py")
  set(SEASTATE_EXECUTABLE "${Python_EXECUTABLE}")
  set(SOURCE_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}/..")
  set(BUILD_DIRECTORY "${CTEST_BINARY_DIR}/glue-codes/other")
  regression(${TEST_SCRIPT} ${SEASTATE_EXECUTABLE} ${SOURCE_DIRECTORY} ${BUILD_DIRECTORY} " " ${TESTNAME} "${LABEL}" " ")
endfunction(py_wavetank_regression)

#===============================================================================
# Regression tests
#===============================================================================

# OpenFAST regression tests
of_regression("AWT_YFix_WSt"                           "openfast;elastodyn;aerodyn;servodyn")
of_regression("AWT_WSt_StartUp_HighSpShutDown"         "openfast;elastodyn;aerodyn;servodyn")
of_regression("AWT_YFree_WSt"                          "openfast;elastodyn;aerodyn;servodyn")
of_regression("AWT_YFree_WTurb"                        "openfast;elastodyn;aerodyn;servodyn")
of_regression("AWT_WSt_StartUpShutDown"                "openfast;elastodyn;aerodyn;servodyn")
of_regression("AOC_WSt"                                "openfast;elastodyn;aerodyn;servodyn")
of_regression("AOC_YFree_WTurb"                        "openfast;elastodyn;aerodyn;servodyn")
of_regression("AOC_YFix_WSt"                           "openfast;elastodyn;aerodyn;servodyn")
of_regression("AOC_YFriction_Loading"                  "openfast;elastodyn;aerodyn;servodyn")
of_regression("AOC_YFriction_Stiffness"                "openfast;elastodyn;aerodyn;servodyn")
of_regression("UAE_Dnwind_YRamp_WSt"                   "openfast;elastodyn;aerodyn;servodyn")
of_regression("UAE_Upwind_Rigid_WRamp_PwrCurve"        "openfast;elastodyn;aerodyn;servodyn")
of_regression("WP_VSP_WTurb_PitchFail"                 "openfast;elastodyn;aerodyn;servodyn")
of_regression("WP_VSP_ECD"                             "openfast;elastodyn;aerodyn;servodyn")
of_regression("WP_VSP_WTurb"                           "openfast;elastodyn;aerodyn;servodyn")
of_regression("SWRT_YFree_VS_EDG01"                    "openfast;elastodyn;aerodyn;servodyn")
of_regression("SWRT_YFree_VS_EDC01"                    "openfast;elastodyn;aerodyn;servodyn")
of_regression("SWRT_YFree_VS_WTurb"                    "openfast;elastodyn;aerodyn;servodyn")
of_regression("5MW_Land_DLL_WTurb"                     "openfast;elastodyn;aerodyn;servodyn")
of_regression("5MW_Land_DLL_WTurb_wNacDrag"            "openfast;elastodyn;aerodyn;servodyn")
of_regression("5MW_Land_DLL_WTurb_wBlPDyn"             "openfast;elastodyn;aerodyn;servodyn")
of_regression("5MW_OC3Mnpl_DLL_WTurb_WavesIrr"         "openfast;elastodyn;aerodyn;servodyn;hydrodyn;subdyn;offshore")
of_regression("5MW_OC3Mnpl_DLL_WTurb_WavesIrr_IceDyn"  "openfast;elastodyn;aerodyn;servodyn;hydrodyn;subdyn;icedyn;offshore")
of_regression("5MW_OC3Mnpl_DLL_WTurb_WavesIrr_IceFloe" "openfast;elastodyn;aerodyn;servodyn;hydrodyn;subdyn;icefloe;offshore")
of_regression("5MW_OC3Mnpl_DLL_WTurb_WavesIrr_Restart" "openfast;elastodyn;aerodyn;servodyn;hydrodyn;subdyn;offshore;restart")
of_regression("5MW_OC3Trpd_DLL_WSt_WavesReg"           "openfast;elastodyn;aerodyn;servodyn;hydrodyn;subdyn;offshore")
of_regression("5MW_OC4Jckt_DLL_WTurb_WavesIrr_MGrowth" "openfast;elastodyn;aerodyn;servodyn;hydrodyn;subdyn;offshore")
of_regression("5MW_ITIBarge_DLL_WTurb_WavesIrr"        "openfast;elastodyn;aerodyn;servodyn;hydrodyn;map;offshore")
of_regression("5MW_TLP_DLL_WTurb_WavesIrr_WavesMulti"  "openfast;elastodyn;aerodyn;servodyn;hydrodyn;map;offshore")
of_regression("5MW_OC3Spar_DLL_WTurb_WavesIrr"         "openfast;elastodyn;aerodyn;servodyn;hydrodyn;map;offshore")
of_regression("5MW_OC4Semi_WSt_WavesWN"                "openfast;elastodyn;aerodyn;servodyn;hydrodyn;moordyn;offshore")
of_regression("5MW_MRSemi_DLL_WSt_WavesIrr"            "openfast;elastodyn;aerodyn;servodyn;hydrodyn;moordyn;offshore;subdyn;olaf;multirotor")
of_regression("5MW_Land_BD_DLL_WTurb"                  "openfast;beamdyn;aerodyn;servodyn")
of_regression("5MW_Land_BD_DLL_WTurb_StC"              "openfast;beamdyn;aerodyn;servodyn;stc")
of_regression("5MW_Land_BD_Init"                       "openfast;beamdyn;aerodyn;servodyn")
of_regression("5MW_OC4Jckt_ExtPtfm"                    "openfast;elastodyn;extptfm;offshore")
of_regression("HelicalWake_OLAF"                       "openfast;aerodyn;olaf")
of_regression("EllipticalWing_OLAF"                    "openfast;aerodyn;olaf")
of_regression("StC_test_OC4Semi"                       "openfast;servodyn;hydrodyn;moordyn;offshore;stc")
of_regression("StC_test_OC4Semi_blade2"                "openfast;servodyn;hydrodyn;moordyn;offshore;stc")
of_regression("MHK_RM1_Fixed"                          "openfast;elastodyn;aerodyn;mhk;offshore")
of_regression("MHK_RM1_Floating"                       "openfast;elastodyn;aerodyn;hydrodyn;moordyn;mhk;offshore")
of_regression("MHK_RM1_Floating_MR"                    "openfast;elastodyn;aerodyn;servodyn;hydrodyn;moordyn;multirotor;offshore")
of_regression("MHK_RM1_Floating_wNacDrag"              "openfast;elastodyn;aerodyn;hydrodyn;moordyn;mhk;offshore")
of_regression("MHK_RM1_Floating_Tank-scaled"           "openfast;elastodyn;aerodyn;hydrodyn;moordyn;mhk;offshore;scaled")
of_regression("Tailfin_FreeYaw1DOF_PolarBased"         "openfast;elastodyn;aerodyn")
of_regression("Tailfin_FreeYaw1DOF_Unsteady"           "openfast;elastodyn;aerodyn")
of_regression("5MW_Land_DLL_WTurb_ADsk"                "openfast;elastodyn;aerodisk")
of_regression("5MW_Land_DLL_WTurb_ADsk_SED"            "openfast;simple-elastodyn;aerodisk")
of_regression("5MW_Land_DLL_WTurb_SED"                 "openfast;simple-elastodyn;aerodyn")
of_regression("IEA22MW_ModalDamping"                   "openfast;beamdyn;servodyn")
of_regression("IEA22MW_ModalDampingLoose"              "openfast;beamdyn;servodyn")
of_regression("OC6_phaseII"                            "openfast;soildyn;subdyn;hydrodyn;offshore;stc")
of_regression("MinimalExample"                         "openfast;elastodyn")

of_aeromap_regression("5MW_Land_AeroMap"               "aeromap;elastodyn;aerodyn")

# OpenFAST C++ API test
if(BUILD_OPENFAST_CPP_DRIVER)
  of_cpp_interface_regression("5MW_Land_DLL_WTurb_cpp" "openfast;fastlib;cpp")
  of_cpp_interface_regression("5MW_Restart_cpp"        "openfast;fastlib;cpp;restart")
  of_cpp_interface_regression("5MW_Land_DLL_WTurb_ExtInfw_cpp" "openfast;fastlib;extinfw;cpp")
endif()

# OpenFAST Driver test for OpenFAST C++ Library
# This tests the FAST Library and FAST_Library.h
if(BUILD_OPENFAST_LIB_DRIVER)
  of_fastlib_regression("AWT_YFree_WSt"                    "fastlib;elastodyn;aerodyn;servodyn")
endif()

# OpenFAST Python API test
of_regression_py("5MW_Land_DLL_WTurb_py"                     "openfast;fastlib;python;elastodyn;aerodyn;servodyn")
of_regression_py("5MW_ITIBarge_DLL_WTurb_WavesIrr_py"        "openfast;fastlib;python;elastodyn;aerodyn;servodyn;hydrodyn;map;offshore")
of_regression_py("5MW_TLP_DLL_WTurb_WavesIrr_WavesMulti_py"  "openfast;fastlib;python;elastodyn;aerodyn;servodyn;hydrodyn;map;offshore")
of_regression_py("5MW_OC3Spar_DLL_WTurb_WavesIrr_py"         "openfast;fastlib;python;elastodyn;aerodyn;servodyn;hydrodyn;map;offshore")
of_regression_py("5MW_OC4Semi_WSt_WavesWN_py"                "openfast;fastlib;python;elastodyn;aerodyn;servodyn;hydrodyn;moordyn;offshore")
of_regression_py("5MW_Land_BD_DLL_WTurb_py"                  "openfast;fastlib;python;beamdyn;aerodyn;servodyn")
of_regression_py("HelicalWake_OLAF_py"                       "openfast;fastlib;python;aerodyn;olaf")
of_regression_py("EllipticalWing_OLAF_py"                    "openfast;fastlib;python;aerodyn;olaf")

# AeroAcoustic regression test
of_regression_aeroacoustic("IEA_LB_RWT-AeroAcoustics"  "openfast;aerodyn;aeroacoustics")

# Linearized OpenFAST regression tests
of_regression_linear("Fake5MW_AeroLin_B1_UA4_DBEMT3"  "-highpass=0.05"  "openfast;linear;elastodyn;aerodyn")
of_regression_linear("Fake5MW_AeroLin_B3_UA6"         "-highpass=0.05"  "openfast;linear;elastodyn;aerodyn")
of_regression_linear("WP_Stationary_Linear"           ""                "openfast;linear;elastodyn")
of_regression_linear("Ideal_Beam_Fixed_Free_Linear"   "-highpass=0.10"  "openfast;linear;beamdyn")
of_regression_linear("Ideal_Beam_Free_Free_Linear"    "-highpass=0.10"  "openfast;linear;beamdyn")
of_regression_linear("Damped_Beam_Fixed"              "-highpass=0.10"  "openfast;linear;beamdyn")
of_regression_linear("Damped_Beam_Rotating"           "-highpass=0.10"  "openfast;linear;beamdyn")
of_regression_linear("Damped_Beam_Rotated"            "-highpass=0.10"  "openfast;linear;beamdyn")
of_regression_linear("5MW_Land_Linear_Aero"           "-highpass=0.25"  "openfast;linear;elastodyn;servodyn;aerodyn")
of_regression_linear("5MW_Land_Linear_Aero_CalcSteady" "-highpass=0.25"  "openfast;linear;elastodyn;servodyn;aerodyn")
of_regression_linear("5MW_Land_BD_Linear"             ""                "openfast;linear;beamdyn;servodyn")
of_regression_linear("5MW_Land_BD_Linear_Aero"        "-highpass=0.25"  "openfast;linear;beamdyn;servodyn;aerodyn")
of_regression_linear("5MW_OC4Semi_Linear"             ""                "openfast;linear;hydrodyn;servodyn;map")
of_regression_linear("5MW_OC4Semi_MD_Linear"          ""                "openfast;linear;hydrodyn;servodyn;moordyn")
of_regression_linear("StC_test_OC4Semi_Linear_Nac"    ""                "openfast;linear;servodyn;stc")
of_regression_linear("StC_test_OC4Semi_Linear_Tow"    ""                "openfast;linear;servodyn;stc")
of_regression_linear("WP_Stationary_Linear"           ""                "openfast;linear;elastodyn")
of_regression_linear("5MW_OC3Spar_Linear"             ""                "openfast;linear;map;hydrodyn")
of_regression_linear("5MW_OC3Mnpl_Linear"             ""                "openfast;linear;hydrodyn;servodyn;moordyn")
of_regression_linear("MHK_RM1_Floating_MR_Linear"     "-highpass=0.05"  "openfast;linear;elastodyn;aerodyn;servodyn;hydrodyn;moordyn;multirotor;offshore;mhk")

# FAST Farm regression tests
if(BUILD_FASTFARM)
  ff_regression("AMReX"             ""                               "fastfarm")
  ff_regression("TSinflow"          ""                               "fastfarm")
  ff_regression("LESinflow"         ""                               "fastfarm")
  ff_regression("TSinflow_curl"     ""                               "fastfarm")
  ff_regression("ModAmb_3"          ""                               "fastfarm")
  ff_regression("TSinflowADskSED"   ""                               "fastfarm;aerodisk;simple-elastodyn")
  ff_regression("MD_Shared"         "-compFile=FAST.Farm.FarmMD.MD"  "fastfarm;moordyn")
endif()

# AeroDyn regression tests
ad_regression("ad_timeseries_shutdown"      "aerodyn;bem")
ad_regression("ad_EllipticalWingInf_OLAF"   "aerodyn;bem")
ad_regression("ad_HelicalWakeInf_OLAF"      "aerodyn;bem")
ad_regression("ad_Kite_OLAF"                "aerodyn;bem")
ad_regression("ad_MultipleHAWT"             "aerodyn;bem")
ad_regression("ad_QuadRotor_OLAF"           "aerodyn;bem")
ad_regression("ad_VerticalAxis_OLAF"        "aerodyn;bem")
ad_regression("ad_MHK_RM1_Fixed"            "aerodyn;bem;mhk")
ad_regression("ad_MHK_RM1_Floating"         "aerodyn;bem;mhk")
ad_regression("ad_BAR_CombinedCases"        "aerodyn;bem") # NOTE: doing BAR at the end to avoid copy errors
ad_regression("ad_BAR_OLAF"                 "aerodyn;bem")
ad_regression("ad_BAR_SineMotion"           "aerodyn;bem")
ad_regression("ad_BAR_SineMotion_UA4_DBEMT3" "aerodyn;bem")
ad_regression("ad_BAR_RNAMotion"            "aerodyn;bem")
ad_regression("ad_B1n2_OLAF"                "aerodyn;OLAF")
ad_regression("ad_Sphere_OLAF"              "aerodyn;OLAF")
py_ad_regression("py_ad_5MW_OC4Semi_WSt_WavesWN"     "aerodyn;bem;python")
py_ad_regression("py_ad_B1n2_OLAF"                   "aerodyn;OLAF;python")

# UnsteadyAero
ua_regression("ua_redfreq"                  "unsteadyaero")

# BeamDyn regression tests
bd_regression("bd_5MW_dynamic"               "beamdyn;dynamic")
bd_regression("bd_5MW_dynamic_gravity_Az00"  "beamdyn;dynamic")
bd_regression("bd_5MW_dynamic_gravity_Az90"  "beamdyn;dynamic")
bd_regression("bd_5MW_dynamic_modal_damping" "beamdyn;dynamic")
bd_regression("bd_curved_beam"              "beamdyn;static")
bd_regression("bd_isotropic_rollup"         "beamdyn;static")
bd_regression("bd_static_cantilever_beam"   "beamdyn;static")
bd_regression("bd_static_twisted_with_k1"   "beamdyn;static")

# HydroDyn regression tests
hd_regression("hd_5MW_ITIBarge_DLL_WTurb_WavesIrr"          "hydrodyn;offshore")
hd_regression("hd_5MW_OC3Spar_DLL_WTurb_WavesIrr"           "hydrodyn;offshore")
hd_regression("hd_5MW_OC4Semi_WSt_WavesWN"                  "hydrodyn;offshore")
hd_regression("hd_5MW_TLP_DLL_WTurb_WavesIrr_WavesMulti"    "hydrodyn;offshore")
hd_regression("hd_TaperCylinderPitchMoment"                 "hydrodyn;offshore")
hd_regression("hd_NBodyMod1"                                "hydrodyn;offshore")
hd_regression("hd_NBodyMod2"                                "hydrodyn;offshore")
hd_regression("hd_NBodyMod3"                                "hydrodyn;offshore")
hd_regression("hd_WaveStMod1"                               "hydrodyn;offshore")
hd_regression("hd_WaveStMod2"                               "hydrodyn;offshore")
hd_regression("hd_WaveStMod3"                               "hydrodyn;offshore")
hd_regression("hd_MHstLMod2"                                "hydrodyn;offshore")
hd_regression("hd_MHstLMod1_compare"                        "hydrodyn;offshore")
hd_regression("hd_MHstLMod2_compare"                        "hydrodyn;offshore")
hd_regression("hd_MHstLMod2_RectMmbr"                       "hydrodyn;offshore")
hd_regression("hd_MCF_WaveStMod0"                           "hydrodyn;offshore")
hd_regression("hd_MCF_WaveStMod1"                           "hydrodyn;offshore")
hd_regression("hd_MCF_WaveStMod2"                           "hydrodyn;offshore")
hd_regression("hd_MCF_WaveStMod3"                           "hydrodyn;offshore")
hd_regression("hd_ExctnMod1_ExctnDisp1"                     "hydrodyn;offshore")
hd_regression("hd_ExctnMod1_ExctnDisp2"                     "hydrodyn;offshore")
hd_regression("hd_ExctnMod1_ExctnDisp2_PtfmYMod1"           "hydrodyn;offshore")
hd_regression("hd_5MW_OC4Semi_WSt_WavesWN_PtfmYMod0_LargeYaw" "hydrodyn;offshore")
hd_regression("hd_5MW_OC4Semi_WSt_WavesWN_PtfmYMod1_LargeYaw" "hydrodyn;offshore")
hd_regression("hd_NonlinearFKHst"                           "hydrodyn;offshore")

# Py-HydroDyn regression tests
py_hd_regression("py_hd_5MW_OC4Semi_WSt_WavesWN"            "hydrodyn;offshore;python")

# SubDyn regression tests
sd_regression("SD_Cable_5Joints"                              "subdyn;offshore")
sd_regression("SD_PendulumDamp"                               "subdyn;offshore")
sd_regression("SD_Rigid"                                      "subdyn;offshore")
sd_regression("SD_SparHanging"                                "subdyn;offshore")
sd_regression("SD_AnsysComp1_PinBeam"                         "subdyn;offshore") # TODO Issue #855
sd_regression("SD_AnsysComp2_Cable"                           "subdyn;offshore")
sd_regression("SD_AnsysComp3_PinBeamCable"                    "subdyn;offshore") # TODO Issue #855
sd_regression("SD_Spring_Case1"                               "subdyn;offshore")
sd_regression("SD_Spring_Case2"                               "subdyn;offshore")
sd_regression("SD_Spring_Case3"                               "subdyn;offshore")
sd_regression("SD_Revolute_Joint"                             "subdyn;offshore")
sd_regression("SD_2Beam_Spring"                               "subdyn;offshore")
sd_regression("SD_2Beam_Cantilever"                           "subdyn;offshore")
sd_regression("SD_2Beam_MixedDiscretization"                  "subdyn;offshore")
sd_regression("SD_CantileverBeam_Rectangular"                 "subdyn;offshore")
sd_regression("SD_SelfWeight_FloatingSystem"                  "subdyn;offshore")
# TODO test below are bugs, should be added when fixed
# sd_regression("SD_Force"                                      "subdyn;offshore")
# sd_regression("SD_AnsysComp4_UniversalCableRigid"             "subdyn;offshore")
# sd_regression("SD_Rigid2Interf_Cables"                        "subdyn;offshore")

# InflowWind regression tests
ifw_regression("ifw_turbsimff"                                "inflowwind")
ifw_regression("ifw_uniform"                                  "inflowwind")
ifw_regression("ifw_nativeBladed"                             "inflowwind")
ifw_regression("ifw_BoxExceed"                                "inflowwind")
ifw_regression("ifw_BoxExceedTwr"                             "inflowwind")
ifw_regression("ifw_HAWC"                                     "inflowwind")

# Py-InflowWind regression tests
py_ifw_regression("py_ifw_turbsimff"                          "inflowwind;python")

yaml_equiv("inflowwind" "ifw_turbsimff"    "${CTEST_INFLOWWIND_EXECUTABLE}" "inflowwind;yaml")
yaml_equiv("inflowwind" "ifw_uniform"      "${CTEST_INFLOWWIND_EXECUTABLE}" "inflowwind;yaml")
yaml_equiv("inflowwind" "ifw_HAWC"         "${CTEST_INFLOWWIND_EXECUTABLE}" "inflowwind;yaml")
yaml_equiv("inflowwind" "ifw_nativeBladed" "${CTEST_INFLOWWIND_EXECUTABLE}" "inflowwind;yaml")
# Wave 4 driver-conversion mode (see yaml_equiv_driver's own comment above, and its
# first instance under aerodisk): also converts ifw_driver.inp itself to YAML and runs
# yaml driver -> yaml primary.
yaml_equiv_driver("inflowwind" "ifw_uniform"    "${CTEST_INFLOWWIND_EXECUTABLE}" "inflowwind;yaml")
yaml_equiv_driver("inflowwind" "ifw_turbsimff"  "${CTEST_INFLOWWIND_EXECUTABLE}" "inflowwind;yaml")

# NOTE: no yaml_equiv_openfast registration for AeroDisk -- the only two CompAero==1
# glue-code cases in r-test (5MW_Land_DLL_WTurb_ADsk, 5MW_Land_DLL_WTurb_ADsk_SED) both
# require a Bladed DISCON DLL, so neither is a suitable DLL-free target; standalone
# driver-level coverage (below) is what's available today.
yaml_equiv("aerodisk" "adsk_timeseries_shutdown" "${CTEST_AERODISK_EXECUTABLE}" "aerodisk;yaml")
# Wave 4 driver-conversion mode (first instance -- see yaml_equiv_driver's own comment
# above): also converts adsk_driver.dvr itself to YAML and runs yaml driver -> yaml primary.
yaml_equiv_driver("aerodisk" "adsk_timeseries_shutdown" "${CTEST_AERODISK_EXECUTABLE}" "aerodisk;yaml")

# NOTE: no yaml_equiv_openfast registration for Simplified ElastoDyn either -- the only
# two CompElast==3 glue-code cases in r-test (5MW_Land_DLL_WTurb_SED,
# 5MW_Land_DLL_WTurb_ADsk_SED) both require a Bladed DISCON DLL, so neither is a
# suitable DLL-free target; standalone driver-level coverage (below) is what's
# available today. The inline EDFile glue path was verified with a manual smoke run
# (DLL copied by hand), same approach as the AeroDisk case above.
yaml_equiv("simple-elastodyn" "sed_test_freewheel" "${CTEST_SED_EXECUTABLE}" "simple-elastodyn;yaml")
yaml_equiv("simple-elastodyn" "sed_test_HSSbrk"    "${CTEST_SED_EXECUTABLE}" "simple-elastodyn;yaml")
# Wave 4 driver-conversion mode (see yaml_equiv_driver's own comment above, and its
# first instance under aerodisk): also converts sed_driver.dvr itself to YAML and runs
# yaml driver -> yaml primary.
yaml_equiv_driver("simple-elastodyn" "sed_test_freewheel" "${CTEST_SED_EXECUTABLE}" "simple-elastodyn;yaml")
yaml_equiv_driver("simple-elastodyn" "sed_test_HSSbrk"    "${CTEST_SED_EXECUTABLE}" "simple-elastodyn;yaml")

# SeaState standalone-driver yaml-equivalence: seastate_1 (WaveMod=3, white noise) and
# seastate_CNW1 (WaveMod=2 JONSWAP with ConstWaveMod=1 constrained wave) cover distinct
# WaveMod/constrained-wave code paths while staying cheap (short NSteps/TimeInterval).
yaml_equiv("seastate" "seastate_1"    "${CTEST_SEASTATE_EXECUTABLE}" "seastate;yaml")
yaml_equiv("seastate" "seastate_CNW1" "${CTEST_SEASTATE_EXECUTABLE}" "seastate;yaml")
# Wave 4 driver-conversion mode (see yaml_equiv_driver's own comment above): also
# converts seastate_driver.inp itself to YAML (class-B/sequential-reader driver) and
# runs yaml driver -> yaml primary.
yaml_equiv_driver("seastate" "seastate_1"    "${CTEST_SEASTATE_EXECUTABLE}" "seastate;yaml")
yaml_equiv_driver("seastate" "seastate_CNW1" "${CTEST_SEASTATE_EXECUTABLE}" "seastate;yaml")

# AWT_YFix_WSt: CompServo=1 with no DISCON DLL (all ServoDyn control modes 0), so its
# all-yaml/single-file modes exercise ServoDyn YAML conversion and the inline ServoFile
# glue path directly via ctest (ServoDyn has no r-test driver cases -- glue coverage only).
# CompElast=1 (full ElastoDyn), so all-yaml/single-file also exercise ElastoDyn YAML
# conversion and its inline EDFile glue path (ElastoDyn likewise has no r-test driver
# cases -- glue coverage only); its BldFile/TwrFile/FurlFile stay text paths.
yaml_equiv_openfast("AWT_YFix_WSt" "perfile"    "${CTEST_OPENFAST_EXECUTABLE}" "openfast;yaml")
yaml_equiv_openfast("AWT_YFix_WSt" "allyaml"    "${CTEST_OPENFAST_EXECUTABLE}" "openfast;yaml")
yaml_equiv_openfast("AWT_YFix_WSt" "singlefile" "${CTEST_OPENFAST_EXECUTABLE}" "openfast;yaml")

# StC_test_OC4Semi: CompServo=1 with no DLL (all control modes 0) and all four StC
# groups populated (blade prescribed-force, nacelle omni, tower TLCD, two substructure
# Z-DOF instances), so all-yaml also exercises the StC .yaml sub-file type (converted
# as its own file type, referenced by path -- never inlined) and single-file exercises
# an inline ServoDyn section whose StC entries stay text paths.
# NOTE: no "perfile" registration here (2.8) -- convert_fst's per-file mode leaves
# every module file entry as its original text path unchanged (no module conversion
# runs at all in that mode), so it exercises no ServoDyn/StC conversion whatsoever; the
# cheap AWT_YFix_WSt perfile case above already covers the .fst-level per-file round
# trip. Running this case's own (much longer) perfile variant would be ~10 CPU-minutes
# spent re-testing exactly nothing StC/ServoDyn-specific -- dropped for cost, keeping
# allyaml + singlefile (the two modes that actually convert ServoDyn/StC).
yaml_equiv_openfast("StC_test_OC4Semi" "allyaml"    "${CTEST_OPENFAST_EXECUTABLE}" "openfast;yaml")
yaml_equiv_openfast("StC_test_OC4Semi" "singlefile" "${CTEST_OPENFAST_EXECUTABLE}" "openfast;yaml")

# MHK_RM1_Fixed: CompSeaSt=1 with no DISCON DLL (CompServo=0), so the inline SeaStFile
# glue path (all-yaml/single-file) can be exercised directly via ctest, unlike AeroDisk/
# Simplified ElastoDyn's inline glue paths which currently require a manually-staged DLL.
yaml_equiv_openfast("MHK_RM1_Fixed" "perfile"    "${CTEST_OPENFAST_EXECUTABLE}" "openfast;yaml")
yaml_equiv_openfast("MHK_RM1_Fixed" "allyaml"    "${CTEST_OPENFAST_EXECUTABLE}" "openfast;yaml")
yaml_equiv_openfast("MHK_RM1_Fixed" "singlefile" "${CTEST_OPENFAST_EXECUTABLE}" "openfast;yaml")

# HydroDyn standalone-driver yaml-equivalence: hd_5MW_OC4Semi_WSt_WavesWN (PotMod=1
# WAMIT semisubmersible with 44 joints/25 cylindrical members/2 fill groups, exercising
# the AddCLin/AddBLin/AddBQuad WAMIT-object matrix list and an external PotFile
# rootname), hd_MCF_WaveStMod1 (the MacCamy-Fuchs "MCF" keyword substitution across the
# simple/depth-based/member-based cylindrical coefficient tables), and
# hd_MHstLMod2_RectMmbr (rectangular member cross-sections/coefficients, the RdtnDT
# "default" sentinel, the short 11-column Members row form, and member/joint output
# lists) together span nearly the full HydroDyn schema.
yaml_equiv("hydrodyn" "hd_5MW_OC4Semi_WSt_WavesWN" "${CTEST_HYDRODYN_EXECUTABLE}" "hydrodyn;yaml")
yaml_equiv("hydrodyn" "hd_MCF_WaveStMod1"          "${CTEST_HYDRODYN_EXECUTABLE}" "hydrodyn;yaml")
yaml_equiv("hydrodyn" "hd_MHstLMod2_RectMmbr"      "${CTEST_HYDRODYN_EXECUTABLE}" "hydrodyn;yaml")

# hd_NBodyMod2 (2.8): NBody=4/NBodyMod=2 (nWAMITObj=4, coupling terms neglected between
# bodies), closing the NBody>1/nWAMITObj>1 coverage gap in HydroDyn's YAML path --
# multi-entry PotFile lists, the AddCLin/AddBLin/AddBQuad matrix-list block slicing
# across 4 WAMIT objects, and FKMod broadcast with NBody>1. Its PotFile rootnames
# ("semi_center"/"semi_col") are case-local WAMIT data, exercising the widened
# hydrodyn INPUT_GLOBS in executeYamlEquivalenceCase.py.
yaml_equiv("hydrodyn" "hd_NBodyMod2"               "${CTEST_HYDRODYN_EXECUTABLE}" "hydrodyn;yaml")

# Wave 4 driver-conversion mode (see yaml_equiv_driver's own comment above): also
# converts hd_driver.inp itself to YAML (class-B/sequential-reader driver) and runs
# yaml driver -> yaml primary. hd_NBodyMod2 exercises PRPInputsMod=1 (the
# prp_steady_state_inputs section); hd_5MW_OC4Semi_WSt_WavesWN exercises PRPInputsMod=2
# (PRPInputsFile, an externally-referenced time-series file resolved relative to the
# driver, left unconverted per the second-order rule).
yaml_equiv_driver("hydrodyn" "hd_NBodyMod2"               "${CTEST_HYDRODYN_EXECUTABLE}" "hydrodyn;yaml")
yaml_equiv_driver("hydrodyn" "hd_5MW_OC4Semi_WSt_WavesWN" "${CTEST_HYDRODYN_EXECUTABLE}" "hydrodyn;yaml")

# MHK_RM1_Floating: CompHydro=1 (self-contained WAMIT PotFile, RdtnDT="default") with no
# DISCON DLL (CompServo=0), so the inline HydroFile glue path (all-yaml/single-file) can
# be exercised directly via ctest. (MHK_RM1_Fixed above has CompHydro=0 -- HydroFile is
# "unused" there -- so it does not exercise HydroDyn; MHK_RM1_Floating is the DLL-free
# CompHydro==1 case this module actually needs.)
yaml_equiv_openfast("MHK_RM1_Floating" "perfile"    "${CTEST_OPENFAST_EXECUTABLE}" "openfast;yaml")
yaml_equiv_openfast("MHK_RM1_Floating" "allyaml"    "${CTEST_OPENFAST_EXECUTABLE}" "openfast;yaml")
yaml_equiv_openfast("MHK_RM1_Floating" "singlefile" "${CTEST_OPENFAST_EXECUTABLE}" "openfast;yaml")

# MHK_RM1_Floating_MR: NRotors=2, MirrorRotor "F T", CompServo=0 (ServoFile "unused" --
# no DISCON DLL staging needed), with every active module inline-YAML-capable
# (ElastoDyn per-rotor EDFile, AeroDyn, InflowWind, SeaState, HydroDyn, SubDyn,
# MoorDyn). This is the multirotor `input_files:rotors` sequence exercise: rotor 1's
# and rotor 2's (distinct) EDFile must each convert/inline correctly in allyaml/
# single-file, and MirrorRotor's per-rotor list must round-trip.
yaml_equiv_openfast("MHK_RM1_Floating_MR" "perfile"    "${CTEST_OPENFAST_EXECUTABLE}" "openfast;yaml")
yaml_equiv_openfast("MHK_RM1_Floating_MR" "allyaml"    "${CTEST_OPENFAST_EXECUTABLE}" "openfast;yaml")
yaml_equiv_openfast("MHK_RM1_Floating_MR" "singlefile" "${CTEST_OPENFAST_EXECUTABLE}" "openfast;yaml")

# 5MW_OC3Spar_DLL_WTurb_WavesIrr (2.8, the headline offshore glue case): CompServo=1
# with PCMode=VSContrl=5 (Bladed-style DISCON_OC3Hywind.dll), CompAero=2, CompHydro=1,
# CompMooring=1 (MAP++, stays a text path), and InflowFile pointing at the shared
# ../5MW_Baseline/ directory -- so all-yaml/single-file exercise both headline items
# together: DISCON DLL staging (executeYamlEquivalenceCase.py) and the generic
# relative-path rewrite (yamlDeckConverter.py's InflowWind Wind/*.bts file, inlined
# from a directory that differs from the .fst's).
yaml_equiv_openfast("5MW_OC3Spar_DLL_WTurb_WavesIrr" "perfile"    "${CTEST_OPENFAST_EXECUTABLE}" "openfast;yaml")
yaml_equiv_openfast("5MW_OC3Spar_DLL_WTurb_WavesIrr" "allyaml"    "${CTEST_OPENFAST_EXECUTABLE}" "openfast;yaml")
yaml_equiv_openfast("5MW_OC3Spar_DLL_WTurb_WavesIrr" "singlefile" "${CTEST_OPENFAST_EXECUTABLE}" "openfast;yaml")

# AeroDyn standalone-driver yaml-equivalence: ad_BAR_RNAMotion (Wake_Mod=1 BEMT rotor,
# prescribed rigid-body/pitch/rotor motion, TwrShadow=1, tower table, nodal outputs off)
# and ad_BAR_OLAF (Wake_Mod=3 OLAF -- OLAFInputFileName stays a path -- with the Nodal
# Outputs section active and BldNd_BlOutNd="ALL") together span the BEMT and OLAF wake
# models and the nodal-outputs schema; both are single-rotor, 3-bladed BAR-turbine cases.
# The existing AWT_YFix_WSt/MHK_RM1_Fixed/MHK_RM1_Floating glue trios above already
# exercise AeroDyn (CompAero=2) through convert_fst's AeroFile conversion, since
# yamlDeckConverter.py now knows how to convert it -- no new glue registrations needed.
yaml_equiv("aerodyn" "ad_BAR_RNAMotion" "${CTEST_AERODYN_EXECUTABLE}" "aerodyn;yaml")
yaml_equiv("aerodyn" "ad_BAR_OLAF"      "${CTEST_AERODYN_EXECUTABLE}" "aerodyn;yaml")

# AeroDyn standalone-driver YAML input (Wave 4, task 4.5, the largest driver schema):
# ad_BAR_CombinedCases exercises the basic-HAWT-format single-turbine geometry plus the
# 10-column combined-case table (AnalysisType=3); ad_BAR_RNAMotion exercises the
# advanced-format single-turbine geometry (merged per-blade `blades:` list) with
# time-varying nacelle-yaw/rotor-speed/blade-pitch motion prescribed via CSV files
# (AnalysisType=1, NacMotionType=RotMotionType=BldMotionType=1).
yaml_equiv_driver("aerodyn" "ad_BAR_CombinedCases" "${CTEST_AERODYN_EXECUTABLE}" "aerodyn;yaml")
yaml_equiv_driver("aerodyn" "ad_BAR_RNAMotion"     "${CTEST_AERODYN_EXECUTABLE}" "aerodyn;yaml")

# UnsteadyAero standalone-driver YAML input (Wave 4, task 4.6, from-scratch: the UA
# driver file IS the top-level input, no separate primary -- so only the "driver" mode
# is registered, there is no bare yaml_equiv_unsteadyaero counterpart). The two cases
# together cover both schema branches simulation_control:SimMod selects: ua_redfreq
# (UA2.dvr, SimMod=1) exercises "periodic-motion" (reduced-frequency/oscillating-AoA);
# ua_elast (UA4.dvr, SimMod=3) exercises "aeroelastic" (LinDyn-coupled, constant-inflow/
# dynamic-motion sub-case). CTEST_UADRIVER_EXECUTABLE is the UA driver's own cache
# variable (reg_tests/CMakeLists.txt) -- NOT "CTEST_UA_EXECUTABLE".
yaml_equiv_driver("unsteadyaero" "ua_redfreq" "${CTEST_UADRIVER_EXECUTABLE}" "unsteadyaero;yaml")
yaml_equiv_driver("unsteadyaero" "ua_elast"   "${CTEST_UADRIVER_EXECUTABLE}" "unsteadyaero;yaml")

# MoorDyn standalone-driver yaml-equivalence: the four cases together span the
# free-form schema: md_BodiesAndRods (BODIES + RODS incl. Body1/Body1Pinned
# attachments, option-keyword aliases kb/cb/WtrDpth, an unrecognized option keyword,
# and TmaxIC=0), md_5MW_OC4Semi (classic line-heavy floating case: Fixed/Vessel
# points, prescribed platform motion, and a large multi-channel-per-line OUTPUTS
# list), md_lineFail (a Free rod, line ends attached to rod ends R1A/R1B, and the
# FAILURE section's comma-separated line-ID rows), and md_VIV (11-column LINE
# DICTIONARY form with the optional Cl column, tScheme=RK4, seabed friction options,
# and a WaterKin file kept as a path).
# The existing MHK_RM1_Floating glue trio above already exercises MoorDyn
# (CompMooring=3, no DISCON DLL) through convert_fst's MooringFile conversion, since
# yamlDeckConverter.py now knows how to convert it -- no new glue registrations needed.
yaml_equiv("moordyn" "md_BodiesAndRods" "${CTEST_MOORDYN_EXECUTABLE}" "moordyn;yaml")
yaml_equiv("moordyn" "md_5MW_OC4Semi"   "${CTEST_MOORDYN_EXECUTABLE}" "moordyn;yaml")
yaml_equiv("moordyn" "md_lineFail"      "${CTEST_MOORDYN_EXECUTABLE}" "moordyn;yaml")

# MoorDyn standalone-driver YAML input (Wave 4, task 4.4b): md_5MW_OC4Semi exercises
# InputsMod=1 (a real prescribed-platform-motion InputsFile, kept as a path) with the
# single-row (NumTurbines=0) initial-positions table; md_waterkin2 additionally
# exercises the optional farm:SeaStateFile branch (a SeaState input file referenced
# from the driver).
yaml_equiv_driver("moordyn" "md_5MW_OC4Semi" "${CTEST_MOORDYN_EXECUTABLE}" "moordyn;yaml")
yaml_equiv_driver("moordyn" "md_waterkin2"   "${CTEST_MOORDYN_EXECUTABLE}" "moordyn;yaml")
yaml_equiv("moordyn" "md_VIV"           "${CTEST_MOORDYN_EXECUTABLE}" "moordyn;yaml")

# NOTE: no yaml_equiv registration for FEAMooring (CompMooring=2). Its YAML support is
# implemented (FEAM_Yaml.f90 + funnel + full glue inline path) and was verified
# bit-identical out-of-harness against the OC4Semi FEAMooring deck, but there is no honest
# in-harness vehicle: no r-test glue deck selects CompMooring=2 (the *_FEAMooring.dat files
# in the OC3Spar/OC4Semi/TLP/ITIBarge decks are inactive alternates -- those decks run MAP++
# or MoorDyn), the r-test submodule has no FEAMooring driver case, and the FEAM driver is
# idiosyncratic (takes the input file directly, no .dvr; writes a non-standard FEAM.out) so it
# does not fit the executeYamlEquivalenceCase.py driver conventions. Coverage gap accepted by
# the user for this legacy module (2026-07-09); revisit if a CompMooring=2 r-test case is added.

# BeamDyn standalone-driver yaml-equivalence: bd_5MW_dynamic (dynamic solve, GA2 time
# integration, quadrature=2/Trapezoidal so refine's non-"default" path is live, 9 node
# outputs + full OutList) and bd_static_cantilever_beam (static/QuasiStaticInit path,
# quadrature=1/Gaussian, small 3-key-point single-member geometry, 1 node output)
# together cover both quadrature branches, the "default"-sentinel and explicit-value
# forms of the simulation_control fields (dynamic case takes "DEFAULT" throughout;
# static case gives refine/quadrature explicit values), and both NNodeOuts sizes.
yaml_equiv("beamdyn" "bd_5MW_dynamic"             "${CTEST_BEAMDYN_EXECUTABLE}" "beamdyn;yaml")
yaml_equiv("beamdyn" "bd_static_cantilever_beam"  "${CTEST_BEAMDYN_EXECUTABLE}" "beamdyn;yaml")
# Wave 4 driver-conversion mode (see yaml_equiv_driver's own comment above): also
# convert the standalone driver's own input file to YAML. No r-test BeamDyn driver
# case actually sets NumPointLoads > 0, so these only exercise the multi_point_loads
# table's empty-list (NumPointLoads==0 -> 1 all-zero row) path; the table's row-mapping
# emission/parsing was verified out-of-harness with a hand-built NumPointLoads>0 deck.
yaml_equiv_driver("beamdyn" "bd_5MW_dynamic"             "${CTEST_BEAMDYN_EXECUTABLE}" "beamdyn;yaml")
yaml_equiv_driver("beamdyn" "bd_static_cantilever_beam"  "${CTEST_BEAMDYN_EXECUTABLE}" "beamdyn;yaml")

# 5MW_Land_BD_Init: CompElast=2 (ElastoDyn + BeamDyn for blades), CompAero=0,
# CompServo=0 (no DISCON DLL) -- the cheapest CompElast=2 glue case in r-test, so the
# BDBldFile-per-blade-file-path conversion (yamlDeckConverter.py's convert_fst) and
# BeamDyn's read-input funnel are exercised end-to-end through the full glue path in
# all three modes. BDBldFile itself is never inlined (FAST_Yaml.f90's GetBDBldFiles
# only ever accepts a list of file paths, in both allyaml and singlefile modes) --
# consistent with BeamDyn having no PassedFileIsYaml/inline entry point at all (see
# BeamDyn_Yaml.f90's header comment).
yaml_equiv_openfast("5MW_Land_BD_Init" "perfile"    "${CTEST_OPENFAST_EXECUTABLE}" "openfast;yaml")
yaml_equiv_openfast("5MW_Land_BD_Init" "allyaml"    "${CTEST_OPENFAST_EXECUTABLE}" "openfast;yaml")
yaml_equiv_openfast("5MW_Land_BD_Init" "singlefile" "${CTEST_OPENFAST_EXECUTABLE}" "openfast;yaml")

# SubDyn standalone-driver yaml-equivalence: SD_Cable_5Joints (2 reaction joints with
# the plain 7-column reactions table, both beam ("1c") and cable (MType=2) members,
# cable properties with a non-zero CtrlChannel, and a 4-entry member_output_list) and
# SD_MultiTP (a floating structure -- NReact=0 -- with 2 transition pieces and 3
# interface joints whose TPIdx values are 0/1/2, exercising the TPIdx-per-interface
# path and the Guyan-damping/GuyanDampMat block with a non-zero GuyanDampMod) together
# cover both the reactions-with-fixed-base and the floating/multi-TP configurations,
# both member-type branches (MSpin vs. COSMID last-column semantics), and the cable
# property table's optional CtrlChannel field.
yaml_equiv("subdyn" "SD_Cable_5Joints" "${CTEST_SUBDYN_EXECUTABLE}" "subdyn;yaml")
yaml_equiv("subdyn" "SD_MultiTP"       "${CTEST_SUBDYN_EXECUTABLE}" "subdyn;yaml")
# Wave 4 driver-conversion mode (see yaml_equiv_driver's own comment above): also
# convert the standalone driver's own input file to YAML. SD_MultiTP is the preferred
# table exerciser (nTP=2, so tp_ref_points/TPIdx both list more than one entry);
# SD_Cable_5Joints (nTP=1, nAppliedLoads=0) covers the single-TP/no-loads path.
yaml_equiv_driver("subdyn" "SD_MultiTP"       "${CTEST_SUBDYN_EXECUTABLE}" "subdyn;yaml")
yaml_equiv_driver("subdyn" "SD_Cable_5Joints" "${CTEST_SUBDYN_EXECUTABLE}" "subdyn;yaml")

# 5MW_OC3Mnpl_Linear: CompSub=1 (SubDyn), CompHydro=0, CompServo=0 (no DISCON DLL) --
# the cheapest CompSub=1 glue case in r-test (TMax=DT=0.005s), so the SubFile
# conversion/inlining (yamlDeckConverter.py's convert_fst) and SubDyn's read-input
# funnel are exercised end-to-end through the full glue path in all three modes.
# The underlying monopile deck (NRELOffshrBsline5MW_OC3Monopile_SubDyn.dat) has a
# non-empty (quoted-empty, "") SSIfile column on its reaction row and a non-zero
# GuyanDampMod=2 block, both round-tripped verbatim by the converter.
yaml_equiv_openfast("5MW_OC3Mnpl_Linear" "perfile"    "${CTEST_OPENFAST_EXECUTABLE}" "openfast;yaml")
yaml_equiv_openfast("5MW_OC3Mnpl_Linear" "allyaml"    "${CTEST_OPENFAST_EXECUTABLE}" "openfast;yaml")
yaml_equiv_openfast("5MW_OC3Mnpl_Linear" "singlefile" "${CTEST_OPENFAST_EXECUTABLE}" "openfast;yaml")

# 5MW_OC4Jckt_ExtPtfm: CompSub=2 (ExtPtfm_MCKF), CompHydro=0, CompServo=0 (no DISCON
# DLL) -- the only CompSub=2 glue case in r-test, so the SubFile conversion/inlining
# (yamlDeckConverter.py's convert_fst -> convert_extptfm) and ExtPtfm's read-input
# funnel are exercised end-to-end through the full glue path in all three modes. It
# uses DT="default", a Craig-Bampton reduced superelement (Red_FileName=ExtPtfm_SE.dat,
# with NActiveDOFList=-1 "all CB modes"/NInitPosList=NInitVelList=0), no connections,
# and user modal forcing (UserForcing=True, Force_FileName=ExtPtfm_Frc.dat) -- the
# reduced/forcing files stay path-only and round-trip verbatim through the converter.
yaml_equiv_openfast("5MW_OC4Jckt_ExtPtfm" "perfile"    "${CTEST_OPENFAST_EXECUTABLE}" "openfast;yaml")
yaml_equiv_openfast("5MW_OC4Jckt_ExtPtfm" "allyaml"    "${CTEST_OPENFAST_EXECUTABLE}" "openfast;yaml")
yaml_equiv_openfast("5MW_OC4Jckt_ExtPtfm" "singlefile" "${CTEST_OPENFAST_EXECUTABLE}" "openfast;yaml")

# 5MW_OC3Mnpl_DLL_WTurb_WavesIrr_IceDyn: CompIce=2 (IceDyn), CompSub=1 (SubDyn),
# CompServo=1 with PCMode=VSContrl=5 (Bladed-style DISCON.dll) -- the only CompIce=2
# glue case in r-test, so the IceFile conversion/inlining (yamlDeckConverter.py's
# convert_fst -> convert_icedyn) and IceDyn's read-input funnel are exercised
# end-to-end through the full glue path in all three modes, alongside DISCON DLL
# staging (executeYamlEquivalenceCase.py), mirroring the 5MW_OC3Spar_DLL_WTurb_WavesIrr
# headline case above. IceModel=6 in the underlying deck (IceDyn_Input.dat), so
# ice_model_6's fields are the live branch; ice_model_1..5 round-trip verbatim
# (unused by IceModel=6 but still read unconditionally by IceD_ReadInput).
yaml_equiv_openfast("5MW_OC3Mnpl_DLL_WTurb_WavesIrr_IceDyn" "perfile"    "${CTEST_OPENFAST_EXECUTABLE}" "openfast;yaml")
yaml_equiv_openfast("5MW_OC3Mnpl_DLL_WTurb_WavesIrr_IceDyn" "allyaml"    "${CTEST_OPENFAST_EXECUTABLE}" "openfast;yaml")
yaml_equiv_openfast("5MW_OC3Mnpl_DLL_WTurb_WavesIrr_IceDyn" "singlefile" "${CTEST_OPENFAST_EXECUTABLE}" "openfast;yaml")

yaml_example_smoke("${CTEST_OPENFAST_EXECUTABLE}" "openfast;yaml")

# SeaState regression tests
seast_regression("seastate_1"                                "seastate")
seast_regression("seastate_wr_kin1"                          "seastate")
seast_regression("seastate_CNW1"                             "seastate")
seast_regression("seastate_CNW2"                             "seastate")
seast_regression("seastate_WaveMod7_WaveStMod1"              "seastate")
seast_regression("seastate_WaveMod7_WaveStMod2"              "seastate")
seast_regression("seastate_WaveMod7_WaveStMod3"              "seastate")
seast_regression("seastate_WvCrntMod1"                       "seastate")
seast_regression("seastate_WvCrntMod2"                       "seastate")
seast_regression("seastate_CurrMod3"                         "seastate")
seast_regression("seastate_wavemod5"                         "seastate")   # place at end since it reads outputs generated by seastate_wr_kin1
py_seast_regression("py_seastate_1"                          "seastate;python")

# MoorDyn regression tests
md_regression("md_5MW_OC4Semi"                                "moordyn")
md_regression("md_lineFail"                                   "moordyn")
md_regression("md_BodiesAndRods"                              "moordyn")
md_regression("md_bodyDrag"                                   "moordyn")
md_regression("md_cable"                                      "moordyn")
md_regression("md_case2"                                      "moordyn")
md_regression("md_case5"                                      "moordyn")
md_regression("md_float"                                      "moordyn")
md_regression("md_horizontal"                                 "moordyn")
md_regression("md_no_line"                                    "moordyn")
md_regression("md_vertical"                                   "moordyn")
md_regression("md_BdyExtLdDmpg"                               "moordyn")
md_regression("md_VIV"                                        "moordyn")
md_regression("md_waterkin2"                                  "moordyn")
md_regression("md_waterkin3"                                  "moordyn")
py_md_regression("py_md_5MW_OC4Semi"                          "moordyn;python")
# the following tests are excessively slow in double precision, so skip these in normal testing
#md_regression("md_Single_Line_Quasi_Static_Test"              "moordyn")
md_regression("md_viscoelastic"                               "moordyn")
md_regression("md_syrope"                                     "moordyn")

#  OpenFAST IO Library regression tests
py_openfast_io_library_pytest("openfast_io_library" "openfast_io;python")

# AeroDisk regression tests
adsk_regression("adsk_timeseries_shutdown"                    "aerodisk")

# SimplifiedElastoDyn regression tests
sed_regression("sed_test_HSSbrk"                              "simple-elastodyn")
sed_regression("sed_test_freewheel"                           "simple-elastodyn")

# Wavetank library interface (MD + SS + AD)
py_wavetank_regression("py_wavetank_test1"                    "wavetank;aerodyn;moordyn;seastate;python;scaled")
