#
# Copyright 2026 National Renewable Energy Laboratory
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

"""
Smoke test for the curated hand-written YAML example deck.

Stages the AWT_YFix_WSt case files plus the AWT27 turbine data, drops in the
single-file YAML example deck (comments, !include, anchor/alias, inline
InflowWind) from reg_tests/yaml-examples/, runs OpenFAST on it, and requires a
clean exit with a binary output file. This guards the reuse features that the
converter-driven equivalence tests do not exercise.

Usage:
    python3 executeYamlExampleSmoke.py executable sourceDirectory buildDirectory

    executable      - path to the openfast executable
    sourceDirectory - path to the local r-test repository
    buildDirectory  - directory in build/reg_tests where the case is staged
                      (the case is staged one level below so ../AWT27 resolves)
"""

import argparse
import shutil
import subprocess
import sys
from pathlib import Path

parser = argparse.ArgumentParser(description="Runs the curated YAML example deck as a smoke test.")
parser.add_argument("executable", type=Path, help="path to the openfast executable")
parser.add_argument("sourceDirectory", type=Path, help="path to the r-test repository")
parser.add_argument("buildDirectory", type=Path, help="staging parent directory in the build tree")
args = parser.parse_args()
args.executable = args.executable.resolve()
args.sourceDirectory = args.sourceDirectory.resolve()
args.buildDirectory = args.buildDirectory.resolve()

case_src = args.sourceDirectory / "glue-codes" / "openfast" / "AWT_YFix_WSt"
turbine_src = args.sourceDirectory / "glue-codes" / "openfast" / "AWT27"
examples_src = Path(__file__).resolve().parent / "yaml-examples"

for required in (args.executable, case_src, turbine_src, examples_src):
    if not required.exists():
        sys.exit(f"error: {required} does not exist")

# Stage at the standard case depth so relative paths (../AWT27) resolve.
case_dir = args.buildDirectory / "AWT_YFix_WSt_yamlexample"
turbine_dir = args.buildDirectory / "AWT27"
if case_dir.exists():
    shutil.rmtree(case_dir)
shutil.copytree(case_src, case_dir)
if turbine_dir.exists():
    shutil.rmtree(turbine_dir)
shutil.copytree(turbine_src, turbine_dir)
for example_file in examples_src.iterdir():
    shutil.copy(example_file, case_dir)

deck = case_dir / "AWT_YFix_WSt_single_file.yaml"
completed = subprocess.run(
    [str(args.executable), str(deck.name)],
    cwd=case_dir,
    capture_output=True,
    text=True,
)
sys.stdout.write(completed.stdout)
sys.stderr.write(completed.stderr)
if completed.returncode != 0:
    sys.exit(f"error: OpenFAST exited with status {completed.returncode} on {deck}")

outb = case_dir / "AWT_YFix_WSt_single_file.outb"
if not outb.exists():
    sys.exit(f"error: expected output file {outb} was not created")

print("yaml example smoke test passed")
