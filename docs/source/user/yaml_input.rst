.. _yaml_input:

YAML Input Files
================

OpenFAST supports two input-file formats: the traditional text format and, for a
growing set of modules, an equivalent YAML format. Either may be used for any
supported file, and the two may be mixed freely within one model (a text ``.fst``
may reference a YAML module file and vice versa). Both formats are parsed into the
same internal data, so simulation results are identical.

Format detection
----------------

The file extension decides the format, everywhere: a file named ``*.yaml`` or
``*.yml`` (case-insensitive) is parsed as YAML; any other name is parsed as the
traditional text format. There are no flags and no content sniffing.

Schema conventions
------------------

- Each file is one YAML mapping. Top-level keys are section names derived from the
  text format's banner comments (for example ``general``, ``steady_wind``,
  ``output``); the keys inside each section are the documented parameter names
  (``TMax``, ``WindType``, ``OutList``). Key lookup is case-insensitive.
- Keys within a section may appear in any order, and unrecognized keys produce a
  warning naming the key, file, and line — typos cannot silently do nothing.
- Counts are not separate inputs: where the text format has a count plus a list
  (``NWindVel`` + ``WindVxiList``), the YAML format has only the list, and the
  count is its length.
- The scalar ``default`` requests a parameter's default value, exactly like
  ``DEFAULT`` in the text format.
- ``OutList`` is a sequence of channel names (no terminating ``END``)::

     output:
       SumPrint: false
       OutList: [Wind1VelX, Wind1VelY, Wind1VelZ]

- Tables are written as a ``columns`` list plus ``rows`` of flow sequences::

     tower_stations:
       columns: [HtFract, TMassDen, TwFAStif, TwSSStif]
       rows:
         - [0.0, 5590.87, 614.34e9, 614.34e9]
         - [1.0, 1086.71,  41.57e9,  41.57e9]

- File references are plain string values, resolved relative to the referencing
  file, exactly as in the text format.

Comments and echo
-----------------

``#`` comments are allowed anywhere. When ``Echo`` is true, the echo file contains
a verbatim copy of every physical input file — comments included — under banners
naming each file.

Reuse: includes, anchors, and merge keys
----------------------------------------

``!include path.yaml`` splices another YAML file's content at that position. Paths
are resolved relative to the *including* file (the same rule as the text format's
``@filename``), includes may nest (to a depth of 10), and cycles are detected and
reported.

Standard YAML anchors (``&name``), aliases (``*name``), and merge keys
(``<<: *name``) are fully supported for reusing and overriding blocks of input —
for example, defining one turbine and copying it with a single change::

   turbines:
     - &T1
       elastodyn: ED.yaml
       aerodyn:   AD.yaml
       servodyn:  SrvD_T1.yaml
     - <<: *T1                    # turbine 2 = turbine 1 ...
       servodyn: SrvD_T2.yaml     # ... except its ServoDyn file

Merge keys follow the standard YAML rules: keys written explicitly in the host
mapping always win, and when several aliases are merged (``<<: [*a, *b]``) the
earlier one takes precedence.

A FAST.Farm primary deck's ``turbines`` sequence (:ref:`FF:Input:YAML`) is
the same pattern at farm scale -- anchor the shared fields on turbine 1 and
merge-with-override on every other turbine, so a common high-resolution-grid
spacing is written once::

   turbines:
     - &T1
       WT_X: 0.0
       WT_Y: 0.0
       WT_Z: 0.0
       WT_FASTInFile: WT1.fst
       X0_High: -63.0
       Y0_High: -63.0
       Z0_High: 0.0
       dX_High: 3.0
       dY_High: 3.0
       dZ_High: 3.0
     - <<: *T1                    # turbine 2 = turbine 1's high-res grid ...
       WT_X: 630.0                # ... except its position ...
       WT_FASTInFile: WT2.fst     # ... and its own OpenFAST primary file

Inline module input: the uniform value rule
--------------------------------------------

In a YAML OpenFAST primary file, every entry under ``input_files`` follows one
rule, at every level:

- a **string** value is a file path (text or YAML, decided by its extension);
- a **mapping** value is that module's input, written inline.

Inlining removes the separate module file entirely — the module's YAML schema
simply nests under its ``input_files`` key::

   input_files:
     EDFile: ED.dat                # text module file (path)
     AeroFile: AD.yaml             # YAML module file (path)
     InflowFile:                   # inline module input (mapping)
       general:
         WindType: 1
         ...
       steady_wind:
         HWindSpeed: 12
         RefHt: 90
         PLexp: 0.2

File paths written *inside* an inline section resolve relative to the deck
file itself (there is no separate module file to be relative to). When moving
an existing multi-file model into a single deck, rewrite any relative paths in
the inlined sections accordingly. (This runtime semantic is unchanged; it only
matters when hand-authoring or hand-editing a single-file deck. The
``reg_tests`` YAML-equivalence converter's ``--single-file`` mode
(``yamlDeckConverter.py``'s ``convert_fst``) already does this rewrite for
you automatically whenever an inlined module file's own directory differs
from the deck's.)

Formats mix freely: one deck may combine text files, YAML files, and inline
sections. Inline input is available for modules whose YAML schema exists
(currently InflowWind; AeroDisk or AeroDyn when ``CompAero`` selects one of
them; ElastoDyn or Simplified ElastoDyn when ``CompElast`` selects one of
them; ServoDyn when ``CompServo`` selects it; SeaState when ``CompSeaSt``
selects it; HydroDyn when ``CompHydro`` selects it; SubDyn or ExtPtfm when
``CompSub`` selects one of them; and MoorDyn, FEAMooring, or OrcaFlex when
``CompMooring`` selects one of them);
an inline mapping for any other module -- or for ``MooringFile`` when
``CompMooring``
selects a module without a YAML schema (MAP++) -- is a
clear fatal error suggesting a file path instead. Combined with ``!include``
and anchors, this supports fully single-file models.

.. note::

   **MAP++ is text-only.** MAP++ (``CompMooring`` = 1) has no YAML reader --
   its input file is parsed by the external MAP++ (C++) library, which
   understands only the legacy text format. Both an inline ``MooringFile``
   mapping *and* a ``.yaml``/``.yml`` ``MooringFile`` path are rejected with a
   MAP++-specific fatal error. For a YAML mooring input, use MoorDyn
   (``CompMooring`` = 3) or FEAMooring (``CompMooring`` = 2). Second-order files (e.g.
Structural Control files under ServoDyn, potential-flow data under HydroDyn,
AeroDyn's airfoil/blade/tailfin/AeroAcoustics/OLAF files, ElastoDyn's
blade/tower/furling files, or MoorDyn's bathymetry/water-kinematics/lookup-
table files) are never inlined: they stay referenced by path, in text or YAML
form where a schema exists (of the above, only Structural Control files have
one; the binary/tabular potential-flow data files themselves are always text).

OpenFAST primary file (.fst)
----------------------------

The YAML form of the OpenFAST primary file mirrors the text format's banners as
sections: ``description`` (a string), ``simulation_control``,
``feature_switches``, ``environment``, ``input_files``, ``output``,
``linearization``, and ``visualization``. Keys are the documented parameter
names (``TMax``, ``CompElast``, ``OutFileFmt``, ...). Differences from the text
format:

- ``NRotors`` is not an input. It is ``1 +`` the number of entries in the
  optional ``input_files:rotors`` sequence. Each ``rotors`` entry is a mapping
  with ``EDFile``, ``ServoFile``, and (when ``CompElast`` is 2) ``BDBldFile``
  for that additional rotor.
- ``EDFile`` serves ElastoDyn (``CompElast`` 1 or 2) and Simplified ElastoDyn
  (``CompElast`` 3); both have a YAML schema, so inline mapping input under
  ``EDFile`` is accepted for either.
- ``BDBldFile`` is a sequence of blade-file paths (its length gives the number
  of BeamDyn blade files); it is required only when ``CompElast`` is 2.
- ``MirrorRotor`` (under ``feature_switches``) is a sequence of true/false
  values, required with exactly ``NRotors`` entries only for multirotor models.
- ``NLinTimes`` is not an input: it is the length of
  ``linearization:LinTimes``. The whole ``linearization`` section may be
  omitted when ``Linearize`` is false, as may ``visualization`` when ``WrVTK``
  is 0 — every entry in those sections has a default.
- Unused module files are simply omitted (no ``"unused"`` placeholders): only
  the files for enabled modules are required.

A complete, runnable single-file example — demonstrating comments,
``!include``, an anchor/alias pair, and an inline InflowWind section — is kept
at ``reg_tests/yaml-examples/AWT_YFix_WSt_single_file.yaml``.

A multirotor deck reuses rotor definitions with anchors and merge keys::

   input_files:
     EDFile: &ed  ED_rotor1.yaml
     ServoFile: SrvD_rotor1.yaml
     ...
     rotors:                      # rotor 2..N; NRotors = 1 + list length
       - EDFile: *ed              # same ElastoDyn input as rotor 1
         ServoFile: SrvD_rotor2.yaml

Interoperability
----------------

Every OpenFAST YAML input file is a valid standard YAML document, readable by any
YAML library (PyYAML, ruamel.yaml, yq, editors). ``!include`` is standard *syntax*
(a local tag) whose meaning is defined by OpenFAST; a generic loader sees a tagged
string rather than the spliced content. OpenFAST never gives custom meaning to
standard syntax.

For simplicity and unambiguity, OpenFAST accepts a documented subset of YAML.
The following are rejected with errors naming the file and line: block scalars
(``|``, ``>``), multi-document streams, tab characters in indentation, duplicate
keys within a mapping, and tags other than ``!include``.

Error reporting
---------------

Errors name the key path, the file, and the line, through includes and copies::

   >> The required key "general:WindType" was not found in "IfW.yaml" (mapping starting on line 3).
   >> The key "general:PropagationDir" on line #7 of "IfW.yaml" was not assigned a valid REAL value. Text: "0,0"

Supported files
---------------

The set of files accepted in YAML form is growing module by module; each module's
documentation describes its YAML schema alongside the text format. Currently
supported:

- OpenFAST primary file (``.fst``) — see above
- InflowWind primary input file (:ref:`ifw-yaml-input`), including inline use
  under ``input_files:InflowFile``
- AeroDisk primary input file (:ref:`adsk-yaml-input`), including inline use
  under ``input_files:AeroFile`` (when ``CompAero`` selects AeroDisk)
- ElastoDyn primary input file (:ref:`elastodyn-yaml-input`), including inline
  use under ``input_files:EDFile`` (when ``CompElast`` selects ElastoDyn);
  blade, tower, and furling files stay referenced by path
- Simplified ElastoDyn (SED) primary input file (:ref:`sed-yaml-input`),
  including inline use under ``input_files:EDFile`` (when ``CompElast`` selects
  Simplified ElastoDyn)
- ServoDyn primary input file (:ref:`servodyn-yaml-input`), including inline
  use under ``input_files:ServoFile`` (when ``CompServo`` selects ServoDyn)
- Structural Control (StC) input file (:ref:`stc-yaml-input`), referenced by
  path from a ServoDyn deck (never inlined)
- SeaState primary input file (:ref:`seastate-yaml-input`), including inline
  use under ``input_files:SeaStFile`` (when ``CompSeaSt`` selects SeaState)
- HydroDyn primary input file (:ref:`hydrodyn-yaml-input`), including inline
  use under ``input_files:HydroFile`` (when ``CompHydro`` selects HydroDyn);
  potential-flow data files (``PotFile``/``GeoFile``) stay referenced by path
- AeroDyn primary input file (:ref:`aerodyn-yaml-input`), including inline use
  under ``input_files:AeroFile`` (when ``CompAero`` selects AeroDyn); airfoil,
  blade, tailfin, AeroAcoustics, and OLAF files stay referenced by path
- MoorDyn primary input file (:ref:`moordyn-yaml-input`), including inline use
  under ``input_files:MooringFile`` (when ``CompMooring`` selects MoorDyn,
  ``CompMooring`` = 3);
  bathymetry grids, water-kinematics files, stiffness/damping lookup tables,
  and Syrope working-curve files stay referenced by path
- FEAMooring primary input file (:ref:`feamooring-yaml-input`), including inline
  use under ``input_files:MooringFile`` (when ``CompMooring`` selects FEAMooring,
  ``CompMooring`` = 2). FEAMooring's primary input references no further data
  files, so nothing stays a path -- every field is inlined
- SubDyn primary input file (:ref:`subdyn-yaml-input`), including inline use
  under ``input_files:SubFile`` (when ``CompSub`` selects SubDyn, ``CompSub`` = 1)
- ExtPtfm_MCKF primary input file (:ref:`extptfm-yaml-input`), including inline
  use under ``input_files:SubFile`` (when ``CompSub`` selects ExtPtfm,
  ``CompSub`` = 2). Its Guyan/Craig-Bampton reduced superelement file
  (``Red_FileName``) and the connection/user-forcing time-series files
  (``Conn_FileName``, ``Force_FileName``, ``FConn_FileName``) stay referenced by
  path
- BeamDyn primary input file (:ref:`beamdyn-yaml-input`), referenced by path
  from a glue-code deck's ``BDBldFile`` entries (when ``CompElast`` selects
  ElastoDyn + BeamDyn); BeamDyn has no inline-input glue path, so
  ``BDBldFile`` is always a file path, never inlined. The blade properties
  file stays referenced by path.
- IceDyn primary input file (:ref:`icedyn-yaml-input`), including inline use
  under ``input_files:IceFile`` (when ``CompIce`` selects IceDyn,
  ``CompIce`` = 2). IceDyn's primary input references no further data files,
  so nothing stays a path -- every field is inlined.
- IceFloe primary input file (:ref:`icefloe-yaml-input`), referenced by path
  from ``input_files:IceFile`` (when ``CompIce`` selects IceFloe,
  ``CompIce`` = 1). IceFloe has no inline-input glue path, so ``IceFile`` is
  always a file path, never inlined.
- OrcaFlex Interface primary input file (:ref:`orcaflex-yaml-input`),
  including inline use under ``input_files:MooringFile`` (when ``CompMooring``
  selects OrcaFlex, ``CompMooring`` = 4). The OrcaFlex simulation input
  (``DirRoot``) and the OrcaFlex DLL (``DLL_FileName``) stay referenced by
  path. **Coverage gap:** this YAML/inline path is implemented but not
  exercised by any reg-test -- there is no ``CompMooring`` = 4 deck in the
  test suite, and exercising it requires the proprietary OrcaFlex DLL, which
  is unavailable in CI.
- **Module driver input files** — the standalone ``*_driver`` executables now
  read their own driver input file (normally ``<base>.dvr``) in YAML
  (``<base>.yaml``) as well as text, independently of the module primary file
  each driver points at: AeroDisk (:ref:`adsk-driver-yaml-input`), Simplified
  ElastoDyn / SED (:ref:`sed-driver-yaml-input`), SeaState
  (:ref:`seastate-driver-yaml-input`), InflowWind
  (:ref:`ifw-driver-yaml-input`), SubDyn (:ref:`subdyn-driver-yaml-input`),
  BeamDyn (:ref:`beamdyn-driver-yaml-input`), HydroDyn
  (:ref:`hydrodyn-driver-yaml-input`), MoorDyn
  (:ref:`moordyn-driver-yaml-input`), AeroDyn
  (:ref:`aerodyn-driver-yaml-input`), and UnsteadyAero
  (:ref:`ua-driver-yaml-input`). Each driver's second-order files stay
  path-valued (the module primary file itself, plus that module's own
  airfoil/blade files, ``PRPInputsFile``, prescribed-motion ``InputsFile``,
  and time-series files as applicable). A driver's own input file and the
  module-primary file it points at are each independently text-or-YAML: a
  YAML driver deck may point at a text module primary and vice versa.
- FAST.Farm primary input file (:ref:`FF:Input:YAML`) — ``.fstf`` becomes
  ``.yaml``/``.yml``; ``turbines`` is a sequence of block mappings
  (``WT_X``/``WT_Y``/``WT_Z`` position, ``WT_FASTInFile`` path, plus the
  high-resolution-grid columns when ``Mod_AmbWind`` is 2 or 3) and is the
  natural home for YAML anchors and merge keys -- see "Reuse: includes,
  anchors, and merge keys" above.
  Each turbine's ``.fst``, ``MD_FileName``, ``WindFilePath``, ``InflowFile``,
  ``WindDirPrefix``, and ``WAT_BoxFile`` stay path-valued. Current scope is
  paths-only turbines (``WT_FASTInFile`` is always a path, never an inline
  mapping of that turbine's own module inputs); fully-inline turbine
  definitions are a documented follow-up.
- TurbSim primary input file (:ref:`TurbSim_yaml_input`) — ``.inp`` becomes
  ``.yaml``/``.yml``; the literal ``default`` token is preserved exactly as
  in the text format (TurbSim's default-aware readers consume it the same
  way); user-defined profile, spectra, and time-series files stay
  path-valued.


.. _feamooring-yaml-input:

FEAMooring YAML input file
--------------------------

The FEAMooring primary input file may also be written in YAML (name it
``*.yaml`` or ``*.yml``); the conventions above apply. (FEAMooring has no formal
module-documentation page of its own -- see the FEAMooring Theory Manual and
User's Guide linked from :ref:`user_guide` -- so its YAML schema is documented
here.) Top-level keys mirror the text format's section banners:
``simulation_control``, ``lines``, ``output``, and ``outputs``. When
``CompMooring`` selects FEAMooring (``CompMooring`` = 2), a glue-code YAML
primary file may inline the whole ``MooringFile`` section as a mapping instead of
a path, following the general inline-input rule above.

Unlike the text format, the following counts are never given explicitly -- they
derive from list lengths: **NumLines** (the ``lines`` sequence length) and
**NumOuts** (the ``outputs:OutList`` length). ``NumElems`` (the finite-element
count per line) is a scalar, not a list count, so it is kept.

Notable schema points:

- ``simulation_control:DT``, ``:Gravity``, and ``:WtrDens`` each accept the
  literal ``default`` (the glue code's coupling interval / gravitational
  acceleration / water density is used) or a number, exactly like the text
  format.
- ``lines`` is a sequence with one mapping per mooring line. Each mapping holds
  the line's material and geometry scalars (``LEAStiff``, ``LMassDen``,
  ``LDMassDen``, ``LineCI``, ``LineCD``, ``LUnstrLen``, ``BottmStiff``,
  ``LRadAnch``, ``LAngAnch``, ``LDpthAnch``, ``LRadFair``, ``LAngFair``,
  ``LDrftFair``, ``Tension``) and ``GSL``, a 3-element list of linear spring
  stiffnesses in x, y, z. The anchor/fairlead azimuth angles ``LAngAnch`` and
  ``LAngFair`` are given in **degrees**, exactly as in the text format.
- FEAMooring's primary input references no second-order / external data files,
  so nothing stays a path -- every field is inlined.
- There is no ``FileFormat``/legacy-format branch: the primary file is a single
  fixed-schema file, so the YAML schema is a straight one-to-one of it.

.. code-block:: yaml

   # FEAMooring primary input file (YAML form)
   simulation_control:
     Echo: false
     DT: default
     NumElems: 20
     Gravity: default
     WtrDens: default
     MaxIter: 100
     Eps: 1e-4

   lines:
     - LEAStiff: 7.536E8
       LMassDen: 113.35
       LDMassDen: 4.72
       LineCI: 0
       LineCD: 6.67377
       LUnstrLen: 835.35
       BottmStiff: 1.0E4
       LRadAnch: 837.6
       LAngAnch: 60.0
       LDpthAnch: 200.0
       LRadFair: 40.868
       LAngFair: 60.0
       LDrftFair: 14.0
       Tension: 1.0E6
       GSL: [1.0E10, 1.0E10, 1.0E10]

   output:
     SumPrint: true
     OutFile: 1
     TabDelim: true
     OutFmt: "G0"
     Tstart: 0

   outputs:
     OutList: ["FairT1", "AnchT1"]


.. _icedyn-yaml-input:

IceDyn YAML input file
-----------------------

The IceDyn primary input file may also be written in YAML (name it ``*.yaml``
or ``*.yml``); the conventions above apply. (IceDyn has no formal
module-documentation page of its own -- see the Ice Module Manual linked from
:ref:`user_guide` -- so its YAML schema is documented here.) Top-level keys
mirror the text format's section banners: ``structure_properties``,
``ice_models``, ``ice_general``, and ``ice_model_1`` through ``ice_model_6``
(one section per ice-model number, each holding that model's parameters
regardless of which ``IceModel`` is actually selected -- exactly like the text
format, which reads every model's block unconditionally). When ``CompIce``
selects IceDyn (``CompIce`` = 2), a glue-code YAML primary file may inline the
whole ``IceFile`` section as a mapping instead of a path, following the
general inline-input rule above; IceDyn is initialized once per
support-structure leg, and the inline input is applied identically to every
leg.

Unlike the text format, **NumLegs** is never given explicitly -- it derives
from the length of ``structure_properties:LegPosX`` (``LegPosY`` and
``StWidth`` must have the same length).

Notable schema points:

- IceDyn's primary input references no second-order / external data files, so
  nothing stays a path -- every field is inlined.
- There is no ``OutList``/outputs section: IceDyn's text reader never reads an
  output-channel list from its primary file, so the YAML schema has none
  either.
- There is no ``FileFormat``/legacy-format branch: the primary file is a
  single fixed-schema file, so the YAML schema is a straight one-to-one of it.

.. code-block:: yaml

   # IceDyn primary input file (YAML form)
   structure_properties:
     LegPosX: [0]
     LegPosY: [0]
     StWidth: [6]

   ice_models:
     IceModel: 6
     IceSubModel: 1

   ice_general:
     IceVel: 0.1
     IceThks: 0.8
     WtDen: 1000
     IceDen: 900
     InitLoc: 0.0
     InitTm: 0.0
     Seed1: 2
     Seed2: 5

   ice_model_1:
     Ikm: 2.7
     Ag: 3.5e6
     Qg: 65000
     Rg: 8.314
     Tice: 269
     Poisson: 0.3
     WgAngle: 90.0
     EIce: 9.5
     SigNm: 5

   ice_model_2:
     Pitch: 1.0
     IceStr2: 5.0
     Delmax2: 1.0

   ice_model_3:
     ThkMean: 0.5
     ThkVar: 0.04
     VelMean: 0.001
     VelVar: 1e-6
     TeMean: 15
     StrMean: 5
     StrVar: 1
     DelMean: 0.1
     DelVar: 0.01
     PMean: 0.2
     PVar: 0.01

   ice_model_4:
     PrflMean: 0
     PrflSig: 0.02
     ZoneNo1: 10
     ZoneNo2: 1
     ZonePitch: 0.27
     IceStr: 5.0
     Delmax: 0.027

   ice_model_5:
     ConeAgl: 55.0
     ConeDwl: 8.0
     ConeDtp: 1.0
     RdupThk: 0.3
     mu: 0.3
     FlxStr: 0.7
     StrLim: 0.1
     StrRtLim: 1e-2

   ice_model_6:
     FloeLth: 800
     FloeWth: 800
     CPrAr: 5.0
     dPrAr: -0.5
     Fdr: 9
     Kic: 140
     FspN: 3.3

.. _icefloe-yaml-input:

IceFloe YAML input file
-------------------------

The IceFloe primary input file may also be written in YAML (name it ``*.yaml``
or ``*.yml``); the conventions above apply. IceFloe has no formal
module-documentation page of its own, and unlike every other module covered
here it also has no fixed field-by-field schema: the text format is a flat
list of ``NAME value`` lines (comment lines start with ``!``, ``#``, ``$``, or
``%``), read into a name/value table that is queried by name wherever the
code needs a parameter, rather than parsed into named fields up front. The
YAML form mirrors this directly: every top-level key of the document becomes
one name/value entry, using the same names the text format's comments
document (see the Ice Module Manual linked from :ref:`user_guide` for the
full parameter list). Parameter names are matched case-insensitively (as
substrings, exactly like the text-format reader), so YAML keys should match
the text format's names.

When ``CompIce`` selects IceFloe (``CompIce`` = 1), ``input_files:IceFile``
must still be a path to a ``.yaml``/``.yml`` (or text) file -- IceFloe has no
inline-input glue path, unlike IceDyn.

.. code-block:: yaml

   # IceFloe primary input file (YAML form)
   iceType: 4
   timeStep: 0.25
   duration: 60.0
   rampTime: 10.0
   numLegs: 1
   refIceThick: 0.5
   refIceStrength: 500.0e3
   staticExponent: -0.5


.. _orcaflex-yaml-input:

OrcaFlex Interface YAML input file
-----------------------------------

The OrcaFlex Interface primary input file may also be written in YAML (name
it ``*.yaml`` or ``*.yml``); the conventions above apply. (The OrcaFlex
Interface has no formal module-documentation page of its own -- see the
OrcaFlex Interface User's Guide linked from :ref:`user_guide` -- so its YAML
schema is documented here.) The text-format primary file is tiny: it reads
only the ``Echo`` switch, ``DirRoot`` (the OrcaFlex simulation input file),
and ``DLL_FileName`` (the OrcaFlex DLL); the ``DT`` and ``OutList`` reads in
the text-format reader are commented out and are never exercised, so the
YAML schema does not define them either -- the module always reports its
fixed 18-channel output list. Top-level keys mirror the text format's single
section banner: ``simulation_control``. When ``CompMooring`` selects OrcaFlex
(``CompMooring`` = 4), a glue-code YAML primary file may inline the whole
``MooringFile`` section as a mapping instead of a path, following the general
inline-input rule above.

Notable schema points:

- ``simulation_control:Echo`` is optional and, if present, is accepted and
  discarded: in the text format Echo only controls whether an echo file of
  the *text* input is written while reading, and never becomes part of
  ``InputFileData``; there is nothing analogous to echo when parsing an
  already-in-memory YAML document.
- ``DirRoot`` (the OrcaFlex simulation input file) and ``DLL_FileName`` (the
  OrcaFlex DLL) are read as raw path strings, exactly as the text format
  stores them before its own relative-path resolution; they are never
  inlined or converted -- both name external files (the OrcaFlex simulation
  data file and DLL) that the OrcaFlex Interface loads separately at run
  time.
- There is no ``OutList``/outputs section: unlike most modules, the OrcaFlex
  Interface's text-format reader has its ``OutList``/``NumOuts`` reads
  commented out and always emits the full fixed set of 18 output channels
  from ``Orca_Init``, so the YAML schema defines none either.
- There is no ``FileFormat``/legacy-format branch: the primary file is a
  single fixed-schema file, so the YAML schema is a straight one-to-one of
  it.
- **Coverage gap:** OrcaFlex YAML/inline input is implemented but not
  exercised by any reg-test. There is no ``CompMooring`` = 4 deck in the test
  suite, and OrcaFlex requires the proprietary OrcaFlex DLL, which is
  unavailable in CI, so this path cannot be regression-tested there.

.. code-block:: yaml

   # OrcaFlex Interface primary input file (YAML form)
   simulation_control:
     Echo: false
     DirRoot: OrcaFlexModel.dat
     DLL_FileName: OrcaFlexInterface_x64.dll
