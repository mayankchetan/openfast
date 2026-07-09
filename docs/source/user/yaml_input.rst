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
``CompSub`` selects one of them; and MoorDyn or FEAMooring when ``CompMooring``
selects one of them);
an inline mapping for any other module -- or for ``MooringFile`` when
``CompMooring``
selects a module without a YAML schema (MAP++, OrcaFlex) -- is a
clear fatal error suggesting a file path instead. Combined with ``!include``
and anchors, this supports fully single-file models. Second-order files (e.g.
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
