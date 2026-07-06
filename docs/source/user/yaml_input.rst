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
the inlined sections accordingly.

Formats mix freely: one deck may combine text files, YAML files, and inline
sections. Inline input is available for modules whose YAML schema exists
(currently InflowWind; AeroDisk when ``CompAero`` selects AeroDisk; and
Simplified ElastoDyn when ``CompElast`` selects it); an inline mapping for any
other module -- or for ``AeroFile``/``EDFile`` when the corresponding switch
selects a module without a YAML schema (AeroDyn, or ElastoDyn) -- is a clear
fatal error suggesting a file path instead. Combined with ``!include`` and
anchors, this supports fully single-file models.

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
  (``CompElast`` 3); inline mapping input under ``EDFile`` is accepted only when
  ``CompElast`` selects Simplified ElastoDyn (ElastoDyn has no YAML schema yet).
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
- Simplified ElastoDyn (SED) primary input file (:ref:`sed-yaml-input`),
  including inline use under ``input_files:EDFile`` (when ``CompElast`` selects
  Simplified ElastoDyn)
