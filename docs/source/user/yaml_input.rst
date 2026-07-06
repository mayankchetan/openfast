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

- InflowWind primary input file (:ref:`ifw-yaml-input`)
