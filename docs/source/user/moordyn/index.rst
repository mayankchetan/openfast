.. _MoorDyn:

MoorDyn Users Guide
====================

The documentation for MoorDyn is avaible `here <https://moordyn.readthedocs.io>`_. It features instructions 
for the use of MoorDynF, the module in OpenFAST, and MoorDynC, the standalone C++ code. Input file formats
are described in the `inputs section <https://moordyn.readthedocs.io/en/latest/inputs.html>`_
(`MoorDyn usage <https://moordyn.readthedocs.io/en/latest/inputs.html#the-v2-input-file>`_, specifically the section for V2),
usage of MoorDyn at the FAST.Farm level
(`MoorDyn with FAST.Farm <https://moordyn.readthedocs.io/en/latest/inputs.html#moordyn-with-fast-farm-inputs>`_),
and links to publications with the relevant theory.


Examples of how to use MoorDynF and MoorDynC can be found here:

`MoorDyn Example Uses <https://github.com/FloatingArrayDesign/MoorDyn/tree/dev/example>`_

.. _moordyn-yaml-input:

YAML input file
---------------

The MoorDyn primary input file may also be written in YAML (name it ``*.yaml``
or ``*.yml``); see :ref:`yaml_input` for the conventions shared by all modules.
The YAML schema mirrors the text format's free-form sections as top-level keys:
``line_types``, ``rod_types``, ``bodies``, ``rods``, ``points``, ``lines``,
``syrope_ic``, ``external_loads``, ``control``, ``failure``, ``options``, and
``outputs``. Every section is optional at the file level; MoorDyn applies its
own requirements (e.g. at least one line type and one line) identically for
both formats.

Each table section is a YAML list of mappings — one mapping per table row, with
the text format's column names as keys — so there are no count parameters: the
number of line types, bodies, rods, points, lines, control channels, and
failure conditions is the length of the corresponding list. Cell values are
carried exactly as the text format's tokens, including all of its keyword and
multi-value conventions:

- ``Attachment`` entries take the same keywords as the text format (``Fixed``,
  ``Free``, ``Coupled``, ``Vessel``, ``Body1``, ``Body1Pinned``, ``Turbine2``,
  ...), and line ends attach with the same ``<pointID>`` / ``R<rodID>A`` /
  ``R<rodID>B`` tokens.
- Bar-separated multi-value cells pass through as strings: a line type's ``EA``
  may be ``"3.27e9|1.17e10"`` (viscoelastic) or ``"SYROPE:<file>|alpha|beta"``,
  a body's ``CG``/``I``/``CdA``/``Ca`` may hold 1, 2, 3, or 6 bar-separated
  values with the text format's broadcasting rules, and so on. Quote any value
  containing ``|`` or ``:``.
- A stiffness/damping cell may also name a lookup-table file, and a point's
  ``Z`` accepts the ``seabed`` keyword — exactly as in the text format.
- ``line_types`` rows take the text format's three arities: the 10 base columns
  (``Name``, ``Diam``, ``MassDen``, ``EA``, ``BA``, ``EI``, ``Cd``, ``Ca``,
  ``CdAx``, ``CaAx``); optionally ``Cl`` (the 11-column VIV form, which leaves
  ``dF``/``cF`` at their defaults 0.08/0.18); optionally ``dF`` and ``cF``
  together (the 13-column form, requiring ``Cl``).
- ``control``, ``failure``, and ``syrope_ic`` rows give their line IDs as the
  list ``Lines`` (the text format's comma-separated ID run).
- ``options`` is a mapping of option keyword to value, keyword spellings and
  aliases exactly as documented for the text format (``dtM``, ``kBot``/``kb``,
  ``WtrDpth``/``depth``, ``rhoW``/``rho``, ``cv``/``fricDamp``, ...); entries
  are processed in file order. ``outputs:OutList`` is a list of channel names
  (no terminating ``END``).

Sub-files always stay referenced by path, resolved relative to the primary
input file: bathymetry grid files (a ``WtrDpth`` option naming a file),
water-kinematics files (``WaterKin``), stiffness/damping lookup tables, and
Syrope working-curve files.

.. code-block:: yaml

   # MoorDyn primary input file (YAML form; abridged)
   line_types:
       - Name: main
         Diam: 0.0766
         MassDen: 113.35
         EA: 7.536E8
         BA: -1.0
         EI: 0
         Cd: 2.0
         Ca: 0.8
         CdAx: 0.4
         CaAx: 0.25

   points:
       - {ID: 1, Attachment: Fixed,  X: 418.8,  Y: 725.383, Z: -200.0, M: 0, V: 0, CdA: 0, Ca: 0}
       - {ID: 2, Attachment: Vessel, X: 20.434, Y: 35.393,  Z: -14.0,  M: 0, V: 0, CdA: 0, Ca: 0}

   lines:
       - ID: 1
         LineType: main
         AttachA: 1
         AttachB: 2
         UnstrLen: 835.35
         NumSegs: 20
         Outputs: "-"          # per-line output flag characters, e.g. p, or - for none

   failure:
       - ID: 1
         Attachment: R1A       # a point ID, or R<rodID>A / R<rodID>B
         Lines: [2]
         FailTime: 15
         FailTen: 0

   options:
     dtM: 0.001
     kBot: 3.0e6
     cBot: 3.0e5
     TmaxIC: 60.0

   outputs:
     OutList: [FairTen1, FairTen2, FairTen3, AnchTen1, AnchTen2, AnchTen3]

MoorDyn's input may also be given inline under an OpenFAST primary (.fst)
file's ``input_files:MooringFile`` (only legal when ``CompMooring`` selects
MoorDyn; see :ref:`yaml_input`). Sub-file paths written inside an inline
section resolve relative to the deck file.

.. _moordyn-driver-yaml-input:

YAML driver input file
-----------------------

The standalone MoorDyn driver's own input file (normally ``*.inp``) may also
be written in YAML (name it ``*.yaml`` or ``*.yml``); the driver detects the
format from the file extension, exactly like the primary input file above.
Parameters keep their documented names, grouped into sections that mirror the
text driver format's banners: ``environmental_conditions`` (``Gravity``,
``rhoW``, ``WtrDpth``), ``moordyn`` (``MDInputFile``, ``OutRootName``,
``TMax``, ``dtC``), ``inputs`` (``InputsMod``, ``InputsFile``), and ``farm``
(``NumTurbines``, ``SeaStateFile``, ``initial_positions``). Unlike the primary
input file and the other Wave-4 module drivers, the MoorDyn driver never reads
an ``Echo`` flag at all, so there is no ``general`` section here.

``farm:initial_positions`` is a list of row mappings, one per turbine, giving
each turbine's ``ref_X``, ``ref_Y``, ``surge_init``, ``sway_init``,
``heave_init``, ``roll_init``, ``pitch_init``, and ``yaw_init`` -- the same
eight columns the text format's table carries. Its length must be exactly
``MAX(1, NumTurbines)``: when ``NumTurbines`` is 0 (normal, single-turbine
OpenFAST mode, as opposed to FAST.Farm mode) the list still carries exactly
one row. ``farm:SeaStateFile`` is optional (omit it, or leave it out
entirely, when MoorDyn's driver should not initialize SeaState) -- the same
backwards-compatible behavior as the text format's own missing-line
convention.

``MDInputFile``, ``OutRootName``, ``InputsFile``, and ``SeaStateFile`` all
remain file-path strings, resolved relative to the driver input file.

.. code-block:: yaml

   # MoorDyn driver input file (YAML form)
   environmental_conditions:
     Gravity: 9.80665
     rhoW: 1025.0
     WtrDpth: 200.0

   moordyn:
     MDInputFile: "moordyn.yaml"
     OutRootName: "driver"
     TMax: 60
     dtC: 0.0125

   inputs:
     InputsMod: 1
     InputsFile: "ptfm_motion.dat"

   farm:
     NumTurbines: 0
     initial_positions:
       - ref_X: 0
         ref_Y: 0
         surge_init: 5
         sway_init: 0
         heave_init: 0
         roll_init: 0
         pitch_init: 1.9
         yaw_init: 0
