.. _sed_input-files:

Input and Output Files
======================


Units
-----

SED uses the SI system (kg, m, s, N). 

.. _sed_input-file:

Input file
----------


.. code::
    
    ------- SIMPLIFIED ELASTODYN INPUT FILE ----------------------------------------
   Comment
    ---------------------- SIMULATION CONTROL --------------------------------------
    False         Echo        - Echo input data to "<RootName>.ech" (flag)
              3   Method      - Integration method: {1: RK4, 2: AB4, or 3: ABM4} (-)
    "default"     DT          - Integration time step (s)
    ---------------------- DEGREES OF FREEDOM --------------------------------------
    True          GenDOF      - Generator DOF (flag)
    ---------------------- INITIAL CONDITIONS --------------------------------------
              0   Azimuth     - Initial azimuth angle for blades (degrees)
              0   BlPitch     - Blades initial pitch (degrees)
            0.0   RotSpeed    - Initial or fixed rotor speed (rpm)
              0   NacYaw      - Initial or fixed nacelle-yaw angle (degrees)
              0   PtfmPitch   - Fixed pitch tilt rotational displacement of platform (degrees)
    ---------------------- TURBINE CONFIGURATION -----------------------------------
              3   NumBl       - Number of blades (-)
             63   TipRad      - The distance from the rotor apex to the blade tip (meters)
            1.5   HubRad      - The distance from the rotor apex to the blade root (meters)
           -2.5   PreCone     - Blades cone angle (degrees)
        -5.0191   OverHang    - Distance from yaw axis to rotor apex [3 blades] or teeter pin [2 blades] (meters)
             -5   ShftTilt    - Rotor shaft tilt angle (degrees)
        1.96256   Twr2Shft    - Vertical distance from the tower-top to the rotor shaft (meters)
           87.6   TowerHt     - Height of tower above ground level [onshore] or MSL [offshore] (meters)
    ---------------------- MASS AND INERTIA ----------------------------------------
         115926   RotIner     - Rot inertia about rotor axis [blades + hub] (kg m^2)
        534.116   GenIner     - Generator inertia about HSS (kg m^2)
    ---------------------- DRIVETRAIN ----------------------------------------------
            100   GBoxEff     - Gearbox efficiency (%)
             97   GBRatio     - Gearbox ratio (-)
    ---------------------- OUTPUT --------------------------------------------------
                  OutList     - The next line(s) contains a list of output parameters.  See OutListParameters.xlsx for a listing of available output channels, (-)
    "Azimuth"                 - Blades azimuth angle
    "RotSpeed"                - Low-speed shaft rotational speed
    "RotAcc"                  - Low-speed shaft rotational acceleration
    END of input file (the word "END" must appear in the first 3 columns of this last OutList line)
    -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------





.. _sed_outputs:

Outputs
-------

The write outputs are:
 -  "Azimuth" : Blades azimuth angle (deg) between 0 and 2pi
 -  "RotSpeed": Low-speed shaft rotational speed (rpm)
 -  "RotAcc": Low-speed shaft rotational acceleration (rad/s^2)
 -  "GenSpeed": High-speed shaft rotational speed (rpm)
 -  "GenAcc": High-speed shaft rotational acceleration (rad/s^2)


.. _sed-yaml-input:

YAML input file
----------------

The SED primary input file may also be written in YAML (name it ``*.yaml`` or
``*.yml``); see :ref:`yaml_input` for the conventions shared by all modules.
Parameters keep their documented names, grouped into sections that mirror the
text format's banners: ``general`` (``Echo``, ``IntMethod``, ``DT``),
``degrees_of_freedom`` (``GenDOF``, ``YawDOF``), ``initial_conditions``
(``Azimuth``, ``BlPitch``, ``RotSpeed``, ``NacYaw``, ``PtfmPitch``),
``turbine_configuration`` (``NumBl``, ``TipRad``, ``HubRad``, ``PreCone``,
``OverHang``, ``ShftTilt``, ``Twr2Shft``, ``TowerHt``), ``mass_and_inertia``
(``RotIner``, ``GenIner``), ``drivetrain`` (``GBoxRatio``), and ``output``
(``OutList``). Only ``DT`` accepts the scalar ``default`` exactly like
``"default"``/``DEFAULT`` in the text format, falling back to the time step the
glue code (or driver) supplies; every other key is required. Angle and speed
values (``Azimuth``, ``BlPitch``, ``RotSpeed``, ``NacYaw``, ``PtfmPitch``,
``PreCone``, ``ShftTilt``) are given in the same units as the text format
(degrees, or rpm for ``RotSpeed``) and are converted internally exactly as the
text-format reader does.

.. code-block:: yaml

   # Simplified ElastoDyn (SED) primary input file (YAML form)
   general:
     Echo: false
     IntMethod: 3             # 1: RK4, 2: AB4, 3: ABM4
     DT: default              # or a number of seconds

   degrees_of_freedom:
     GenDOF: true
     YawDOF: true

   initial_conditions:
     Azimuth: 0               # deg
     BlPitch: 0               # deg
     RotSpeed: 12.0           # rpm
     NacYaw: 0                # deg
     PtfmPitch: 0             # deg

   turbine_configuration:
     NumBl: 3
     TipRad: 63.0             # m
     HubRad: 1.5              # m
     PreCone: -2.5            # deg
     OverHang: -5.0191        # m
     ShftTilt: -5             # deg
     Twr2Shft: 1.96256        # m
     TowerHt: 87.6            # m

   mass_and_inertia:
     RotIner: 38677052        # kg m^2
     GenIner: 534.116         # kg m^2

   drivetrain:
     GBoxRatio: 97

   output:
     OutList: [Azimuth, RotSpeed, RotAcc, GenSpeed, GenAcc]

SED's input file may also be given inline under an OpenFAST primary (.fst)
file's ``input_files:EDFile`` (only legal when ``CompElast`` selects Simplified
ElastoDyn; ``EDFile`` also serves ElastoDyn when ``CompElast`` selects that
module instead -- see :ref:`yaml_input`).

.. _sed-driver-yaml-input:

YAML driver input file
-----------------------

The standalone SED driver's own input file (normally ``*.dvr``) may also be
written in YAML (name it ``*.yaml`` or ``*.yml``); the driver detects the
format from the file extension, exactly like the primary input file above.
Parameters keep their documented names, grouped into sections that mirror the
text driver format's banners: ``general`` (``Echo``), ``primary_file``
(``SEDIptFile``, ``OutRootName``), ``output`` (``WrVTK``), and
``case_analysis`` (``TStart``, ``DT``, ``NumTimeSteps``, and the combined case
time/data table). ``TStart`` is always required and does not accept the
``default`` keyword; only ``DT`` and ``NumTimeSteps`` do, falling back to the
values derived from the case data table.

The case-analysis table (time, aerodynamic torque, HSS-brake torque, generator
torque, blade-pitch command, yaw, and yaw rate versus time) is written under
``case_analysis:table`` as either:

- ``file``: a path to a plain time-series data file, resolved relative to the
  YAML driver file's own directory. Every SED r-test driver case sources its
  time series this way; the referenced file keeps its original text layout (an
  optional ``#``/``!``/``%``-prefixed comment header, then whitespace-delimited
  ``Time AerTrq HSSBrTrqC GenTrq BlPitchCom Yaw YawRate`` rows) and is never
  inlined into the YAML document, mirroring the text driver format's own
  ``@filename`` inclusion convention for this table.
- ``rows``: a YAML list of block mappings, one per case timestep, each keyed by
  column name (``Time``, ``AerTrq``, ``HSSBrTrqC``, ``GenTrq``,
  ``BlPitchCom``, ``Yaw``, ``YawRate``) -- for a table given as literal inline
  data rather than an external file.

Exactly one of ``file``/``rows`` must be present. ``HSSBrTrqC`` is forced
positive (its sign is not meaningful); ``BlPitchCom``, ``Yaw`` (deg), and
``YawRate`` (deg/s) are converted to radians and radians/s respectively,
exactly as in the text driver format.

.. code-block:: yaml

   # Simplified ElastoDyn (SED) driver input file (YAML form)
   general:
     Echo: true

   primary_file:
     SEDIptFile: "sed_primary.yaml"
     OutRootName: "sed_driver"

   output:
     WrVTK: 0

   case_analysis:
     TStart: 0.0
     DT: default
     NumTimeSteps: 99
     table:
       file: "Free.csv"
