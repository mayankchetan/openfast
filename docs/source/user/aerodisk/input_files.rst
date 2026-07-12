.. _adsk_input-files:

Input and Output Files
======================


Units
-----

AeroDisk uses the SI system (kg, m, s, N). 

.. _adsk_input-file:

Input file
----------

The AeroDisk input file defines the general inputs required for the actuator
disk calculations.  The following inputs may be changed by the user to achieve
the desired behaviour.

Simulation Control
~~~~~~~~~~~~~~~~~~

**echo** [switch]

   Write the input file contents to a file <RootName>.ADsk.ech.  This is useful
   for diagnosing errors reported about the input file.

**DT** [seconds]

   Integration time step for AeroDisk to use, or _"default"_ to use the glue code
   time step.

Environmental Conditions
~~~~~~~~~~~~~~~~~~~~~~~~

**AirDens** [kg/m^3]

   Air density, or _"default"_ to use the air density from the glue code

Actuator Disk Properties
~~~~~~~~~~~~~~~~~~~~~~~~

**RotorRad** [m]

   Radius of the rotor, or _"default"_ to use the value passed from the glue
   code

The lookup table for the disk actuator forces and moments follows. The data in
this table is flexible as it allows for a very simple lookup based on a single
variable (**TSR** for example), or up to four variables.  The last six columns
of the table must include the six force and moment coefficients that correspond
to a set of conditions given in the first set of columns.

**InColNames** [-]

   Comma separated List of column names corresponding to the variable columns in
   the input file.  See below for options.

**InColDims** [-]

   Comma separted list of the number  unique entries for each of the named
   variable column names.  The number of rows in the table must be equal to the
   product of all numbers given.  Must be the same number of entries as given in
   **InColNames**


For the input variable columns in the table, at least one column must be given,
with a maximum of four of the five listed below (**TSR** and **RtSpd** are
mutually exclusive).

**TSR** [-]

   Tip Speed Ratio, cannot be used with _RtSpd_

**RtSpd** [rpm]

   Rotor speed, cannot be used with _TSR_

**VRel** [m/s]

   Relative velocity of wind normal to rotor

**Pitch** [deg]

   Collective blade pitch

**Skew** [deg]

   Skew angle of inflow.  If this is not provided, the affect of skew is modeled
   as :math:`(cos(\chi))^2`
   

The remaining six columns of the table must contain the force and moment
coefficents.  See the example table below.



Sample input file
~~~~~~~~~~~~~~~~~

Note that the table given below is for illustration of the format and does not
represent any particular turbine.

.. code::
    
   --- AERO DISK INPUT FILE -------
   Sample actuator disk input file
   --- SIMULATION CONTROL ---------
    FALSE        echo            - Echo input data to "<RootName>.ADsk.ech" (flag)
     "default"   DT              - Integration time step (s)
   --- ENVIRONMENTAL CONDITIONS ---
         1.225   AirDens         - Air density (kg/m^3) (or "default")
   --- ACTUATOR DISK PROPERTIES ---
        63.0     RotorRad        - Rotor radius (m) (or "default")
   "RtSpd,VRel"  InColNames      - Input column headers (string) {may include a combination of "TSR, RtSpd, VRel, Pitch, Skew"} (up to 4 columns) [choose TSR or RtSpd,VRel; if Skew is absent, Skew is modeled as (COS(Skew))^2]
   9,2           InColDims       - Number of unique values in each column (-) (must have same number of columns as InColName) [each >=2]
   RtSpd     VRel      C_Fx   C_Fy   C_Fz   C_Mx   C_My   C_Mz
   (rpm)     (m/s)     (-)    (-)    (-)    (-)    (-)    (-)
     3.0      9.0     0.2347  0.0    0.0   0.0306  0.0    0.0
     4.0      9.0     0.2349  0.0    0.0   0.0314  0.0    0.0
     5.0      9.0     0.2350  0.0    0.0   0.0322  0.0    0.0
     6.0      9.0     0.2351  0.0    0.0   0.0330  0.0    0.0
     7.0      9.0     0.2352  0.0    0.0   0.0338  0.0    0.0
     8.0      9.0     0.2352  0.0    0.0   0.0346  0.0    0.0
     9.0      9.0     0.2351  0.0    0.0   0.0353  0.0    0.0
    10.0      9.0     0.2350  0.0    0.0   0.0361  0.0    0.0
    11.0      9.0     0.2349  0.0    0.0   0.0368  0.0    0.0
     3.0     12.0     0.7837  0.0    0.0   0.0663  0.0    0.0
     4.0     12.0     0.7733  0.0    0.0   0.0663  0.0    0.0
     5.0     12.0     0.7628  0.0    0.0   0.0663  0.0    0.0
     6.0     12.0     0.7520  0.0    0.0   0.0662  0.0    0.0
     7.0     12.0     0.7409  0.0    0.0   0.0660  0.0    0.0
     8.0     12.0     0.7297  0.0    0.0   0.0658  0.0    0.0
     9.0     12.0     0.7182  0.0    0.0   0.0656  0.0    0.0
    10.0     12.0     0.7066  0.0    0.0   0.0653  0.0    0.0
    11.0     12.0     0.6947  0.0    0.0   0.0649  0.0    0.0
   --- OUTPUTS --------------------
                 OutList         - The next line(s) contains a list of output parameters.  See OutListParameters.xlsx for a listing of available output channels, (-)
   END of input file (the word "END" must appear in the first 3 columns of this last OutList line)
   --------------------------------






.. _adsk_outputs:

Outputs
-------

The write outputs are:
 -  "ADSpeed":    Actuator disk rotational speed               (rpm)   
 -  "ADTSR":      Actuator disk tip-speed ratio                (-)   
 -  "ADPitch":    Actuator-disk collective blade-pitch angle   (deg)   
 -  "ADVWindx, ADVWindy, ADVWindz":    Actuator-disk-averaged wind velocity in the local coordinate system  (m/s)
 -  "ADSTVx, ADSTVy, ADSTVz":          Actuator-disk structural translational velocity in the local coordinate system  (m/s)
 -  "ADVRel":     Actuator-disk-averaged relative wind speed    (m/s)
 -  "ADSkew":     Actuator-disk inflow-skew angle               (deg)
 -  "ADCp, ADCt, ADCq":   Actuator-disk power, thrust, and torque coefficients  (-)
 -  "ADFx, ADFy, ADFz":   Actuator disk aerodynamic force loads in the local coordinate system (N)
 -  "ADMx, ADMy, ADMz":   Actuator disk aerodynamic moment loads in the local coordinate system (N-m)
 -  "ADPower":   Actuator disk power   (W)


.. _adsk-yaml-input:

YAML input file
----------------

The AeroDisk primary input file may also be written in YAML (name it ``*.yaml``
or ``*.yml``); see :ref:`yaml_input` for the conventions shared by all modules.
Parameters keep their documented names, grouped into sections that mirror the
text format's banners: ``general`` (``Echo``, ``DT``), ``environmental_conditions``
(``AirDens``), ``actuator_disk`` (``RotorRad`` and the rotor-performance table),
and ``output`` (``OutList``). ``DT``, ``AirDens``, and ``RotorRad`` each accept
the scalar ``default`` exactly like ``"default"``/``DEFAULT`` in the text format,
falling back to the value the glue code (or driver) supplies.

The Actuator Disk Properties table is written under ``actuator_disk:table`` using
the ``columns``/``rows`` convention (:ref:`yaml_input`):

- ``columns`` lists the index-variable names actually used, in any order/subset
  of ``TSR``, ``RtSpd``, ``VRel``, ``Pitch``, ``Skew`` — the text format's
  ``InColNames``. ``TSR`` and ``RtSpd`` are mutually exclusive, exactly as in the
  text format.
- ``dims`` gives the number of unique values expected for each name in
  ``columns`` (same order/length) — the text format's ``InColDims``. The number
  of rows in the table must equal the product of these counts.
- ``rows`` is the full data grid: one row per combination of the index values,
  each row holding the ``columns`` values (in that order) followed by the six
  fixed coefficient columns ``C_Fx``, ``C_Fy``, ``C_Fz``, ``C_Mx``, ``C_My``,
  ``C_Mz`` — the same column layout as the text-format table body.

.. code-block:: yaml

   # AeroDisk primary input file (YAML form)
   general:
     Echo: false
     DT: default             # or a number of seconds

   environmental_conditions:
     AirDens: default        # or a value in kg/m^3

   actuator_disk:
     RotorRad: default        # or a value in meters
     table:
       columns: [RtSpd, VRel]
       dims: [9, 2]
       rows:
         - [3.0,   9.0, 0.2347, 0.0, 0.0, 0.0306, 0.0, 0.0]
         - [4.0,   9.0, 0.2349, 0.0, 0.0, 0.0314, 0.0, 0.0]
         - [5.0,   9.0, 0.2350, 0.0, 0.0, 0.0322, 0.0, 0.0]
         - [6.0,   9.0, 0.2351, 0.0, 0.0, 0.0330, 0.0, 0.0]
         - [7.0,   9.0, 0.2352, 0.0, 0.0, 0.0338, 0.0, 0.0]
         - [8.0,   9.0, 0.2352, 0.0, 0.0, 0.0346, 0.0, 0.0]
         - [9.0,   9.0, 0.2351, 0.0, 0.0, 0.0353, 0.0, 0.0]
         - [10.0,  9.0, 0.2350, 0.0, 0.0, 0.0361, 0.0, 0.0]
         - [11.0,  9.0, 0.2349, 0.0, 0.0, 0.0368, 0.0, 0.0]
         - [3.0,  12.0, 0.7837, 0.0, 0.0, 0.0663, 0.0, 0.0]
         - [4.0,  12.0, 0.7733, 0.0, 0.0, 0.0663, 0.0, 0.0]
         - [5.0,  12.0, 0.7628, 0.0, 0.0, 0.0663, 0.0, 0.0]
         - [6.0,  12.0, 0.7520, 0.0, 0.0, 0.0662, 0.0, 0.0]
         - [7.0,  12.0, 0.7409, 0.0, 0.0, 0.0660, 0.0, 0.0]
         - [8.0,  12.0, 0.7297, 0.0, 0.0, 0.0658, 0.0, 0.0]
         - [9.0,  12.0, 0.7182, 0.0, 0.0, 0.0656, 0.0, 0.0]
         - [10.0, 12.0, 0.7066, 0.0, 0.0, 0.0653, 0.0, 0.0]
         - [11.0, 12.0, 0.6947, 0.0, 0.0, 0.0649, 0.0, 0.0]

   output:
     OutList: [ADSpeed, ADTSR, ADPitch, ADCp, ADCt, ADCq]

AeroDisk's input file may also be given inline under an OpenFAST primary (.fst)
file's ``input_files:AeroFile`` (only legal when ``CompAero`` selects AeroDisk;
see :ref:`yaml_input`).

.. _adsk-driver-yaml-input:

YAML driver input file
-----------------------

The standalone AeroDisk driver's own input file (normally ``*.dvr``) may also be
written in YAML (name it ``*.yaml`` or ``*.yml``); the driver detects the format
from the file extension, exactly like the primary input file above. Parameters
keep their documented names, grouped into sections that mirror the text driver
format's banners: ``general`` (``Echo``), ``primary_file`` (``ADskIptFile``,
``OutRootName``), ``geometry_environment`` (``AirDens``, ``RotorRad``,
``RotorHeight``, ``ShftTilt``), and ``case_analysis`` (``TStart``, ``DT``,
``NumTimeSteps``, and the combined case time/data table). Unlike the AeroDisk
*primary* file's ``AirDens``/``RotorRad``, the driver's copies of those two
values (used as fallback defaults when the primary file itself says
``default``) do not accept the ``default`` keyword; only ``DT`` and
``NumTimeSteps`` do, falling back to the values derived from the case data
table.

The case-analysis table (time, wind velocity, rotor speed, pitch, and yaw versus
time) is written under ``case_analysis:table`` as either:

- ``file``: a path to a plain time-series data file, resolved relative to the
  YAML driver file's own directory. Every AeroDisk r-test driver case sources
  its (typically large) time series this way; the referenced file keeps its
  original text layout (an optional ``#``/``!``/``%``-prefixed comment header,
  then whitespace-delimited ``Time WndSpeed_X WndSpeed_Y WndSpeed_Z RotSpd
  Pitch Yaw`` rows) and is never inlined into the YAML document, mirroring the
  text driver format's own ``@filename`` inclusion convention for this table.
- ``rows``: a YAML list of block mappings, one per case timestep, each keyed by
  column name (``Time``, ``WndSpeed_X``, ``WndSpeed_Y``, ``WndSpeed_Z``,
  ``RotSpd``, ``Pitch``, ``Yaw``) -- for a table given as literal inline data
  rather than an external file.

Exactly one of ``file``/``rows`` must be present. ``RotSpd`` (rpm), ``Pitch``
(deg), and ``Yaw`` (deg) are converted to rad/s and radians respectively,
exactly as in the text driver format.

.. code-block:: yaml

   # AeroDisk driver input file (YAML form)
   general:
     Echo: true

   primary_file:
     ADskIptFile: "adsk_primary.yaml"
     OutRootName: "adsk_driver"

   geometry_environment:
     AirDens: 1.225
     RotorRad: 63.0
     RotorHeight: 80.0
     ShftTilt: 0

   case_analysis:
     TStart: 0.0
     DT: default
     NumTimeSteps: 2750
     table:
       file: "adsk_TimeseriesInput.csv"
