.. _TurbSim_appendix:

Appendix
========

.. _TurbSim_input_files:

TurbSim Input Files
-------------------


1) Primary TurbSim Input Files:
:download:`(TurbSim input file example) <examples/TurbSim.inp>`:

This is the primary input file for TurbSim. Most simulations will require only this file. 


2) TurbSim secondary input files for user-defined input

Input files that can be specified in the primary input file to import user-defined data.

:download:`(user-defined profiles example) <examples/TurbSim_User.profiles>`:

:download:`(user-defined spectra example) <examples/TurbSim_User.spectra>`:

:download:`(user-defined time-series example) <examples/TurbSim_User.timeSeriesInput>`:


.. _TurbSim_yaml_input:

YAML input file
----------------

The TurbSim primary input file may also be written in YAML (name it ``*.yaml``
or ``*.yml``); see :ref:`yaml_input` for the conventions shared by all
modules. The YAML schema mirrors the text format's section banners as
top-level keys: ``runtime_options``, ``turbine_model``,
``meteorological_boundary_conditions``,
``non_iec_meteorological_boundary_conditions``,
``spatial_coherence_parameters``, and
``coherent_turbulence_scaling_parameters``. All six sections are always
present at the file level -- TurbSim itself only actually reads the last
section for non-IEC spectral models (matching the text format, which always
carries those lines even when the IEC spectral models never consume them).

Several keys accept the literal scalar ``default`` in place of a value,
exactly like the text format's `"default"`/`"DEFAULT"` token (e.g.
``ETMc: default``, ``WindProfileType: default``, ``UStar: default``): TurbSim
computes the same derived default it would from an equivalent text deck, using
whatever other inputs that default depends on. Do not pre-resolve this token
to a numeric value when hand-writing or generating a YAML deck -- some
defaults are computed from other parameters (and, for a few fields, consume
random-number draws in the same sequence the text reader does), so
substituting a literal number can silently change the result. ``UserFile``,
``ProfileFile``, and ``CTEventPath`` remain external file paths in both
formats.

``InCDec1``, ``InCDec2``, and ``InCDec3`` (the u/v/w-component coherence
decrement and coherence-B pair) are carried as a single quoted two-number
string, exactly like the text format's `"12.0  0.00035273"` convention (or the
literal ``default``) -- not a YAML list -- since TurbSim parses that pair from
one input token in both formats.

.. code-block:: yaml

   # TurbSim primary input file (YAML form; abridged)
   runtime_options:
       Echo: false
       RandSeed1: 123456
       RandSeed2: 789012
       WrADFF: true
       ...
   turbine_model:
       NumGrid_Z: 6
       NumGrid_Y: 6
       TimeStep: 0.05
       ...
   meteorological_boundary_conditions:
       TurbModel: "IECVKM"
       UserFile: "TurbSim_User.spectra"
       IECstandard: "1-ED2"
       IECturbc: "A"
       IEC_WindType: "NTM"
       ETMc: default
       WindProfileType: default
       ...
   non_iec_meteorological_boundary_conditions:
       Latitude: default
       RICH_NO: 0.05
       UStar: default
       ...
   spatial_coherence_parameters:
       SCMod1: default
       InCDec1: default
       CohExp: default
       ...
   coherent_turbulence_scaling_parameters:
       CTEventPath: "./EventData"
       CTEventFile: "random"
       Randomize: true
       ...

