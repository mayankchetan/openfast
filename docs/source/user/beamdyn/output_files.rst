.. _bd-output-files:
   
Output Files
============

BeamDyn produces three types of output files, depending on the options
selected: an echo file, a summary file, and a time-series results file.
The following sections detail the purpose and contents of these files.

Echo File
---------

If the user sets the ``Echo`` flag to TRUE in the BeamDyn primary
input file, the contents of this file will be echoed to a file with the
naming convention ``InputFile.ech``. The echo file is helpful for
debugging the input files. The contents of an echo file will be
truncated if BeamDyn encounters an error while parsing an input file.
The error usually corresponds to the line after the last successfully
echoed line.

.. _sum-file:

Summary File
------------

In stand-alone mode, BeamDyn generates a summary file with the naming
convention, ``InputFile.sum`` if the ``SumPrint`` parameter is set
to TRUE. When coupled to FAST, the summary file is named
``InputFile.BD.R*.B*.sum.yaml``. This file summarizes key information about the
simulation, including:

-  Blade mass.

-  Blade length.

-  Blade center of mass.

-  Initial global position vector in BD coordinate system.

-  Initial global rotation tensor in BD coordinate system.

-  Analysis type.

-  Numerical damping coefficients.

-  Time step size.

-  Maximum number of iterations in the Newton-Raphson solution.

-  Convergence parameter in the stopping criterion.

-  Factorization frequency in the Newton-Raphson solution.

-  Numerical integration (quadrature) method.

-  FE mesh refinement factor used in trapezoidal quadrature.

-  Number of elements.

-  Number of FE nodes.

-  Initial position vectors of FE nodes in BD coordinate system.

-  Initial rotation vectors of FE nodes in BD coordinate system.

-  Quadrature point position vectors in BD coordinate system. For Gauss
   quadrature, the physical coordinates of Gauss points are listed. For
   trapezoidal quadrature, the physical coordinates of the quadrature
   points are listed.

-  Sectional stiffness and mass matrices at quadrature points in local
   blade reference coordinate system. These are the data being used in
   calculations at quadrature points and they can be different from the
   section in Blade Input File since BeamDyn linearly interpolates the
   sectional properties into quadrature points based on need.

-  Initial displacement vectors of FE nodes in BD coordinate system.

-  Initial rotational displacement vectors of FE nodes in BD coordinate
   system.

-  Initial translational velocity vectors of FE nodes in BD coordinate
   system.

-  Initial angular velocity vectors of FE nodes in BD coordinate system.

-  Requested output information.

All of these quantities are output in this file in the BD coordinate
system, the one being used internally in BeamDyn calculations. The
initial blade reference coordinate system, denoted by a subscript
:math:`r0` that follows the IEC standard, is related to the internal BD
coordinate system by :numref:`IECBD` in :numref:`beamdyn-theory`.

Summary file: internal representation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When coupled to FAST and ``SumPrint = TRUE``, the YAML-format summary
file additionally includes a block of entries documenting the FE basis
and the reference-line fit used internally, under the comment header
``# --- FE basis and reference-line fit (internal representation of
user inputs)``:

-  ``GLL_nodes_xi`` — GLL (FE) node locations in element natural
   coordinate [-1,1].

-  ``QP_xi`` — Quadrature point locations in element natural coordinate
   [-1,1].

-  ``Shp`` — Shape functions Shp(i,j)=N_i(QP_xi(j)): Lagrange
   interpolants on the GLL nodes, evaluated at quadrature points.

-  ``ShpDer`` — Shape function derivatives dN_i/dxi at quadrature
   points.

-  ``Jacobian`` — Jacobian d(arclength)/d(xi) at each quadrature point
   (rows) per element (columns).

-  ``kp_fit_order`` — Nodes (qfit) in the least-squares GLL-basis fit
   of the keypoint reference line, per element; polynomial order =
   qfit-1.

-  ``kp_fit_coef_E<i>`` — Reference-line fit for element ``<i>``:
   nodal values (columns X,Y,Z,twist) on the qfit-node GLL Lagrange
   basis; first/last rows pinned to first/last keypoint.

Note that the sectional stiffness and mass matrices are interpolated
linearly from the input stations to the quadrature points, while the
reference line is represented by a least-squares polynomial fit (order
≤ 7) through the key points — the fit does not, in general, pass
through interior key points; the ``kp_fit_coef_E*`` entries give the
fit actually used.

Results File
------------

The BeamDyn time-series results are written to a text-based file with
the naming convention ``DriverInputFile.out`` where
``DriverInputFile`` is the name of the driver input file when BeamDyn
is run in the stand-alone mode. If BeamDyn is coupled to FAST, then FAST
will generate a master results file that includes the BeamDyn results.
The results in ``DriverInputFile.out`` are in table format, where each
column is a data channel (the first column always being the simulation
time), and each row corresponds to a simulation time step. The data
channel are specified in the OUTPUT section of the primary input file.
The column format of the BeamDyn-generated file is specified using the
``OutFmt`` parameters of the primary input file.

