This document describes how to get the mesh files required to run the examples.

In this director you will find a directory called `meshdata` which contains the fortran code for generating the mesh files using Gmsh. Please run those files to generate the mesh files. After generating the mesh files you can run the examples given here. The description of each file is given below.

1. ./meshdata/Generate3DMesh.F90: This file generates a 3D structured mesh of eight nodes brick elements.
2. ./meshdata/Generate3DMesh_two_regions.F90: This file generates a 3D structured mesh of eight nodes brick elements with two regions.
3. ./meshdata/GenerateMesh2D_triangle.F90: This file generates a 2D mesh of three and six nodes triangle elements. In this file you need to define the one of the following macro variables. Note that this program generates will create file in gmsh as well as hdf format.
   - `SMALL_TRI3_MESH`: This will generate a mesh of six nodes triangle elements.
   - `SMALL_TRI6_MESH`: This will generate a small mesh of six node triangle elements.
   - `BIG_TRI3_MESH`: This will generate a moderately big mesh of three nodes triangle elements.
   - `BIG_BIG_TRI3_MESH`: This will generate a big mesh of three nodes triangle elements
   - `BIG_TRI6_MESH`: This will generate a moderately big mesh of six nodes triangle elements.
   - `BIG_BIG_TRI6_MESH`: This will generate a big mesh of six nodes triangle elements
