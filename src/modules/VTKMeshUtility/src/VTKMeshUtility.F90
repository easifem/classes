! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Build VTK unstructured grid data from mesh, FEDOF, and fields
!
! This module converts easifem data to the arrays of a VTK unstructured
! grid, which are used by VTK writers (VTKHDFFile_, etc.):
!
! - points(3, nPoints): coordinates of points
! - connectivity(:): zero based point ids, VTK local node ordering
! - offsets(0:nCells): offsets(i) is the end of cell i in connectivity
! - types(nCells): VTK cell types
!
! Two kinds of grids are supported:
!
! - Linear grid: the points are the nodes of the mesh, and the cells
!   are the (geometric) elements of the mesh. Point data are the
!   vertex DOFs of the fields.
! - Lagrange grid: each cell has its own (not shared) Lagrange
!   interpolation points of the order of FEDOF (equidistance points).
!   The field is evaluated at these points by using the shape
!   functions of FEDOF. Therefore, any FEDOF (Lagrange, hierarchical,
!   etc.) is supported, and higher order solutions are visualized with
!   higher order (Lagrange) VTK cells.

MODULE VTKMeshUtility
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE FEDOF_Class, ONLY: FEDOF_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: INT8
IMPLICIT NONE
PRIVATE

PUBLIC :: VTKGetLinearGrid
PUBLIC :: VTKGetLagrangeGrid
PUBLIC :: VTKGetVertexValues
PUBLIC :: VTKGetLagrangeValues
PUBLIC :: VTKGetFieldCompo

!----------------------------------------------------------------------------
!                                                VTKGetLinearGrid@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: VTK unstructured grid from the nodes and elements of a mesh
!
! The points are all the nodes of the mesh in local node order.
! The cells are the active elements of the mesh. The VTK cell type and
! node ordering are obtained from GetVTKElementType_ (easifemBase).
! Inactive elements are skipped. Meshes with different element types
! are supported.
!
! Arguments:
!   mesh : CLASS(AbstractMesh_) - mesh
!   points : REAL(DFP) - coordinates, SIZE(points) = (3, nPoints)
!   connectivity : INTEGER(I4B) - zero based point ids of all cells
!   offsets : INTEGER(I4B) - SIZE(offsets) = nCells + 1, offsets(1) = 0
!   types : INTEGER(INT8) - VTK cell types, SIZE(types) = nCells
!
! Formula:
!
! The point ids of cell \(i\) are
!
! $$ \text{connectivity}(\text{offsets}(i)+1:\text{offsets}(i+1)) $$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEMesh_Class, ONLY: FEMesh_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: INT8
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
! USE VTKMeshUtility, ONLY: VTKGetLinearGrid
! IMPLICIT NONE
! TYPE(HDF5File_) :: meshfile
! TYPE(FEMesh_) :: amesh
! REAL(DFP), ALLOCATABLE :: points(:, :)
! INTEGER(I4B), ALLOCATABLE :: connectivity(:), offsets(:)
! INTEGER(INT8), ALLOCATABLE :: types(:)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL meshfile%Initiate(fileName="./meshdata/small_tri3_mesh.h5", &
!                        MODE="READ")
! CALL meshfile%OPEN()
! CALL amesh%Initiate(hdf5=meshfile, dim=2)
! CALL VTKGetLinearGrid(mesh=amesh, points=points, &
!                       connectivity=connectivity, offsets=offsets, &
!                       types=types)
! CALL OK(SIZE(points, 2) .EQ. amesh%GetTotalNodes(), "nPoints")
! CALL OK(SIZE(types) .EQ. amesh%GetTotalElements(), "nCells")
! CALL OK(ALL(types .EQ. 5_INT8), "VTK_TRIANGLE")
! CALL OK(offsets(SIZE(offsets)) .EQ. SIZE(connectivity), "offsets")
! CALL amesh%DEALLOCATE()
! CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[VTKGetLagrangeGrid]], [[VTKGetVertexValues]]

INTERFACE
  MODULE SUBROUTINE VTKGetLinearGrid(mesh, points, connectivity, &
                                     offsets, types)
    CLASS(AbstractMesh_), INTENT(INOUT) :: mesh
    !! mesh
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: points(:, :)
    !! coordinates of points, shape (3, nPoints)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: connectivity(:)
    !! zero based point ids of cells
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: offsets(:)
    !! offsets of cells in connectivity, size nCells + 1
    INTEGER(INT8), ALLOCATABLE, INTENT(INOUT) :: types(:)
    !! VTK cell types, size nCells
  END SUBROUTINE VTKGetLinearGrid
END INTERFACE

!----------------------------------------------------------------------------
!                                               VTKGetLagrangeGrid@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: VTK unstructured grid of Lagrange cells from FEDOF
!
! For each active element of the mesh of fedof:
!
! 1. The cell order \(p\) is obtained from fedof%GetCellOrder.
! 2. The VTK cell type and node ordering for the topology and order
!    are obtained from GetVTKElementTypeFromTopology_ (easifemBase),
!    e.g., VTK_QUADRATIC_TRIANGLE for a triangle with \(p=2\), and
!    VTK_LAGRANGE_QUADRILATERAL for a quadrangle with \(p \ge 3\).
! 3. The equidistance interpolation points of order \(p\) are
!    computed in the reference element and mapped to the physical
!    element by using the shape functions of geofedof.
!
! The points are not shared between cells (discontinuous grid). The
! same loop is used in [[VTKGetLagrangeValues]], so the values
! computed there match the points computed here.
!
! An error is raised if a VTK cell type is not available for a
! topology and order (e.g., triangle with \(p \ge 3\)).
!
! Arguments:
!   fedof : CLASS(FEDOF_) - FEDOF of the field
!   geofedof : CLASS(FEDOF_) - FEDOF of the geometry
!   points : REAL(DFP) - coordinates, SIZE(points) = (3, nPoints)
!   connectivity : INTEGER(I4B) - zero based point ids of all cells
!   offsets : INTEGER(I4B) - SIZE(offsets) = nCells + 1, offsets(1) = 0
!   types : INTEGER(INT8) - VTK cell types, SIZE(types) = nCells
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: INT8
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! USE VTKMeshUtility, ONLY: VTKGetLagrangeGrid
! IMPLICIT NONE
! CHARACTER(*), PARAMETER :: tomlFileName = "VTKHDFFile_test.toml"
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_) :: fedof, geofedof
! TYPE(ScalarField_) :: u
! REAL(DFP), ALLOCATABLE :: points(:, :)
! INTEGER(I4B), ALLOCATABLE :: connectivity(:), offsets(:)
! INTEGER(INT8), ALLOCATABLE :: types(:)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(fileName=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       fileName=tomlFileName, tomlName="u")
! CALL VTKGetLagrangeGrid(fedof=fedof, geofedof=geofedof, &
!                         points=points, connectivity=connectivity, &
!                         offsets=offsets, types=types)
! ! order 2 triangle: VTK_QUADRATIC_TRIANGLE with 6 points
! CALL OK(ALL(types .EQ. 22_INT8), "VTK_QUADRATIC_TRIANGLE")
! CALL OK(SIZE(points, 2) .EQ. 6 * SIZE(types), "nPoints")
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[VTKGetLagrangeValues]], [[VTKGetLinearGrid]]

INTERFACE
  MODULE SUBROUTINE VTKGetLagrangeGrid(fedof, geofedof, points, &
                                       connectivity, offsets, types)
    CLASS(FEDOF_), INTENT(INOUT) :: fedof
    !! FEDOF of the field
    CLASS(FEDOF_), INTENT(INOUT) :: geofedof
    !! FEDOF of the geometry
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: points(:, :)
    !! coordinates of points, shape (3, nPoints)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: connectivity(:)
    !! zero based point ids of cells
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: offsets(:)
    !! offsets of cells in connectivity, size nCells + 1
    INTEGER(INT8), ALLOCATABLE, INTENT(INOUT) :: types(:)
    !! VTK cell types, size nCells
  END SUBROUTINE VTKGetLagrangeGrid
END INTERFACE

!----------------------------------------------------------------------------
!                                               VTKGetVertexValues@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Values of a nodal field at the points of the linear grid
!
! The first nPoints DOFs (vertex DOFs, in local node order of the mesh)
! of the field are returned. This is consistent with the points of
! [[VTKGetLinearGrid]].
!
! - ScalarField_: values(1, nPoints)
! - VectorField_: values(3, nPoints), missing components are zero
!
! An error is raised for other field types.
!
! Arguments:
!   field : CLASS(AbstractNodeField_) - ScalarField_ or VectorField_
!   nPoints : INTEGER(I4B) - number of points of the linear grid
!   values : REAL(DFP) - SIZE(values) = (ncomp, nPoints)
!
! See also: [[VTKGetLinearGrid]], [[VTKGetFieldCompo]]
! (an example is given in VTKHDFFile_Class, WritePointData)

INTERFACE
  MODULE SUBROUTINE VTKGetVertexValues(field, nPoints, values)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: field
    !! nodal field (ScalarField_ or VectorField_)
    INTEGER(I4B), INTENT(IN) :: nPoints
    !! number of points
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: values(:, :)
    !! values, shape (ncomp, nPoints)
  END SUBROUTINE VTKGetVertexValues
END INTERFACE

!----------------------------------------------------------------------------
!                                             VTKGetLagrangeValues@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Values of a nodal field at the points of the Lagrange grid
!
! The field is evaluated at the equidistance interpolation points of
! each active cell (see [[VTKGetLagrangeGrid]]) by using the shape
! functions of field%fedof:
!
! $$ u(\xi_k) = \sum_{I} N^I(\xi_k) u_I $$
!
! - ScalarField_: values(1, nPoints)
! - VectorField_: values(3, nPoints), missing components are zero
!
! Arguments:
!   field : CLASS(AbstractNodeField_) - ScalarField_ or VectorField_
!   values : REAL(DFP) - SIZE(values) = (ncomp, nPoints)
!
! See also: [[VTKGetLagrangeGrid]], [[VTKGetFieldCompo]]
! (an example is given in VTKHDFFile_Class, WritePointData)

INTERFACE
  MODULE SUBROUTINE VTKGetLagrangeValues(field, values)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: field
    !! nodal field (ScalarField_ or VectorField_)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: values(:, :)
    !! values, shape (ncomp, nPoints)
  END SUBROUTINE VTKGetLagrangeValues
END INTERFACE

!----------------------------------------------------------------------------
!                                                 VTKGetFieldCompo@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Number of VTK components of a nodal field
!
! Returns 1 for ScalarField_, 3 for VectorField_, and 0 for the field
! types which are not supported by VTK writers.
!
! Arguments:
!   field : CLASS(AbstractNodeField_) - nodal field
!
! Returns:
!   ans : INTEGER(I4B) - number of components (0, 1, or 3)
!
! See also: [[VTKGetVertexValues]], [[VTKGetLagrangeValues]]

INTERFACE
  MODULE FUNCTION VTKGetFieldCompo(field) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(IN) :: field
    !! nodal field
    INTEGER(I4B) :: ans
    !! number of components
  END FUNCTION VTKGetFieldCompo
END INTERFACE

END MODULE VTKMeshUtility
