!> author: Vikas Sharma, Ph. D.
! date: 2025-05-30
! summary:  Generate a 2D quadrangle mesh with two regions

PROGRAM main
USE GmshStructuredMesh_Class
USE Gmsh_Class
USE FPL
USE GlobalData
USE MSHFile_Class
USE HDF5File_Class

IMPLICIT NONE

TYPE(GmshStructuredMesh_) :: obj
TYPE(Gmsh_) :: gmsh
TYPE(ParameterList_) :: param
CHARACTER(*), PARAMETER :: title = "very_small_quad4_mesh_two_region"
REAL(DFP), PARAMETER :: pointsOnAxis1(3) = [0.0, 1.0, 2.0]
REAL(DFP), PARAMETER :: pointsOnAxis2(2) = [0.0, 1.0]

INTEGER(I4B), PARAMETER :: transfinitePointsOnAxis1(2) = [3, 3]
INTEGER(I4B), PARAMETER :: transfinitePointsOnAxis2(1) = [2]

INTEGER(I4B) :: ierr

TYPE(MSHFile_) :: mshFile
TYPE(HDF5File_) :: hdf5file

CALL FPL_Init()
CALL param%Initiate()

CALL SetGmshStructuredMeshParam( &
  param=param, &
  filename=title//".msh", &
  pointsOnAxis1=pointsOnAxis1, &
  pointsOnAxis2=pointsOnAxis2, &
  transfinitePointsOnAxis1=transfinitePointsOnAxis1, &
  transfinitePointsOnAxis2=transfinitePointsOnAxis2, &
  recombineAll=.TRUE.)

CALL obj%Initiate(param)

ierr = gmsh%initialize()
ierr = gmsh%model%add("GmshStructuredMesh2D")
CALL obj%Generate(gmsh)
! ierr = gmsh%fltk%run()
ierr = gmsh%finalize()

CALL param%DEALLOCATE()
CALL obj%DEALLOCATE()

CALL mshFile%Initiate(filename=title//'.msh', STATUS="OLD", ACTION="READ")
CALL mshFile%OPEN()
CALL mshFile%READ()
CALL hdf5file%Initiate(title//'.h5', MODE="NEW")
CALL hdf5file%OPEN()
CALL mshFile%Export(hdf5=hdf5file, group="")
CALL mshFile%DEALLOCATE()
CALL hdf5file%DEALLOCATE()

END PROGRAM main
