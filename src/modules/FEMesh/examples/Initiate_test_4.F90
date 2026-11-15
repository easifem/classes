!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Initiate a mesh from a group of the mesh file
!
! When only group is given, the mesh is read from that group. The
! node coordinates are searched in group, in group/nodeCoord, and
! in the parent group of group.
!
! The mesh very_small_quad4_mesh.h5 is the unit square with four
! Quadrangle4 elements (global numbers 13 to 16) and nine nodes:
!
!   3 ---- 6 ---- 4
!   |  14  |  16  |
!   7 ---- 9 ---- 8
!   |  13  |  15  |
!   1 ---- 5 ---- 2

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEMesh_Class, ONLY: FEMesh_
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/very_small_quad4_mesh.h5"
  TYPE(FEMesh_) :: obj
  TYPE(HDF5File_) :: meshfile

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()

  CALL obj%Initiate(hdf5=meshfile, group="/surfaceEntities_1")
  CALL OK(obj%IsInit(), "IsInit")
  CALL OK(obj%GetTotalElements() .EQ. 4_I4B, "GetTotalElements")
  CALL OK(obj%GetTotalNodes() .EQ. 9_I4B, "GetTotalNodes")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
