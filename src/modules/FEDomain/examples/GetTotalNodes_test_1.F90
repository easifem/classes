!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the total number of nodes
!
! GetTotalNodes returns the number of nodes of the domain, or of
! the mesh of dimension dim. The operator .tNodes. does the same.
!
! The mesh very_small_quad4_mesh.h5 is the unit square: four
! Quadrangle4 elements (13 to 16), eight Line2 elements (5 to 12) on
! the boundary curves, four Point elements (1 to 4), and nine nodes:
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
  USE FEDomain_Class, ONLY: FEDomain_
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
  TYPE(HDF5File_) :: meshfile
  TYPE(FEDomain_) :: obj

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, group="")

  CALL OK(obj%GetTotalNodes() .EQ. 9_I4B, "domain")
  CALL OK(obj%GetTotalNodes(DIM=1_I4B) .EQ. 8_I4B, "curve mesh")
  CALL OK(obj%GetTotalNodes(DIM=0_I4B) .EQ. 4_I4B, "point mesh")
  CALL OK((.tNodes.obj) .EQ. 9_I4B, ".tNodes. obj")
  CALL OK((obj.tNodes.1_I4B) .EQ. 8_I4B, "obj .tNodes. dim")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
