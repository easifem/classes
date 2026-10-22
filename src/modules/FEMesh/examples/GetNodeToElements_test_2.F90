!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the elements connected to nodes without allocation
!
! GetNodeToElements_ writes the global element numbers in ans and
! returns their number in tsize. ans must be large enough.
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
  INTEGER(I4B) :: elems(10), tsize

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  CALL obj%GetNodeToElements_(ans=elems, tsize=tsize, globalNode=7_I4B)
  CALL OK(tsize .EQ. 2_I4B, "node 7: tsize")
  CALL OK(ALL(elems(1:tsize) .EQ. [13_I4B, 14_I4B]), "node 7")

  CALL obj%GetNodeToElements_(ans=elems, tsize=tsize, &
    globalNode=[7_I4B, 8_I4B])
  CALL OK(ALL(elems(1:tsize) .EQ. [13_I4B, 14_I4B, 15_I4B, 16_I4B]), &
    "nodes 7 and 8")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
