!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the connectivity of an element
!
! GetConnectivity returns the vertex connectivity of an element of
! the mesh of dimension dim (default nsd). GetNNE returns its number
! of nodes, and GetConnectivity_ works without allocation.
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
    "./meshdata/very_small_quad4_mesh.h5"
  TYPE(HDF5File_) :: meshfile
  TYPE(FEDomain_) :: obj
  INTEGER(I4B), ALLOCATABLE :: con(:)
  INTEGER(I4B) :: buf(20), tsize

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, group="")

  con = obj%GetConnectivity(globalElement=14_I4B)
  CALL OK(ALL(con .EQ. [7, 9, 6, 3]), "element 14")
  con = obj%GetConnectivity(globalElement=6_I4B, DIM=1_I4B)
  CALL OK(ALL(con .EQ. [5, 2]), "line element 6")
  CALL OK(obj%GetNNE(13_I4B) .EQ. 4_I4B, "GetNNE")
  CALL OK(obj%GetNNE(6_I4B, DIM=1_I4B) .EQ. 2_I4B, "GetNNE dim=1")
  CALL obj%GetConnectivity_(globalElement=13_I4B, ans=buf, &
                            tsize=tsize, opt="V")
  CALL OK(ALL(buf(1:tsize) .EQ. [1, 5, 9, 7]), "GetConnectivity_")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
