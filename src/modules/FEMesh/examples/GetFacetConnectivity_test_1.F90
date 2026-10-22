!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the connectivity of a facet of an element
!
! GetFacetConnectivity returns the global node numbers of local
! facet iface of an element. GetFacetConnectivity_ does the same
! without allocation, and GetTotalFacetConnectivity returns the
! number of nodes of the facet.
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
  INTEGER(I4B), ALLOCATABLE :: con(:)
  INTEGER(I4B) :: buf(10), tsize

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  con = obj%GetFacetConnectivity(globalElement=13_I4B, iface=1_I4B)
  CALL OK(ALL(con .EQ. [1, 5]), "facet 1 of 13")
  con = obj%GetFacetConnectivity(globalElement=13_I4B, iface=2_I4B)
  CALL OK(ALL(con .EQ. [5, 9]), "facet 2 of 13")
  CALL obj%GetFacetConnectivity_(globalElement=14_I4B, iface=2_I4B, &
    ans=buf, tsize=tsize)
  CALL OK(ALL(buf(1:tsize) .EQ. [9, 6]), "GetFacetConnectivity_")
  CALL OK(obj%GetTotalFacetConnectivity(globalElement=13_I4B, &
    iface=1_I4B) .EQ. 2_I4B, &
    "GetTotalFacetConnectivity")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
