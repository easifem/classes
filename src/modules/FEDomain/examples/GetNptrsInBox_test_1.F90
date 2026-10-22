!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the nodes in a box
!
! The box is [-0.1, 0.6] x [-0.1, 0.6].
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
  USE BaseType, ONLY: BoundingBox_
  USE BaseType, ONLY: math => TypeMathOpt
  USE BoundingBox_Method, ONLY: BoundingBoxInitiate => Initiate
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDomain_Class, ONLY: FEDomain_
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE SortUtility, ONLY: Sort
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
  TYPE(HDF5File_) :: meshfile
  TYPE(FEDomain_) :: obj
  TYPE(BoundingBox_) :: box
  INTEGER(I4B), ALLOCATABLE :: nodes(:)
  INTEGER(I4B) :: buf(20), tnodes

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, group="")

  CALL BoundingBoxInitiate(obj=box, nsd=3_I4B, &
    lim=[-0.1_DFP, 0.6_DFP, -0.1_DFP, &
    0.6_DFP, -0.1_DFP, 0.1_DFP])
  CALL obj%GetNptrsInBox(box=box, nptrs=nodes)
  CALL OK(ALL(Sort(nodes) .EQ. [1, 5, 7, 9]), "GetNptrsInBox")
  CALL obj%GetNptrsInBox_(box=box, nptrs=buf, tnodes=tnodes)
  CALL OK(tnodes .EQ. 4_I4B, "GetNptrsInBox_")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
