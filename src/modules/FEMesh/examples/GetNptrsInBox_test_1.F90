!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the nodes and elements in a box
!
! GetNptrsInBox returns the global nodes inside (or on) a bounding
! box. GetElemNumberInBox returns the elements around these nodes.
! The box is [-0.1, 0.6] x [-0.1, 0.6].
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
  USE BaseType, ONLY: BoundingBox_
  USE BaseType, ONLY: math => TypeMathOpt
  USE BoundingBox_Method, ONLY: BoundingBoxInitiate => Initiate
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEMesh_Class, ONLY: FEMesh_
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE SortUtility, ONLY: Sort
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/very_small_quad4_mesh.h5"
  TYPE(FEMesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  TYPE(BoundingBox_) :: box
  INTEGER(I4B), ALLOCATABLE :: nodes(:), elems(:)
  INTEGER(I4B) :: buf(20), buf2(20), tnodes, telems

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  CALL BoundingBoxInitiate(obj=box, nsd=3_I4B, &
    lim=[-0.1_DFP, 0.6_DFP, -0.1_DFP, &
    0.6_DFP, -0.1_DFP, 0.1_DFP])

  CALL obj%GetNptrsInBox(box=box, nptrs=nodes)
  CALL OK(ALL(Sort(nodes) .EQ. [1, 5, 7, 9]), "GetNptrsInBox")
  CALL obj%GetNptrsInBox_(box=box, nptrs=buf, tnodes=tnodes)
  CALL OK(tnodes .EQ. 4_I4B, "GetNptrsInBox_")

  CALL obj%GetElemNumberInBox(box=box, elemNumber=elems)
  CALL OK(ALL(elems .EQ. [13, 14, 15, 16]), "GetElemNumberInBox")
  CALL obj%GetElemNumberInBox_(box=box, elemNumber=buf2, &
    tElements=telems, nptrs=buf, &
    tnodes=tnodes)
  CALL OK(telems .EQ. 4_I4B .AND. tnodes .EQ. 4_I4B, &
    "GetElemNumberInBox_")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
