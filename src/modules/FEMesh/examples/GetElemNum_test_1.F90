!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the element numbers of a mesh
!
! GetElemNum returns the global (or local) element numbers of the
! mesh or of a mesh id. GetElemNum_ does the same without
! allocation.
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
  INTEGER(I4B), ALLOCATABLE :: elems(:)
  INTEGER(I4B) :: buf(10), tsize

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  elems = obj%GetElemNum()
  CALL OK(ALL(elems .EQ. [13, 14, 15, 16]), "global")
  elems = obj%GetElemNum(islocal=.TRUE.)
  CALL OK(ALL(elems .EQ. [1, 2, 3, 4]), "local")
  elems = obj%GetElemNum(meshid=1_I4B)
  CALL OK(ALL(elems .EQ. [13, 14, 15, 16]), "meshid")

  CALL obj%GetElemNum_(islocal=.FALSE., ans=buf, tsize=tsize)
  CALL OK(ALL(buf(1:tsize) .EQ. [13, 14, 15, 16]), "GetElemNum_")
  CALL obj%GetElemNum_(meshid=1_I4B, islocal=.TRUE., ans=buf, &
    tsize=tsize)
  CALL OK(ALL(buf(1:tsize) .EQ. [1, 2, 3, 4]), "GetElemNum_ meshid")
  CALL obj%GetElemNum_(meshid=[1_I4B], islocal=.FALSE., ans=buf, &
    tsize=tsize)
  CALL OK(tsize .EQ. 4_I4B, "GetElemNum_ meshid(:)")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
