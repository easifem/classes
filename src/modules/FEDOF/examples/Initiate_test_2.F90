!> author: Vikas Sharma, Ph. D.
! date: 2025-06-01
! summary:  Initiate fedof for H1 and Hierarchical bases, order is a vector.

PROGRAM main
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE AbstractMesh_Class, ONLY: AbstractMesh_
  USE HDF5File_Class, ONLY: HDF5File_
  USE Display_Method, ONLY: Display
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE GlobalData, ONLY: LGT
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE ReallocateUtility, ONLY: Reallocate
  USE BaseType, ONLY: math => TypeMathOpt
  USE Test_Method, ONLY: OK
  IMPLICIT NONE

  TYPE(FEDOF_) :: fedof
  TYPE(FEDomain_) :: dom
  CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
  CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5", &
    baseInterpolation = "Hierarchical", &
    baseContinuity = "H1"
  TYPE(HDF5File_) :: meshfile
  INTEGER(I4B) :: found, want
  INTEGER(I4B), ALLOCATABLE :: cellOrder(:)
  LOGICAL(LGT) :: isok

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL meshfile%Initiate(filename, mode="READ")
  CALL meshfile%OPEN()
  CALL dom%Initiate(meshfile, '')
  meshptr => dom%GetMeshPointer()
  CALL Reallocate(cellOrder, meshptr%GetTotalCells())

  CALL test1
  CALL test2
  CALL test3
  CALL test4

  CALL dom%DEALLOCATE()
  CALL meshfile%DEALLOCATE()

CONTAINS

!---------------------------------------------------------------------------
!                                                                      test1
!---------------------------------------------------------------------------

  SUBROUTINE test1
    cellOrder = 1
    CALL fedof%Initiate(baseContinuity=baseContinuity, &
                        baseInterpolation=baseInterpolation, &
                        order=cellOrder, dom=dom, islocal=math%yes)
    found = fedof%GetTotalDOF()
    want = meshptr%GetTotalNodes()
    isok = found .EQ. want
    CALL OK(isok, "TotalDOF matched for order=1:")
  END SUBROUTINE test1

!---------------------------------------------------------------------------
!                                                                      test2
!---------------------------------------------------------------------------

  SUBROUTINE test2
    cellOrder = 2
    CALL fedof%Initiate(baseContinuity=baseContinuity, &
                        baseInterpolation=baseInterpolation, &
                        order=cellOrder, dom=dom, islocal=math%yes)

    found = fedof%GetTotalDOF()
    want = meshptr%GetTotalNodes() + meshptr%GetTotalFaces()
    isok = found .EQ. want
    CALL OK(isok, "TotalDOF matched for order=2:")
  END SUBROUTINE test2

!---------------------------------------------------------------------------
!                                                                      test3
!---------------------------------------------------------------------------

  SUBROUTINE test3
    cellOrder = 3
    CALL fedof%Initiate(baseContinuity=baseContinuity, &
                        baseInterpolation=baseInterpolation, &
                        order=cellOrder, dom=dom, islocal=math%yes)

    found = fedof%GetTotalDOF()
    want = meshptr%GetTotalNodes() &
      + 2 * meshptr%GetTotalFaces() &
      + meshptr%GetTotalCells()
    isok = found .EQ. want
    CALL OK(isok, "TotalDOF matched for order=3:")
  END SUBROUTINE test3

!---------------------------------------------------------------------------
!                                                                      test4
!---------------------------------------------------------------------------

  SUBROUTINE test4
    cellOrder = 4
    CALL fedof%Initiate(baseContinuity=baseContinuity, &
                        baseInterpolation=baseInterpolation, &
                        order=cellOrder, dom=dom, islocal=math%yes)

    found = fedof%GetTotalDOF()
    want = meshptr%GetTotalNodes() &
      + 3 * meshptr%GetTotalFaces() &
      + 3 * meshptr%GetTotalCells()
    isok = found .EQ. want
    CALL OK(isok, "TotalDOF matched for order=4:")
  END SUBROUTINE test4

!---------------------------------------------------------------------------
!
!---------------------------------------------------------------------------

END PROGRAM main
