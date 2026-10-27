!> author: Vikas Sharma, Ph. D.
! date: 2025-06-06
! summary:  Test the intitiate method for H1, Hierarchical basis,
! for different orders.

PROGRAM main
  USE AbstractMesh_Class, ONLY: AbstractMesh_
  USE BaseType, ONLY: math => TypeMathOpt
  USE BaseType, ONLY: quadopt => TypeQuadratureOpt
  USE Display_Method, ONLY: Display
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE GlobalData, ONLY: LGT
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK
  USE Test_Method, ONLY: IS
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5"
  TYPE(FEDOF_) :: obj
  TYPE(FEDomain_) :: dom
  CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
  TYPE(HDF5File_) :: meshfile
  INTEGER(I4B) :: found, want
  LOGICAL(LGT) :: isok

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL meshfile%Initiate(filename, mode="READ")
  CALL meshfile%OPEN()
  CALL dom%Initiate(meshfile, '')
  meshptr => dom%GetMeshPointer()

  CALL test1
  CALL test2
  CALL test3
  CALL test4

CONTAINS

!---------------------------------------------------------------------------
!                                                                      test1
!---------------------------------------------------------------------------

  SUBROUTINE test1
    CALL obj%Initiate( &
                       order=1, &
                       dom=dom, &
                       baseContinuity="H1", &
                       baseInterpolation="Hierarchical", &
                       quadratureIsHomogeneous=math%yes, &
                       quadratureType=[quadopt%GaussLegendre], &
                       quadratureOrder=[2], &
                       quadratureIsOrder=math%yes)

    found = obj%GetTotalDOF()
    want = meshptr%GetTotalNodes()
    isok = found .EQ. want
    CALL OK(isok, "Total DOF matched for order = 1: ")
  END SUBROUTINE test1

!---------------------------------------------------------------------------
!                                                                      test2
!---------------------------------------------------------------------------

  SUBROUTINE test2
    CALL obj%Initiate( &
                       order=2, &
                       dom=dom, &
                       baseContinuity="H1", &
                       baseInterpolation="Hierarchical", &
                       quadratureIsHomogeneous=math%yes, &
                       quadratureType=[quadopt%GaussLegendre], &
                       quadratureOrder=[4], &
                       quadratureIsOrder=math%yes)

    found = obj%GetTotalDOF()
    want = meshptr%GetTotalNodes() + meshptr%GetTotalFaces()
    isok = found .EQ. want
    CALL OK(isok, "Total DOF matched for order = 2: ")
  END SUBROUTINE test2

!---------------------------------------------------------------------------
!                                                                      test3
!---------------------------------------------------------------------------

  SUBROUTINE test3
    CALL obj%Initiate( &
                       order=3, &
                       dom=dom, &
                       baseContinuity="H1", &
                       baseInterpolation="Hierarchical", &
                       quadratureIsHomogeneous=math%yes, &
                       quadratureType=[quadopt%GaussLegendre], &
                       quadratureOrder=[6], &
                       quadratureIsOrder=math%yes)

    found = obj%GetTotalDOF()
    want = meshptr%GetTotalNodes() &
      + 2 * meshptr%GetTotalFaces() &
      + meshptr%GetTotalCells()

    isok = found .EQ. want
    CALL OK(isok, "Total DOF matched for order = 3: ")
  END SUBROUTINE test3

!---------------------------------------------------------------------------
!                                                                      test4
!---------------------------------------------------------------------------

  SUBROUTINE test4
    CALL obj%Initiate( &
                       order=4, &
                       dom=dom, &
                       baseContinuity="H1", &
                       baseInterpolation="Hierarchical", &
                       quadratureIsHomogeneous=math%yes, &
                       quadratureType=[quadopt%GaussLegendre], &
                       quadratureOrder=[8], &
                       quadratureIsOrder=math%yes)

    found = obj%GetTotalDOF()
    want = meshptr%GetTotalNodes() &
      + 3 * meshptr%GetTotalFaces() &
      + 3 * meshptr%GetTotalCells()

    isok = found .EQ. want
    CALL OK(isok, "Total DOF matched for order = 4: ")
  END SUBROUTINE test4

END PROGRAM main
