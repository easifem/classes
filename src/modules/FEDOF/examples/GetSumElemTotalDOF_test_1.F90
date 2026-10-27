!> author: Vikas Sharma, Ph. D.
! date: 2026-10-27
! summary: Sum of the element DOFs for H1 Hierarchical on triangles
!
! For a triangle of order p the element has
! 3 + 3 (p - 1) + (p - 1)(p - 2) / 2 degrees of freedom, so the sum
! over all elements of small_tri3_mesh.h5 is 3, 6 and 10 times the
! number of elements for p = 1, 2 and 3. The sum is also compared
! with the element-wise GetTotalDOF. Initiate is called again for
! each order, which checks that the cached value is reset.

PROGRAM main
  USE AbstractMesh_Class, ONLY: AbstractMesh_
  USE BaseType, ONLY: math => TypeMathOpt
  USE BaseType, ONLY: quadopt => TypeQuadratureOpt
  USE Display_Method, ONLY: ToString
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5"
  TYPE(FEDOF_) :: obj
  TYPE(FEDomain_) :: dom
  CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
  TYPE(HDF5File_) :: meshfile
  INTEGER(I4B) :: telems

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL meshfile%Initiate(filename, mode="READ")
  CALL meshfile%OPEN()
  CALL dom%Initiate(meshfile, '')
  meshptr => dom%GetMeshPointer()
  telems = meshptr%GetTotalElements()

  CALL test(order=1, nloc=3)
  CALL test(order=2, nloc=6)
  CALL test(order=3, nloc=10)

  CALL obj%DEALLOCATE()
  CALL dom%DEALLOCATE()
  CALL meshfile%DEALLOCATE()

CONTAINS

!---------------------------------------------------------------------------
!                                                                       test
!---------------------------------------------------------------------------

  SUBROUTINE test(order, nloc)
    INTEGER(I4B), INTENT(IN) :: order, nloc
    INTEGER(I4B) :: found, want, iel
    CHARACTER(:), ALLOCATABLE :: msg

    msg = "order = " // ToString(order) // ": "

    CALL obj%Initiate(order=order, dom=dom, baseContinuity="H1", &
                      baseInterpolation="Hierarchical", &
                      quadratureIsHomogeneous=math%yes, &
                      quadratureType=[quadopt%GaussLegendre], &
                      quadratureOrder=[2 * order], &
                      quadratureIsOrder=math%yes)

    found = obj%GetSumElemTotalDOF()
    CALL OK(found .EQ. nloc * telems, msg // "nloc * telems")

    want = 0
    DO iel = 1, telems
      want = want + obj%GetTotalDOF(globalElement=iel, islocal=math%yes)
    END DO
    CALL OK(found .EQ. want, msg // "sum of GetTotalDOF")

    found = obj%GetSumElemTotalDOF()
    CALL OK(found .EQ. want, msg // "cached value")

  END SUBROUTINE test

END PROGRAM main
