!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise AbstractFE_ Deallocate operations
!
! Uses a concrete triangle finite element to exercise the public API.
!
PROGRAM main
USE AbstractFE_Class, ONLY: AbstractFEPointer_
USE AbstractFE_Class, ONLY: AbstractFEDeallocate
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
IMPLICIT NONE
TYPE(TriangleH1LagrangeFE_) :: obj
TYPE(AbstractFEPointer_), ALLOCATABLE :: refs(:)
TYPE(TriangleH1LagrangeFE_), TARGET :: TARGET

CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=1_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
                  quadratureOrder=[4_I4B])

CALL TARGET%Copy(obj)
ALLOCATE (refs(2))
refs(1)%ptr => TARGET
CALL AbstractFEDeallocate(refs)
CALL OK(.NOT. ALLOCATED(refs), "Wrapper vector released")
CALL OK(.NOT. TARGET%isInit, "Target state reset and still accessible")
CALL obj%DEALLOCATE()
END PROGRAM main
