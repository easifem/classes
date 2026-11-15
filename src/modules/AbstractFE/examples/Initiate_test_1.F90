!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise AbstractFE_ Initiate operations
!
! Uses a concrete triangle finite element to exercise the public API.
!
PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
IMPLICIT NONE
TYPE(TriangleH1LagrangeFE_) :: obj
TYPE(TriangleH1LagrangeFE_) :: other
INTEGER(I4B) :: order, nsd

CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=1_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
                  quadratureOrder=[4_I4B])

CALL OK(obj%isInit, "Initiate sets the initialization flag")
CALL obj%GetParam(order=order, nsd=nsd)
CALL OK(order .EQ. 1_I4B .AND. nsd .EQ. 2_I4B, "Read initial options")
CALL other%Copy(obj)
CALL obj%DEALLOCATE()
CALL OK(.NOT. obj%isInit, "Deallocate resets initialization")
CALL OK(other%isInit, "Copy has independent initialization state")
obj = other
CALL OK(obj%isInit, "Defined assignment copies shared state")
CALL obj%SetParam(order=2_I4B)
CALL obj%GetParam(order=order)
CALL OK(order .EQ. 2_I4B, "SetParam updates stored options")
CALL other%GetParam(order=order)
CALL OK(order .EQ. 1_I4B, "Copy retains its own options")
CALL obj%DEALLOCATE()
CALL other%DEALLOCATE()
END PROGRAM main
