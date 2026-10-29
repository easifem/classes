!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise SetOrder

PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE BasisOpt_Class, ONLY: BasisOpt_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(BasisOpt_) :: obj
INTEGER(I4B) :: order, aniso(3)
LOGICAL(LGT) :: isotropic, anisotropic

CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=2_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
                  quadratureOrder=[6_I4B])

CALL obj%SetIsotropicOrder(order=3_I4B)
CALL OK(obj%GetTotalDOF() .EQ. 10_I4B, "Isotropic cubic DOF count")
CALL obj%ResetIsotropicOrder()
CALL obj%GetParam(order=order, isIsotropicOrder=isotropic)
CALL OK(order .EQ. 0_I4B .AND. .NOT. isotropic, "Reset isotropic state")
CALL obj%SetOrder(order=1_I4B)
CALL OK(obj%GetTotalDOF() .EQ. 3_I4B, "SetOrder recomputes DOFs")
CALL obj%SetTotalDOF(tdof=7_I4B)
CALL OK(obj%GetTotalDOF() .EQ. 7_I4B, "Explicit DOF override")
CALL obj%DEALLOCATE()
END PROGRAM main
