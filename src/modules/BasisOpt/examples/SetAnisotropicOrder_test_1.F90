!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise SetAnisotropicOrder

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE BasisOpt_Class, ONLY: BasisOpt_
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(BasisOpt_) :: obj
INTEGER(I4B) :: orders(3)
LOGICAL(LGT) :: isAniso, isIso

CALL obj%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
  baseContinuity="H1", baseInterpolation="Lagrange", &
  order=2_I4B, &
  ipType=TypeInterpolationOpt%Equidistance, &
  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
  quadratureOrder=[6_I4B])

CALL obj%SetAnisotropicOrder(anisoOrder=[2_I4B, 3_I4B])
CALL obj%GetParam(anisoOrder=orders, isAnisotropicOrder=isAniso, &
  isIsotropicOrder=isIso)
CALL OK(isAniso .AND. .NOT. isIso, "Anisotropic mode")
CALL OK(ALL(orders(1:2) == [2_I4B, 3_I4B]), "Directional orders")
CALL OK(obj%GetTotalDOF() == 12_I4B, "Tensor-product DOF count")
CALL obj%ResetAnisotropicOrder()
CALL obj%GetParam(anisoOrder=orders, isAnisotropicOrder=isAniso)
CALL OK(.NOT. isAniso .AND. ALL(orders == 0_I4B), "Reset aniso state")
CALL obj%DEALLOCATE()
END PROGRAM main
