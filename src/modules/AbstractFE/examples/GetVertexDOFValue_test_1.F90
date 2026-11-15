!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise AbstractFE_ GetVertexDOFValue operations
!
! Uses a concrete triangle finite element to exercise the public API.
!
PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
USE Test_Method, ONLY: OK
USE UserFunction_Class, ONLY: UserFunction_
USE BaseType, ONLY: TypeFEVariableOpt
IMPLICIT NONE
TYPE(TriangleH1LagrangeFE_) :: obj
TYPE(UserFunction_) :: func
REAL(DFP) :: xij(2, 3)
INTEGER(I4B) :: tsize
REAL(DFP) :: ans(3)

CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
  baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
  ipType=TypeInterpolationOpt%Equidistance, &
  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
  quadratureOrder=[4_I4B])

CALL func%Initiate(NAME="constant", returnType=TypeFEVariableOpt%scalar, &
  argType=TypeFEVariableOpt%space, numArgs=4_I4B, numReturns=1_I4B)
CALL func%SetScalarConstantVal(val=2.0_DFP)
xij(:, 1) = [0.0_DFP, 0.0_DFP]
xij(:, 2) = [1.0_DFP, 0.0_DFP]
xij(:, 3) = [0.0_DFP, 1.0_DFP]
CALL obj%GetVertexDOFValue(ans=ans, tsize=tsize, &
  func=func, xij=xij, times=0.0_DFP)
CALL OK(tsize == 3_I4B, "Three vertex coefficients")
CALL OK(ALL(ABS(ans - 2.0_DFP) < 1.0E-10_DFP), "Vertex values")
CALL func%DEALLOCATE()
CALL obj%DEALLOCATE()
END PROGRAM main
