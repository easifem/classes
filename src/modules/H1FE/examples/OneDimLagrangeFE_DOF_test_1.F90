!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise OneDimLagrangeFE_DOF

PROGRAM main
USE BaseType, ONLY: TypeInterpolationOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: TypeFEVariableOpt
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE OneDimLagrangeFE_Class, ONLY: FE_ => OneDimLagrangeFE_
USE Test_Method, ONLY: OK
USE UserFunction_Class, ONLY: UserFunction_
IMPLICIT NONE
TYPE(FE_) :: obj
TYPE(ElemShapeData_) :: shapes
TYPE(QuadraturePoint_) :: quad
TYPE(UserFunction_) :: func
REAL(DFP) :: ans(3), massMat(3, 3), samples(10)
INTEGER(I4B) :: ipiv(3), tsize

CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Lagrange", &
                  ipType=TypeInterpolationOpt%Equidistance, order=2_I4B, &
                  quadratureIsOrder=.TRUE., quadratureOrder=6_I4B)
CALL obj%GetQuadraturePoints(quad)
CALL obj%GetLocalElemShapeData(elemsd=shapes, quad=quad)
CALL func%Initiate(NAME="constant", returnType=TypeFEVariableOpt%scalar, &
                   argType=TypeFEVariableOpt%space, numArgs=1_I4B, &
                   numReturns=1_I4B)
CALL func%SetScalarConstantVal(val=2.0_DFP)
CALL obj%GetDOFValueFromConstant(elemsd=shapes, times=[0.0_DFP, 1.0_DFP], &
                                 ans=ans, tsize=tsize, massMat=massMat, &
                                 ipiv=ipiv, funcValue=samples, &
                                 onlyFaceBubble=.FALSE.)
CALL OK(tsize .EQ. 3_I4B .AND. ALL(ans .EQ. 1.0_DFP), "Constant spatial DOFs")
CALL obj%GetTimeDOFValue(elemsd=shapes, times=[0.0_DFP, 1.0_DFP], &
                         ans=ans, tsize=tsize, massMat=massMat, ipiv=ipiv, &
                         funcValue=samples, &
                         onlyFaceBubble=.TRUE.)
CALL OK(tsize .EQ. 1_I4B .AND. ans(1) .EQ. 1.0_DFP, "Interior time DOF")
CALL obj%GetDOFValueFromSpaceFunction(elemsd=shapes, &
                                      x=[0.0_DFP, 1.0_DFP], func=func, &
                                      ans=ans, tsize=tsize, &
                                      massMat=massMat, ipiv=ipiv, &
                                      funcValue=samples, &
                                      onlyFaceBubble=.FALSE.)
CALL OK(ALL(ABS(ans - 2.0_DFP) .LT. 1.0E-10_DFP), "Spatial samples")
CALL obj%GetDOFValueFromTimeFunction(elemsd=shapes, &
                                     times=[0.0_DFP, 1.0_DFP], func=func, &
                                     ans=ans, tsize=tsize, &
                                     massMat=massMat, ipiv=ipiv, &
                                     funcValue=samples, &
                                     onlyFaceBubble=.FALSE.)
CALL OK(ALL(ABS(ans - 2.0_DFP) .LT. 1.0E-10_DFP), "Temporal samples")
CALL func%DEALLOCATE()
CALL func%Initiate(NAME="space_time", &
                   returnType=TypeFEVariableOpt%scalar, &
                   argType=TypeFEVariableOpt%space, &
                   numArgs=4_I4B, numReturns=1_I4B)
CALL func%SetScalarConstantVal(val=3.0_DFP)
CALL obj%GetTimeDOFValue(elemsd=shapes, x=[0.0_DFP, 0.0_DFP, 0.0_DFP], &
                         nsd=3_I4B, times=[0.0_DFP, 1.0_DFP], func=func, &
                         ans=ans, tsize=tsize, &
                         massMat=massMat, ipiv=ipiv, funcValue=samples, &
                         onlyFaceBubble=.FALSE.)
CALL OK(ALL(ABS(ans - 3.0_DFP) .LT. 1.0E-10_DFP), "Space-time samples")
CALL func%DEALLOCATE()
CALL obj%DEALLOCATE()
END PROGRAM main
