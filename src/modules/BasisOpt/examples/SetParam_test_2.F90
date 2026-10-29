!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise SetParam

PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE BasisOpt_Class, ONLY: BasisOpt_
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(BasisOpt_) :: obj
INTEGER(I4B) :: faceOrder(3, 3), got(3, 3)
INTEGER(I4B) :: nrow, ncol

CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=2_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
                  quadratureOrder=[6_I4B])

faceOrder = 2_I4B
CALL obj%SetParam(faceOrder=faceOrder, tFaceOrder=3_I4B)
CALL obj%GetFaceOrder(ans=got, nrow=nrow, ncol=ncol)
CALL OK(nrow .EQ. 3_I4B .AND. ncol .EQ. 3_I4B, "Face-order shape")
CALL OK(ALL(got .EQ. faceOrder), "SetParam copies face-order columns")
CALL obj%DEALLOCATE()
END PROGRAM main
