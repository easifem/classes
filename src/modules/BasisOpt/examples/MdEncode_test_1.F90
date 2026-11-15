!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise MdEncode

PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE BasisOpt_Class, ONLY: BasisOpt_
USE GlobalData, ONLY: I4B
USE String_Class, ONLY: String
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(BasisOpt_) :: obj
TYPE(String) :: text

CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=2_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
                  quadratureOrder=[6_I4B])

text = obj%MdEncode()
CALL OK(LEN(text%chars()) .GT. 0, "MdEncode produces output")
CALL obj%DEALLOCATE()
END PROGRAM main
