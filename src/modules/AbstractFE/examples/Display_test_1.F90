!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise AbstractFE_ Display operations
!
! Uses a concrete triangle finite element to exercise the public API.
!
PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE GlobalData, ONLY: I4B
USE String_Class, ONLY: String
USE Test_Method, ONLY: OK
USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
IMPLICIT NONE
TYPE(TriangleH1LagrangeFE_) :: obj
TYPE(String) :: markdown, react
INTEGER, PARAMETER :: unitno = 20
INTEGER :: ios
CHARACTER(256) :: line, iomsg

CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=1_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
                  quadratureOrder=[4_I4B])

OPEN (UNIT=unitno, STATUS="scratch", ACTION="readwrite")
CALL obj%Display(msg="Finite element", unitno=unitno)
REWIND (unitno)
READ (unitno, '(A)', IOSTAT=ios, IOMSG=iomsg) line
CALL OK(ios .EQ. 0, "Display writes output")
CLOSE (unitno)
markdown = obj%MdEncode()
react = obj%ReactEncode()
CALL OK(LEN(markdown%chars()) .GT. 0, "Markdown encoding")
CALL OK(LEN(react%chars()) .GT. 0, "React encoding")
CALL obj%DEALLOCATE()
END PROGRAM main
