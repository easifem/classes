!> author: Vikas Sharma, Ph. D.
! date: 2026-10-30
! summary: Check a dependency of the H1 orientation path

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalEvalAll
USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalEvalAll_
IMPLICIT NONE
REAL(DFP), PARAMETER :: x(5) = &
                        [-0.9_DFP, -0.3_DFP, 0.0_DFP, 0.4_DFP, 0.8_DFP]
REAL(DFP) :: values(5, 5), reversed(5, 5), scalar(5)
INTEGER(I4B) :: n, ii, nr, nc
DO n = 0, 4
  CALL UltrasphericalEvalAll_(n=n, lambda=1.5_DFP, x=x, &
                              ans=values, nrow=nr, ncol=nc)
  CALL UltrasphericalEvalAll_(n=n, lambda=1.5_DFP, x=x(5:1:-1), &
                              ans=reversed, nrow=nr, ncol=nc)
  DO ii = 1, SIZE(x)
    scalar(1:n + 1) = UltrasphericalEvalAll( &
                      n=n, lambda=1.5_DFP, x=x(ii))
    CALL OK(MAXVAL(ABS(values(ii, 1:n + 1) - scalar(1:n + 1))) &
            .LT. 1.0E-12_DFP, "Vector and scalar recurrence agree")
  END DO
  CALL OK(MAXVAL(ABS(values(:, 1:n + 1) - reversed(5:1:-1, 1:n + 1))) &
          .LT. 1.0E-12_DFP, "Sample order does not affect values")
END DO
END PROGRAM main
