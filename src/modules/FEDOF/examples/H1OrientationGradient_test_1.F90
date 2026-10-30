!> author: Vikas Sharma, Ph. D.
! date: 2026-10-30
! summary: Check a dependency of the H1 orientation path

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
USE TriangleInterpolationUtility, ONLY: HeirarchicalBasis_Triangle_
USE TriangleInterpolationUtility, ONLY: HeirarchicalBasisGradient_Triangle_
IMPLICIT NONE
REAL(DFP), PARAMETER :: h = 1.0E-6_DFP
REAL(DFP) :: x(2, 3), xp(2, 3), xm(2, 3)
REAL(DFP) :: plus(3, 15), minus(3, 15), gradient(3, 15, 2)
INTEGER(I4B) :: p, mask, o(3), edge, dim, nr, nc, nd, last
x(:, 1) = [-0.7_DFP, -0.8_DFP]
x(:, 2) = [-0.4_DFP, -0.7_DFP]
x(:, 3) = [-0.6_DFP, -0.3_DFP]
DO p = 1, 4
  last = 3 * p
  DO mask = 0, 7
    DO edge = 1, 3
      o(edge) = 1
      IF (BTEST(mask, edge - 1)) o(edge) = -1
    END DO
    CALL HeirarchicalBasisGradient_Triangle_(order=p, &
                                             pe1=p, pe2=p, pe3=p, xij=x, &
                                             refTriangle="BIUNIT", &
                                             edgeOrient1=o(1), &
                                             edgeOrient2=o(2), &
                                             edgeOrient3=o(3), &
                                             faceOrient=[0_I4B, 1_I4B], &
                                             ans=gradient, &
                                             tsize1=nr, tsize2=nc, tsize3=nd)
    DO dim = 1, 2
      xp = x
      xm = x
      xp(dim, :) = xp(dim, :) + h
      xm(dim, :) = xm(dim, :) - h
      CALL HeirarchicalBasis_Triangle_(order=p, pe1=p, pe2=p, pe3=p, &
                                       xij=xp, refTriangle="BIUNIT", &
                                       ans=plus, nrow=nr, ncol=nc, &
                                       edgeOrient1=o(1), edgeOrient2=o(2), &
                                       edgeOrient3=o(3), &
                                       faceOrient=[0_I4B, 1_I4B])
      CALL HeirarchicalBasis_Triangle_(order=p, pe1=p, pe2=p, pe3=p, &
                                       xij=xm, refTriangle="BIUNIT", &
                                       ans=minus, nrow=nr, ncol=nc, &
                                       edgeOrient1=o(1), edgeOrient2=o(2), &
                                       edgeOrient3=o(3), &
                                       faceOrient=[0_I4B, 1_I4B])
      CALL OK(MAXVAL(ABS(gradient(:, 1:last, dim) - &
                         (plus(:, 1:last) - minus(:, 1:last)) / (2 * h))) &
              .LT. 1.0E-7_DFP, &
              "Oriented vertex/edge gradients match differences")
    END DO
  END DO
END DO
END PROGRAM main
