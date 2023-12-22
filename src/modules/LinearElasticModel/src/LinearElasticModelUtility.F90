! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

MODULE LinearElasticModelUtility
USE GlobalData, ONLY: DFP, LGT
IMPLICIT NONE
PRIVATE
PUBLIC :: GetYoungsModulus
PUBLIC :: GetShearModulus

CONTAINS

FUNCTION GetYoungsModulus(E, G, lambda, mu, nu, K) RESULT(ans)
  REAL(DFP), OPTIONAL, INTENT(IN) :: E
  REAL(DFP), OPTIONAL, INTENT(IN) :: G
  REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
  REAL(DFP), OPTIONAL, INTENT(IN) :: mu
  REAL(DFP), OPTIONAL, INTENT(IN) :: nu
  REAL(DFP), OPTIONAL, INTENT(IN) :: K
  REAL(DFP) :: ans

  LOGICAL(LGT) :: isK, isE, isLambda, isG, isMu

  isE = PRESENT(E)
  IF (isE) THEN
    ans = E
    RETURN
  END IF

  isK = PRESENT(K)
  isLambda = PRESENT(lambda)
  isG = PRESENT(G)
  isMu = PRESENT(mu)

  IF (isK) THEN

    IF (isLambda) THEN
      ans = 9.0_DFP * K * (K - lambda) / (3.0_DFP * K - lambda)
      RETURN
    END IF

    IF (isG) THEN
      ans = 9.0_DFP * K * G / (3.0_DFP * K + G)
      RETURN
    END IF

    IF (isMu) THEN
      ans = 9.0_DFP * K * mu / (3.0_DFP * K + mu)
      RETURN
    END IF

    IF (PRESENT(nu)) THEN
      ans = 3.0_DFP * K * (1.0_DFP - 2.0_DFP * nu)
      RETURN
    END IF

  END IF

  IF (isLambda) THEN

    IF (isG) THEN
      ans = G * (3 * lambda + 2 * G) / (lambda + G)
      RETURN
    END IF

    IF (ismu) THEN
      ans = mu * (3 * lambda + 2 * mu) / (lambda + mu)
      RETURN
    END IF

    IF (PRESENT(nu)) THEN
      ans = lambda * (1 + nu) * (1 - 2 * nu) / nu
      RETURN
    END IF
  END IF

  IF (isG) THEN
    IF (PRESENT(nu)) THEN
      ans = 2 * G * (1 + nu)
      RETURN
    END IF
  END IF

  IF (ismu) THEN
    IF (PRESENT(nu)) THEN
      ans = 2 * mu * (1 + nu)
      RETURN
    END IF
  END IF
END FUNCTION GetYoungsModulus

!----------------------------------------------------------------------------
!                                                           GetShearModulus
!----------------------------------------------------------------------------

FUNCTION GetShearModulus(E, G, lambda, mu, nu, K) RESULT(ans)
  REAL(DFP), OPTIONAL, INTENT(IN) :: E
  REAL(DFP), OPTIONAL, INTENT(IN) :: G
  REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
  REAL(DFP), OPTIONAL, INTENT(IN) :: mu
  REAL(DFP), OPTIONAL, INTENT(IN) :: nu
  REAL(DFP), OPTIONAL, INTENT(IN) :: K
  REAL(DFP) :: ans

  LOGICAL(LGT) :: isK, isE, isLambda, isG, isMu
  REAL(DFP) :: r

  isG = PRESENT(G)
  IF (isG) THEN
    ans = G
    RETURN
  END IF

  isMu = PRESENT(mu)
  IF (isMu) THEN
    ans = mu
    RETURN
  END IF

  isE = PRESENT(E)
  isK = PRESENT(K)
  isLambda = PRESENT(lambda)

  IF (isK) THEN

    IF (isE) THEN
      ans = 3 * K * E / (9 * K - E)
      RETURN
    END IF

    IF (isLambda) THEN
      ans = 1.5 * (K - lambda)
      RETURN
    END IF

    IF (PRESENT(nu)) THEN
      ans = 1.5 * K * (1 - 2 * nu) / (1 + nu)
      RETURN
    END IF

  END IF

  IF (isE) THEN
    IF (isLambda) THEN
      r = E**2 + 9 * lambda**2 + 2 * E * lambda
      r = SQRT(r)
      ans = 0.25 * (E - 3 * lambda + r)
      RETURN
    END IF

    IF (PRESENT(nu)) THEN
      ans = 0.5 * E / (1 + nu)
      RETURN
    END IF
  END IF

  IF (isLambda) THEN
    IF (PRESENT(nu)) THEN
      ans = lambda * (1 - 2 * nu) / 2.0 / nu
      RETURN
    END IF
  END IF

END FUNCTION GetShearModulus

END MODULE LinearElasticModelUtility
