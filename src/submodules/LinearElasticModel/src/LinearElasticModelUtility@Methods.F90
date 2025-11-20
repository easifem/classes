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

SUBMODULE(LinearElasticModelUtility) Methods
USE FEVariable_SubtractionMethod, ONLY: OPERATOR(-)
USE FEVariable_MultiplicationMethod, ONLY: OPERATOR(*)
USE FEVariable_DivisionMethod, ONLY: OPERATOR(/)
USE FEVariable_AdditionMethod, ONLY: OPERATOR(+)
USE FEVariable_UnaryMethod, ONLY: OPERATOR(**), Sqrt
USE InputUtility, ONLY: Input

IMPLICIT NONE

REAL(DFP), PARAMETER :: one = 1.0_DFP, zero = 0.0_DFP, minus_one = -1.0_DFP, &
                        half = 0.5_DFP, two = 2.0_DFP

CONTAINS

!----------------------------------------------------------------------------
!                                                               Get_3D_C_invC
!----------------------------------------------------------------------------

MODULE PROCEDURE Get_3D_C_invC
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Get_3D_C_invC()"
#endif

REAL(DFP) :: a

#ifdef DEBUG_VER
CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

C = zero
invC = zero
a = youngsModulus / (one + nu) / (one - two * nu)
C(1, 1) = (one - nu) * a
C(2, 2) = C(1, 1)
C(3, 3) = C(1, 1)
C(4, 4) = a * half * (one - two * nu)
C(5, 5) = C(4, 4)
C(6, 6) = C(4, 4)
C(1, 2) = nu * a
C(1, 3) = C(1, 2)
C(2, 1) = C(1, 2)
C(2, 3) = C(1, 2)
C(3, 1) = C(1, 2)
C(3, 2) = C(1, 2)
a = one / youngsModulus
invC(1, 1) = a
invC(2, 2) = invC(1, 1)
invC(3, 3) = invC(1, 1)
invC(4, 4) = a * two * (one + nu)
invC(5, 5) = invC(4, 4)
invC(6, 6) = invC(4, 4)
invC(1, 2) = -nu * a
invC(1, 3) = invC(1, 2)
invC(2, 1) = invC(1, 2)
invC(2, 3) = invC(1, 2)
invC(3, 1) = invC(1, 2)
invC(3, 2) = invC(1, 2)

#ifdef DEBUG_VER
CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END PROCEDURE Get_3D_C_invC

!----------------------------------------------------------------------------
!                                                     Get_PlaneStress_C_invC
!----------------------------------------------------------------------------

MODULE PROCEDURE Get_PlaneStress_C_invC
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Get_PlaneStress_C_invC()"
#endif

REAL(DFP) :: a

#ifdef DEBUG_VER
CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

C = zero
invC = zero
a = youngsModulus / (one - nu * nu)
C(1, 1) = a
C(2, 2) = C(1, 1)
C(3, 3) = a * (one - nu) * half
C(1, 2) = a * nu
C(2, 1) = C(1, 2)
a = one / youngsModulus
invC(1, 1) = a
invC(2, 2) = invC(1, 1)
invC(3, 3) = a * two * (one + nu)
invC(1, 2) = -nu * a
invC(2, 1) = invC(1, 2)

#ifdef DEBUG_VER
CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END PROCEDURE Get_PlaneStress_C_invC

!----------------------------------------------------------------------------
!                                                     Get_PlaneStrain_C_invC
!----------------------------------------------------------------------------

MODULE PROCEDURE Get_PlaneStrain_C_invC
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Get_PlaneStrain_C_invC()"
#endif

REAL(DFP) :: a

#ifdef DEBUG_VER
CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

C = zero
invC = zero
a = youngsModulus / (one - two * nu) / (one + nu)
C(1, 1) = a * (one - nu)
C(2, 2) = C(1, 1)
C(3, 3) = a * (one - two * nu) * half
C(1, 2) = a * nu
C(2, 1) = C(1, 2)
a = (one + nu) / youngsModulus
invC(1, 1) = a * (one - nu)
invC(2, 2) = invC(1, 1)
invC(3, 3) = two * a
invC(1, 2) = -nu * a
invC(2, 1) = invC(1, 2)

#ifdef DEBUG_VER
CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END PROCEDURE Get_PlaneStrain_C_invC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetC
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetC()"
#endif

LOGICAL(LGT) :: isPlaneStrain0

#ifdef DEBUG_VER
CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

isPlaneStrain0 = Input(default=.TRUE., option=isPlaneStrain)

SELECT CASE (nsd)
CASE (1)
  C(1, 1) = GetYoungsModulus(E=E, nu=nu, lambda=lambda, mu=mu, K=K)
CASE (2)
  IF (isPlaneStrain0) THEN
    CALL GetPlaneStrainC(C=C(1:3, 1:3), E=E, nu=nu, &
                         lambda=lambda, mu=mu, K=K)
  ELSE
    CALL GetPlaneStressC(C=C(1:3, 1:3), E=E, nu=nu, &
                         lambda=lambda, mu=mu, K=K)
  END IF
CASE (3)
  CALL Get3DC(C=C(1:6, 1:6), E=E, nu=nu, &
              lambda=lambda, mu=mu, K=K)
END SELECT

#ifdef DEBUG_VER
CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END PROCEDURE GetC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Get3DC
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Get3DC()"
#endif

REAL(DFP) :: E0, nu0, a

#ifdef DEBUG_VER
CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

E0 = GetYoungsModulus(E=E, nu=nu, lambda=lambda, mu=mu, K=K)
nu0 = GetPoissonRatio(E=E, nu=nu, lambda=lambda, mu=mu, K=K)

C = zero
a = E0 / (one + nu0) / (one - two * nu0)
C(1, 1) = (one - nu0) * a
C(2, 2) = C(1, 1)
C(3, 3) = C(1, 1)
C(4, 4) = a * half * (one - two * nu0)
C(5, 5) = C(4, 4)
C(6, 6) = C(4, 4)
C(1, 2) = nu0 * a
C(1, 3) = C(1, 2)
C(2, 1) = C(1, 2)
C(2, 3) = C(1, 2)
C(3, 1) = C(1, 2)
C(3, 2) = C(1, 2)

#ifdef DEBUG_VER
CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END PROCEDURE Get3DC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetPlaneStressC
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetPlaneStressC()"
#endif

REAL(DFP) :: E0, nu0, a

#ifdef DEBUG_VER
CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

E0 = GetYoungsModulus(E=E, nu=nu, lambda=lambda, mu=mu, K=K)
nu0 = GetPoissonRatio(E=E, nu=nu, lambda=lambda, mu=mu, K=K)

C = zero
a = E0 / (one - nu0 * nu0)
C(1, 1) = a
C(2, 2) = C(1, 1)
C(3, 3) = a * (one - nu0) * half
C(1, 2) = a * nu0
C(2, 1) = C(1, 2)
a = one / E0

#ifdef DEBUG_VER
CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END PROCEDURE GetPlaneStressC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetPlaneStrainC
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetPlaneStrainC()"
#endif

REAL(DFP) :: E0, nu0, a

#ifdef DEBUG_VER
CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

E0 = GetYoungsModulus(E=E, nu=nu, lambda=lambda, mu=mu, K=K)
nu0 = GetPoissonRatio(E=E, nu=nu, lambda=lambda, mu=mu, K=K)

C = zero
a = E0 / (one - two * nu0) / (one + nu0)
C(1, 1) = a * (one - nu0)
C(2, 2) = C(1, 1)
C(3, 3) = a * (one - two * nu0) * half
C(1, 2) = a * nu0
C(2, 1) = C(1, 2)

#ifdef DEBUG_VER
CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END PROCEDURE GetPlaneStrainC

!----------------------------------------------------------------------------
!                                                            GetYoungsModulus
!----------------------------------------------------------------------------

MODULE PROCEDURE GetYoungsModulus
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetYoungsModulus()"
#endif

CHARACTER(5) :: acase
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

isok = PRESENT(E)
IF (isok) THEN
  ans = E
#ifdef DEBUG_VER
  CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
  RETURN
END IF

#include "./selectCaseForGetYoungsModulus.inc"

#ifdef DEBUG_VER
CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END PROCEDURE GetYoungsModulus

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetYoungsModulusFEVar
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetYoungsModulusFEVar()"
#endif

CHARACTER(5) :: acase
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

isok = PRESENT(E)
IF (isok) THEN
  ans = E
#ifdef DEBUG_VER
  CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
  RETURN
END IF

#include "./selectCaseForGetYoungsModulus.inc"

#ifdef DEBUG_VER
CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END PROCEDURE GetYoungsModulusFEVar

!----------------------------------------------------------------------------
!                                                           GetShearModulus
!----------------------------------------------------------------------------

MODULE PROCEDURE GetShearModulus
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetShearModulus()"
#endif

LOGICAL(LGT) :: isok
REAL(DFP) :: r
CHARACTER(4) :: acase

#ifdef DEBUG_VER
CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

isok = PRESENT(G)
IF (isok) THEN
  ans = G
#ifdef DEBUG_VER
  CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
  RETURN
END IF

isok = PRESENT(mu)
IF (isok) THEN
  ans = mu
#ifdef DEBUG_VER
  CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
  RETURN
END IF

#include "./selectCaseForGetShearModulus.inc"

#ifdef DEBUG_VER
CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END PROCEDURE GetShearModulus

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetShearModulusFEVar
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetShearModulusFEVar()"
#endif

LOGICAL(LGT) :: isok
TYPE(FEVariable_) :: r
CHARACTER(4) :: acase

#ifdef DEBUG_VER
CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

isok = PRESENT(G)
IF (isok) THEN
  ans = G
#ifdef DEBUG_VER
  CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
  RETURN
END IF

isok = PRESENT(mu)
IF (isok) THEN
  ans = mu
#ifdef DEBUG_VER
  CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
  RETURN
END IF

# include "./selectCaseForGetShearModulus.inc"

#ifdef DEBUG_VER
CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END PROCEDURE GetShearModulusFEVar

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetPoissonRatio
! Internal variables
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetPoissonRatio()"
#endif
LOGICAL(LGT) :: isok
REAL(DFP) :: r, G0
CHARACTER(4) :: acase

#ifdef DEBUG_VER
CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

isok = PRESENT(nu)
IF (isok) THEN
  ans = nu
#ifdef DEBUG_VER
  CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
  RETURN
END IF

acase = 'FFFF' ! Lam, G, E, K

isok = PRESENT(lambda); IF (isok) acase(1:1) = 'T'
isok = PRESENT(G); IF (isok) acase(2:2) = 'T'; IF (isok) G0 = G
isok = PRESENT(mu); IF (isok) acase(2:2) = 'T'; IF (isok) G0 = mu
isok = PRESENT(E); IF (isok) acase(3:3) = 'T'
isok = PRESENT(K); IF (isok) acase(4:4) = 'T'

SELECT CASE (acase)
CASE ('TTFF')
  ! isLambda and isG
  ans = lambda * half / (lambda + G0)
CASE ('TFTF')
  ! isLambda and isE
  r = SQRT(E * E + 9.0_DFP * lambda * lambda + two * E * lambda)
  ans = two * lambda / (E + lambda + r)
CASE ('FTTF')
  ! isG and isE
  ans = (E - two * G0) * half / G0
CASE ('TFFT')
  ! isLambda and isK
  ans = lambda / (3.0_DFP * K - lambda)
CASE ('FTFT')
  ! isG and isK
  ans = (3.0_DFP * K - two * G0) / (6.0_DFP * K + two * G0)
CASE ('FFTT')
  ! isE and isK
  ans = (3.0_DFP * K - E) / 6.0_DFP * K
#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myName, &
                    'No case found for acase='//acase)
#endif

END SELECT

#ifdef DEBUG_VER
CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END PROCEDURE GetPoissonRatio

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetElasticParam
! Internal variables
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetElasticParam()"
#endif
LOGICAL(LGT) :: isok
REAL(DFP) :: r
CHARACTER(4) :: acase

#ifdef DEBUG_VER
CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

acase = 'FFFF' ! Lam, G, E, Nu

isok = PRESENT(lambda)
IF (isok) THEN
  lam = lambda
  acase(1:1) = 'T'
END IF

isok = PRESENT(shearModulus)
IF (isok) THEN
  G = shearModulus
  acase(2:2) = 'T'
END IF

isok = PRESENT(youngsModulus)
IF (isok) THEN
  EE = youngsModulus
  acase(3:3) = 'T'
END IF

isok = PRESENT(poissonRatio)
IF (isok) THEN
  Nu = poissonRatio
  acase(4:4) = 'T'
END IF

SELECT CASE (acase)
CASE ('FFTT')
  ! IF (isE .AND. isNu) THEN
  lam = EE * nu / (one + nu) / (one - two * nu)
  G = EE * half / (one + nu)

CASE ('FTTF')
  ! ELSE IF (isG .AND. isE) THEN
  lam = G * (EE - two * G) / (3.0 * G - EE)
  nu = (EE - two * G) * half / G

CASE ('FTFT')
  ! ELSE IF (isG .AND. isNu) THEN
  lam = two * G * nu / (one - two * nu)
  EE = two * G * (one + nu)

CASE ('TTFF')
  ! ELSE IF (isLam .AND. isG) THEN
  EE = G * (3.0 * lam + two * G) / (lam + G)
  nu = lam * half / (lam + G)

CASE ('TFTF')
  ! ELSE IF (isLam .AND. isE) THEN
  r = SQRT(EE * EE + 9.0 * lam * lam + two * EE * lam)
  G = (EE - 3.0 * lam + r) / 4.0
  nu = two * lam / (EE + lam + r)

CASE ('TFFT')
  ! ELSE IF (isLam .AND. isNu) THEN
  EE = lam * (one + nu) * (one - two * nu) / nu
  G = lam * (one - two * nu) * half / nu

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myName, &
                    'No case found for acase='//acase)
#endif

END SELECT

#ifdef DEBUG_VER
CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END PROCEDURE GetElasticParam

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

SUBROUTINE AssertError1(a, myName, msg)
  LOGICAL(LGT), INTENT(IN) :: a
  CHARACTER(*), INTENT(IN) :: myName
  CHARACTER(*), INTENT(IN) :: msg

  IF (.NOT. a) THEN
    CALL err%RaiseError(modName//'::'//myName//" - "// &
                        '[INTERNAL ERROR] :: '//msg)
    RETURN
  END IF

END SUBROUTINE AssertError1

END SUBMODULE Methods
