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
IMPLICIT NONE
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

C = 0.0_DFP
invC = 0.0_DFP
a = youngsModulus / (1.0 + nu) / (1.0 - 2.0 * nu)
C(1, 1) = (1.0 - nu) * a
C(2, 2) = C(1, 1)
C(3, 3) = C(1, 1)
C(4, 4) = a * 0.5 * (1.0 - 2.0 * nu)
C(5, 5) = C(4, 4)
C(6, 6) = C(4, 4)
C(1, 2) = nu * a
C(1, 3) = C(1, 2)
C(2, 1) = C(1, 2)
C(2, 3) = C(1, 2)
C(3, 1) = C(1, 2)
C(3, 2) = C(1, 2)
a = 1.0_DFP / youngsModulus
invC(1, 1) = a
invC(2, 2) = invC(1, 1)
invC(3, 3) = invC(1, 1)
invC(4, 4) = a * 2.0 * (1.0 + nu)
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

C = 0.0_DFP
invC = 0.0_DFP
a = youngsModulus / (1.0 - nu * nu)
C(1, 1) = a
C(2, 2) = C(1, 1)
C(3, 3) = a * (1.0 - nu) * 0.5_DFP
C(1, 2) = a * nu
C(2, 1) = C(1, 2)
a = 1.0_DFP / youngsModulus
invC(1, 1) = a
invC(2, 2) = invC(1, 1)
invC(3, 3) = a * 2.0_DFP * (1.0 + nu)
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

C = 0.0_DFP
invC = 0.0_DFP
a = youngsModulus / (1.0 - 2.0 * nu) / (1.0 + nu)
C(1, 1) = a * (1.0 - nu)
C(2, 2) = C(1, 1)
C(3, 3) = a * (1.0 - 2.0 * nu) * 0.5_DFP
C(1, 2) = a * nu
C(2, 1) = C(1, 2)
a = (1.0 + nu) / youngsModulus
invC(1, 1) = a * (1.0 - nu)
invC(2, 2) = invC(1, 1)
invC(3, 3) = 2.0_DFP * a
invC(1, 2) = -nu * a
invC(2, 1) = invC(1, 2)

#ifdef DEBUG_VER
CALL err%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END PROCEDURE Get_PlaneStrain_C_invC

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

acase = 'FFFFF' ! K, Lambda, G, Mu, Nu
isok = PRESENT(K); IF (isok) acase(1:1) = 'T'
isok = PRESENT(lambda); IF (isok) acase(2:2) = 'T'
isok = PRESENT(G); IF (isok) acase(3:3) = 'T'
isok = PRESENT(mu); IF (isok) acase(4:4) = 'T'
isok = PRESENT(nu); IF (isok) acase(5:5) = 'T'

SELECT CASE (acase)
CASE ('TTFFF')
  ! isK isLambda
  ans = 9.0_DFP * K * (K - lambda) / (3.0_DFP * K - lambda)

CASE ('TFTFF')
  ! isK isG
  ans = 9.0_DFP * K * G / (3.0_DFP * K + G)

CASE ('TFFTF')
  ! isK isMu
  ans = 9.0_DFP * K * mu / (3.0_DFP * K + mu)

CASE ('TFFFT')
  ! isK isNu
  ans = 3.0_DFP * K * (1.0_DFP - 2.0_DFP * nu)

CASE ('FTTFF')
  ! isLambda isG
  ans = G * (3 * lambda + 2 * G) / (lambda + G)

CASE ('FTFTF')
  ! isLambda isMu
  ans = mu * (3 * lambda + 2 * mu) / (lambda + mu)

CASE ('FTFFT')
  ! isLambda isNu
  ans = lambda * (1 + nu) * (1 - 2 * nu) / nu

CASE ('FFTFT')
  ! isG isNu
  ans = 2 * G * (1 + nu)

CASE ('FFFTT')
  ! isMu isNu
  ans = 2 * mu * (1 + nu)

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
END PROCEDURE GetYoungsModulus

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

acase = 'FFFF' ! K, E, Lambda, Nu

isok = PRESENT(K); IF (isok) acase(1:1) = 'T'
isok = PRESENT(E); IF (isok) acase(2:2) = 'T'
isok = PRESENT(lambda); IF (isok) acase(3:3) = 'T'
isok = PRESENT(nu); IF (isok) acase(4:4) = 'T'

SELECT CASE (acase)
CASE ('TTFF')
  ! isK isE
  ans = 3 * K * E / (9 * K - E)

CASE ('TFTF')
  ! isK isLambda
  ans = 1.5 * (K - lambda)

CASE ('TFFT')
  ! isK isNu
  ans = 1.5 * K * (1 - 2 * nu) / (1 + nu)

CASE ('FTTF')
  ! isE isLambda
  r = E**2 + 9 * lambda**2 + 2 * E * lambda
  r = SQRT(r)
  ans = 0.25 * (E - 3 * lambda + r)

CASE ('FTFT')
  ! isE isNu
  ans = 0.5 * E / (1 + nu)

CASE ('FFTT')
  ! isLambda isNu
  ans = lambda * (1 - 2 * nu) / 2.0 / nu

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
END PROCEDURE GetShearModulus

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
  lam = EE * nu / (1.0 + nu) / (1.0 - 2.0 * nu)
  G = EE * 0.5_DFP / (1.0 + nu)

CASE ('FTTF')
  ! ELSE IF (isG .AND. isE) THEN
  lam = G * (EE - 2.0 * G) / (3.0 * G - EE)
  nu = (EE - 2.0 * G) * 0.5_DFP / G

CASE ('FTFT')
  ! ELSE IF (isG .AND. isNu) THEN
  lam = 2.0 * G * nu / (1.0 - 2.0 * nu)
  EE = 2.0 * G * (1.0 + nu)

CASE ('TTFF')
  ! ELSE IF (isLam .AND. isG) THEN
  EE = G * (3.0 * lam + 2.0 * G) / (lam + G)
  nu = lam * 0.5 / (lam + G)

CASE ('TFTF')
  ! ELSE IF (isLam .AND. isE) THEN
  r = SQRT(EE * EE + 9.0 * lam * lam + 2.0 * EE * lam)
  G = (EE - 3.0 * lam + r) / 4.0
  nu = 2.0 * lam / (EE + lam + r)

CASE ('TFFT')
  ! ELSE IF (isLam .AND. isNu) THEN
  EE = lam * (1.0 + nu) * (1.0 - 2.0 * nu) / nu
  G = lam * (1.0 - 2.0 * nu) * 0.5_DFP / nu

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
