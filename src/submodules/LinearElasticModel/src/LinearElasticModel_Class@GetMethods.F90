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

SUBMODULE(LinearElasticModel_Class) GetMethods
USE Display_Method, ONLY: ToString
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            GetElasticParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElasticParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetElasticParam()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%GetParam(nu=poissonRatio, G=shearModulus, &
                  youngsModulus=youngsModulus, lambda=lambda, &
                  C=C, invC=invC, &
                  stiffnessPower=stiffnessPower)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetElasticParam

!----------------------------------------------------------------------------
!                                                               GetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetParam()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(elasticityType)) elasticityType = obj%elasticityType
IF (PRESENT(nu)) nu = obj%nu
IF (PRESENT(G)) G = obj%G
IF (PRESENT(youngsModulus)) youngsModulus = obj%E
IF (PRESENT(lambda)) lambda = obj%lambda
IF (PRESENT(C)) THEN
  C(1:obj%nc, 1:obj%nc) = obj%C(1:obj%nc, 1:obj%nc)
END IF

IF (PRESENT(invC)) THEN
  invC(1:obj%nc, 1:obj%nc) = obj%invC(1:obj%nc, 1:obj%nc)
END IF

IF (PRESENT(stiffnessPower)) stiffnessPower = obj%stiffnessPower

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetParam

!----------------------------------------------------------------------------
!                                                                       GetC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetC
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetC()"
#endif
INTEGER(I4B) :: n

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

n = obj%nc
C(1:n, 1:n) = obj%C(1:n, 1:n)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetC

!----------------------------------------------------------------------------
!                                                                    GetinvC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetinvC
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetinvC()"
#endif
INTEGER(I4B) :: n

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

n = obj%nc
invC(1:n, 1:n) = obj%invC(1:n, 1:n)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetinvC

!----------------------------------------------------------------------------
!                                                         GetElasticityType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElasticityType
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetElasticityType()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%elasticityType

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetElasticityType

!----------------------------------------------------------------------------
!                                                                GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
ans = myPrefix
END PROCEDURE obj_GetPrefix

!----------------------------------------------------------------------------
!                                                               GetDataSize
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDataSize
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetDataSize()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = 0
SELECT CASE (obj%elasticityType)
CASE (TypeElasticityOpt%isotropic)
  ans = 2
CASE (TypeElasticityOpt%anisotropic)
  ans = 21
CASE (TypeElasticityOpt%transIsotropic)
  ans = 5
CASE (TypeElasticityOpt%orthotropic)
  ans = 9

#ifdef DEBUG_VER
CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
          'No case found for elasticityType = '//ToString(obj%elasticityType))
#endif

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetDataSize

!----------------------------------------------------------------------------
!                                                                    GetData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetData()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

SELECT CASE (obj%elasticityType)
CASE (TypeElasticityOpt%isotropic)
  CALL LinearElasticModelGetData_Iso(obj, DATA)
CASE (TypeElasticityOpt%anisotropic)
  CALL LinearElasticModelGetData_Aniso(obj, DATA)
CASE (TypeElasticityOpt%transIsotropic)
  CALL LinearElasticModelGetData_Trans(obj, DATA)
CASE (TypeElasticityOpt%orthotropic)
  CALL LinearElasticModelGetData_Ortho(obj, DATA)

#ifdef DEBUG_VER
CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
          'No case found for elasticityType = '//ToString(obj%elasticityType))
#endif
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetData

!----------------------------------------------------------------------------
!                                             LinearElasticModelGetData_Iso
!----------------------------------------------------------------------------

MODULE PROCEDURE LinearElasticModelGetData_Iso
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "LinearElasticModelGetData_Iso()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

DATA(1) = obj%lambda
DATA(2) = obj%G

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE LinearElasticModelGetData_Iso

!----------------------------------------------------------------------------
!                                            LinearElasticModelGetData_Aniso
!----------------------------------------------------------------------------

MODULE PROCEDURE LinearElasticModelGetData_Aniso
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "LinearElasticModelGetData_Aniso()"
#endif
INTEGER(I4B) :: ii, jj, kk

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

kk = 0
DO jj = 1, 6
  DO ii = jj, 6
    kk = kk + 1
    DATA(kk) = obj%C(ii, jj)
  END DO
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE LinearElasticModelGetData_Aniso

!----------------------------------------------------------------------------
!                                           LinearElasticModelGetData_Ortho
!----------------------------------------------------------------------------

MODULE PROCEDURE LinearElasticModelGetData_Ortho
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "LinearElasticModelGetData_Ortho()"
#endif
INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

DO ii = 1, 6
  DATA(ii) = obj%C(ii, ii)
END DO
DATA(7) = obj%C(1, 2)
DATA(8) = obj%C(2, 3)
DATA(9) = obj%C(1, 3)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE LinearElasticModelGetData_Ortho

!----------------------------------------------------------------------------
!                                           LinearElasticModelGetData_Trans
!----------------------------------------------------------------------------

MODULE PROCEDURE LinearElasticModelGetData_Trans
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "LinearElasticModelGetData_Trans()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

DATA(1) = obj%C(1, 1)
DATA(2) = obj%C(3, 3)
DATA(3) = obj%C(5, 5)
DATA(4) = obj%C(1, 2)
DATA(5) = obj%C(1, 3)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE LinearElasticModelGetData_Trans

END SUBMODULE GetMethods
