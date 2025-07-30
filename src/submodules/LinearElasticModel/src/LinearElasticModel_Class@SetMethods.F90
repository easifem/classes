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

SUBMODULE(LinearElasticModel_Class) SetMethods
USE Display_Method, ONLY: ToString
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  SetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetParam()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(elasticityType)) obj%elasticityType = elasticityType
IF (PRESENT(nu)) obj%nu = nu
IF (PRESENT(G)) obj%G = G
IF (PRESENT(youngsModulus)) obj%E = youngsModulus
IF (PRESENT(lambda)) obj%lambda = lambda
IF (PRESENT(C)) obj%C = C
IF (PRESENT(invC)) obj%invC = invC
IF (PRESENT(stiffnessPower)) obj%stiffnessPower = stiffnessPower

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetParam

!----------------------------------------------------------------------------
!                                                                    SetData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetData()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

SELECT CASE (obj%elasticityType)
CASE (TypeElasticityOpt%isotropic)
  CALL LinearElasticModelSetData_Iso(obj, DATA)
CASE (TypeElasticityOpt%anisotropic)
  CALL LinearElasticModelSetData_Aniso(obj, DATA)
CASE (TypeElasticityOpt%transIsotropic)
  CALL LinearElasticModelSetData_Trans(obj, DATA)
CASE (TypeElasticityOpt%orthotropic)
  CALL LinearElasticModelSetData_Ortho(obj, DATA)

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myName, &
                    'No case found for elasticityType = '// &
                    ToString(obj%elasticityType))
#endif
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetData

!----------------------------------------------------------------------------
!                                             LinearElasticModelSetData_Iso
!----------------------------------------------------------------------------

MODULE PROCEDURE LinearElasticModelSetData_Iso
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "LinearElasticModelSetData_Iso()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%lambda = DATA(1)
obj%G = DATA(2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE LinearElasticModelSetData_Iso

!----------------------------------------------------------------------------
!                                            LinearElasticModelSetData_Aniso
!----------------------------------------------------------------------------

MODULE PROCEDURE LinearElasticModelSetData_Aniso
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "LinearElasticModelSetData_Aniso()"
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
    obj%C(ii, jj) = DATA(kk)
    obj%C(jj, ii) = DATA(kk)
  END DO
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE LinearElasticModelSetData_Aniso

!----------------------------------------------------------------------------
!                                           LinearElasticModelSetData_Ortho
!----------------------------------------------------------------------------

MODULE PROCEDURE LinearElasticModelSetData_Ortho
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "LinearElasticModelSetData_Ortho()"
#endif

INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

DO ii = 1, 6
  obj%C(ii, ii) = DATA(ii)
END DO
obj%C(1, 2) = DATA(7)
obj%C(2, 1) = DATA(7)

obj%C(2, 3) = DATA(8)
obj%C(3, 2) = DATA(8)

obj%C(1, 3) = DATA(9)
obj%C(3, 1) = DATA(9)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE LinearElasticModelSetData_Ortho

!----------------------------------------------------------------------------
!                                           LinearElasticModelSetData_Trans
!----------------------------------------------------------------------------

MODULE PROCEDURE LinearElasticModelSetData_Trans
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "LinearElasticModelSetData_Trans()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%C(1, 1) = DATA(1)
obj%C(2, 2) = DATA(1)
obj%C(3, 3) = DATA(2)
obj%C(4, 4) = 0.5_DFP * (DATA(1) - DATA(4))
obj%C(5, 5) = DATA(3)
obj%C(6, 6) = DATA(3)
obj%C(1, 2) = DATA(4)
obj%C(2, 1) = DATA(4)
obj%C(1, 3) = DATA(5)
obj%C(3, 1) = DATA(5)
obj%C(2, 3) = DATA(5)
obj%C(3, 2) = DATA(5)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE LinearElasticModelSetData_Trans

!----------------------------------------------------------------------------
!                                                                 UpdateData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_UpdateData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_UpdateData()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%GetData(DATA)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_UpdateData

!----------------------------------------------------------------------------
!                                                           Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
