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
USE BaseType, ONLY: TypeMathOpt
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  SetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetParam()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(elasticityType)
IF (isok) obj%elasticityType = elasticityType

isok = PRESENT(nu)
IF (isok) obj%nu = nu

isok = PRESENT(G)
IF (isok) obj%G = G

isok = PRESENT(youngsModulus)
IF (isok) obj%E = youngsModulus

isok = PRESENT(lambda)
IF (isok) obj%lambda = lambda

isok = PRESENT(nc)
IF (isok) obj%nc = nc

isok = PRESENT(C)
IF (isok) obj%C(1:obj%nc, 1:obj%nc) = C(1:obj%nc, 1:obj%nc)

isok = PRESENT(invC)
IF (isok) obj%invC(1:obj%nc, 1:obj%nc) = invC(1:obj%nc, 1:obj%nc)

isok = PRESENT(stiffnessPower)
IF (isok) obj%stiffnessPower = stiffnessPower

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
  CALL AssertError1(TypeMathOpt%no, myName, &
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

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Set data for Isotropic linear elasticity

SUBROUTINE LinearElasticModelSetData_Aniso(obj, DATA)
  CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
  REAL(DFP), INTENT(IN) :: DATA(:)

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
END SUBROUTINE LinearElasticModelSetData_Aniso

!----------------------------------------------------------------------------
!                                           LinearElasticModelSetData_Ortho
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Set data for Ortho linear elasticity

SUBROUTINE LinearElasticModelSetData_Ortho(obj, DATA)
  CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
  REAL(DFP), INTENT(IN) :: DATA(:)

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
END SUBROUTINE LinearElasticModelSetData_Ortho

!----------------------------------------------------------------------------
!                                           LinearElasticModelSetData_Trans
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Set data for Transverse Isotropic linear elasticity

SUBROUTINE LinearElasticModelSetData_Trans(obj, DATA)
  CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
  REAL(DFP), INTENT(IN) :: DATA(:)

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
END SUBROUTINE LinearElasticModelSetData_Trans

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

CALL obj%GetData(DATA=DATA, tsize=tsize)

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
