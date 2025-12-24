! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

SUBMODULE(ElasticityOpt_Class) Methods
USE ExceptionHandler_Class, ONLY: e
USE StringUtility, ONLY: Uppercase
USE Display_Method, ONLY: ToString
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    ToNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ToNumber
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ToNumber()"
#endif

CHARACTER(2) :: name0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

name0 = UpperCase(name(1:2))
ans = 0

SELECT CASE (name0)

CASE ("IS")
  ans = obj%Isotropic

CASE ("AN")
  ans = obj%Anisotropic

CASE ("OR")
  ans = obj%Orthotropic

CASE ("TR")
  ans = obj%TransIsotropic

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myName, &
                    "No case found for name = "//name)
#endif
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ToNumber

!----------------------------------------------------------------------------
!                                                                   ToString
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ToString
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ToString()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

SELECT CASE (name)
CASE (TypeElasticityOpt%isotropic)
  ans = TypeElasticityOpt%isotropic_char

CASE (TypeElasticityOpt%anisotropic)
  ans = TypeElasticityOpt%anisotropic_char

CASE (TypeElasticityOpt%orthotropic)
  ans = TypeElasticityOpt%orthotropic_char

CASE (TypeElasticityOpt%transIsotropic)
  ans = TypeElasticityOpt%transIsotropic_char

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myName, &
                    "No case found for name = "//ToString(name))
#endif
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ToString

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
