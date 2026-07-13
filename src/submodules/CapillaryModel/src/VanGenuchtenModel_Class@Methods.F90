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

SUBMODULE(VanGenuchtenModel_Class) Methods
USE TomlUtility, ONLY: GetValue
USE tomlf, ONLY: toml_get => get_value
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "VanGenuchten_Class@Methods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                              GetSaturation
!----------------------------------------------------------------------------

PURE FUNCTION SaturationModel1(ng, mg, pg, smin, smax, suction) RESULT(ans)
  REAL(DFP), INTENT(IN) :: ng, mg, pg, smin, smax, suction
  REAL(DFP) :: ans
  ans = (smax - smin) / (math%one + (suction / pg)**ng)**mg + smin
END FUNCTION SaturationModel1

!----------------------------------------------------------------------------
!                                                              GetSaturation
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSaturation1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetSaturation1()"
#endif

REAL(DFP) :: areal

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (isSuction) THEN
  areal = suction
ELSE
  areal = -suction
END IF

ans = SaturationModel1(ng=params(1), mg=params(2), pg=params(3), &
                       smin=params(4), smax=params(5), suction=areal)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetSaturation1

!----------------------------------------------------------------------------
!                                                              GetSaturation
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSaturation2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetSaturation2()"
#endif

INTEGER(I4B) :: tsize, ii
REAL(DFP) :: areal

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = SIZE(suction)

IF (isSuction) THEN
  DO ii = 1, tsize
    ans(ii) = SaturationModel1(ng=params(1), mg=params(2), pg=params(3), &
                               smin=params(4), smax=params(5), &
                               suction=suction(ii))
  END DO
ELSE
  DO ii = 1, tsize
    areal = -suction(ii)
    ans(ii) = SaturationModel1(ng=params(1), mg=params(2), pg=params(3), &
                               smin=params(4), smax=params(5), &
                               suction=areal)
  END DO
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetSaturation2

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif

INTEGER(I4B) :: ii, origin, stat
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
obj%name = "VanGenuchten"
obj%totalParameters = 5_I4B
obj%paramNames(1) = "ng"
obj%paramNames(2) = "mg"
obj%paramNames(3) = "pg"
obj%paramNames(4) = "smin"
obj%paramNames(5) = "smax"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading ng ...')
#endif

DO ii = 1, obj%totalParameters
  CALL GetValue(table=table, &
                key=TRIM(obj%paramNames(ii)), &
                VALUE=obj%params(ii), &
                default_value=math%zero, &
                isFound=isok, &
                origin=origin, &
                stat=stat)

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, &
                    TRIM(obj%paramNames(ii))// &
                    " is not found in the toml table.")
#endif
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
