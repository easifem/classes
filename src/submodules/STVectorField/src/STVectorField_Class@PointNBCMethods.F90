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

SUBMODULE(STVectorField_Class) PointNBCMethods
USE Display_Method, ONLY: ToString
USE ReallocateUtility, ONLY: Reallocate
USE NeumannBC_Class, ONLY: NeumannBC_
USE BaseType, ONLY: math => TypeMathOpt

#ifdef DEBUG_VER
USE Display_Method, ONLY: Display
#endif

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                         ApplyPointNeumannBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyPointNeumannBC
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyPointNeumannBC1()"
#endif

INTEGER(I4B), PARAMETER :: expandFactor = 2
INTEGER(I4B) :: idof, nrow, ncol, tbc, ibc, spaceCompo
CLASS(NeumannBC_), POINTER :: nbcptr
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ncol = obj%timeCompo
nrow = obj%GetMaxTotalNodeNumForBC()

CALL Reallocate(obj%nodalvalue, nrow, ncol, isExpand=math%yes, &
                expandFactor=expandFactor)
CALL Reallocate(obj%nodenum, nrow, isExpand=math%yes, &
                expandFactor=expandFactor)

tbc = SIZE(obj%nbc_point)
DO ibc = 1, tbc
  nbcptr => obj%nbc_point(ibc)%ptr
  isok = ASSOCIATED(nbcptr)
  IF (.NOT. isok) CYCLE

  spaceCompo = nbcptr%GetDOFNo()

#ifdef DEBUG_VER
  isok = spaceCompo .LE. obj%spaceCompo
  CALL AssertError1( &
    isok, myName, &
    "DOFNo obtained from nbc_point("//ToString(ibc)// &
    ") is "//ToString(spaceCompo)//" is greater than total space &
    &components of the field which is "//ToString(obj%spaceCompo))
#endif

  CALL nbcptr%Get( &
    nodalvalue=obj%nodalvalue, nodenum=obj%nodenum, times=times, &
    nrow=nrow, ncol=ncol, fedof=obj%fedof, geofedof=obj%geofedof, &
    timefedof=obj%timefedof)

  DO idof = 1, ncol
    CALL obj%Set( &
      globalNode=obj%nodenum(1:nrow), VALUE=obj%nodalvalue(1:nrow, idof), &
      scale=scale, addContribution=math%yes, islocal=math%yes, &
      spaceCompo=spaceCompo, timeCompo=idof)
  END DO
END DO

nbcptr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ApplyPointNeumannBC

!----------------------------------------------------------------------------
!                                                             Include Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE PointNBCMethods
