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

SUBMODULE(STVectorField_Class) DBCMethods
USE Display_Method, ONLY: ToString
USE ReallocateUtility, ONLY: Reallocate
USE BaseType, ONLY: math => TypeMathOpt

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyDirichletBC1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyDirichletBC1()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B), PARAMETER :: expandFactor = 2
INTEGER(I4B) :: idof, spaceCompo, nrow, ncol

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%SetMaxTotalNodeNumForBC(dbc=dbc)
nrow = obj%GetMaxTotalNodeNumForBC()
ncol = obj%timeCompo

CALL Reallocate(obj%nodalvalue, nrow, ncol, isExpand=math%yes, &
                expandFactor=expandFactor)
CALL Reallocate(obj%nodenum, nrow, isExpand=math%yes, &
                expandFactor=expandFactor)

CALL dbc%Get( &
  nodalValue=obj%nodalValue, nodeNum=obj%nodeNum, times=times, nrow=nrow, &
  ncol=ncol, fedof=obj%fedof, geofedof=obj%geofedof, timefedof=obj%timefedof)

spaceCompo = dbc%GetDOFNo()

#ifdef DEBUG_VER
isok = spaceCompo .LE. obj%spaceCompo
CALL AssertError1( &
  isok, myName, &
  "DOFNo obtained from dbc ("//ToString(spaceCompo)// &
  ") is greater than the spaceCompo in obj ("//ToString(obj%spaceCompo)//")")
#endif

DO idof = 1, ncol
  CALL obj%Set( &
    globalNode=obj%nodeNum(1:nrow), VALUE=obj%nodalValue(1:nrow, idof), &
    timeCompo=idof, spaceCompo=spaceCompo, islocal=math%yes)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ApplyDirichletBC1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyDirichletBC2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyDirichletBC2()"
#endif

INTEGER(I4B), PARAMETER :: expandFactor = 2
INTEGER(I4B) :: ibc, idof, spaceCompo, tbc, nrow, ncol
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%SetMaxTotalNodeNumForBC(dbcvec=dbc)
nrow = obj%GetMaxTotalNodeNumForBC()
ncol = obj%timeCompo

CALL Reallocate(obj%nodalvalue, nrow, ncol, isExpand=math%yes, &
                expandFactor=expandFactor)
CALL Reallocate(obj%nodenum, nrow, isExpand=math%yes, &
                expandFactor=expandFactor)

tbc = SIZE(dbc)

DO ibc = 1, tbc

  isok = ASSOCIATED(dbc(ibc)%ptr)
  IF (.NOT. isok) CYCLE

  CALL dbc(ibc)%ptr%Get( &
    nodalValue=obj%nodalValue, nodeNum=obj%nodeNum, times=times, nrow=nrow, &
    ncol=ncol, fedof=obj%fedof, geofedof=obj%geofedof, &
    timefedof=obj%timefedof)

  spaceCompo = dbc(ibc)%ptr%GetDOFNo()

#ifdef DEBUG_VER
  isok = spaceCompo .LE. obj%spaceCompo
  CALL AssertError1( &
    isok, myName, "DOFNo obtained from dbc("//ToString(ibc)// &
    ") which is "//ToString(spaceCompo)// &
    ") is greater than the spaceCompo in obj which is "// &
    ToString(obj%spaceCompo))
#endif

  DO idof = 1, ncol
    CALL obj%Set( &
      globalNode=obj%nodeNum(1:nrow), VALUE=obj%nodalValue(1:nrow, idof), &
      timeCompo=idof, spaceCompo=spaceCompo, islocal=math%yes)
  END DO

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ApplyDirichletBC2

!----------------------------------------------------------------------------
!                                                           ApplyDirichletBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyDirichletBC3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyDirichletBC3()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%ApplyDirichletBC(dbc=obj%dbc, times=times)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ApplyDirichletBC3

!----------------------------------------------------------------------------
!                                                             Include Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE DBCMethods
