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

SUBMODULE(BlockNodeField_Class) DBCMethods
USE InputUtility, ONLY: Input
USE DOF_Method, ONLY: OPERATOR(.timeComponents.)
USE DOF_Method, ONLY: OPERATOR(.spaceComponents.)
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
INTEGER(I4B) :: tTimeCompo, tSpaceCompo
#endif

INTEGER(I4B), PARAMETER :: expandFactor = 2
INTEGER(I4B) :: spaceCompo, nrow, ncol

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
tTimeCompo = obj%dof.timeComponents.ivar
isok = tTimeCompo .EQ. 1
CALL AssertError1(isok, myName, &
                  "timeComponents is not equal to 1")
#endif

spaceCompo = dbc%GetDOFNo()

#ifdef DEBUG_VER
tSpaceCompo = obj%dof.spaceComponents.ivar
isok = spaceCompo .LE. tSpaceCompo
CALL AssertError1(isok, myName, &
                  "dbc%GetDOFNo() is greater than obj%dof.spaceComponents")
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%fedofs(ivar)%ptr)
CALL AssertError1(isok, myName, &
                  "obj%fedofs(ivar)%ptr is not associated")
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%geofedofs(ivar)%ptr)
CALL AssertError1(isok, myName, &
                  "obj%geofedofs(ivar)%ptr is not associated")
#endif

CALL obj%SetMaxTotalNodeNumForBC(dbc=dbc, ivar=ivar)
nrow = obj%GetMaxTotalNodeNumForBC(ivar=ivar)
ncol = 1

CALL Reallocate(obj%nodalvalue, nrow, ncol, isExpand=math%yes, &
                expandFactor=expandFactor)
CALL Reallocate(obj%nodenum, nrow, isExpand=math%yes, &
                expandFactor=expandFactor)

CALL dbc%Get( &
  nodalValue=obj%nodalValue, nodeNum=obj%nodeNum, times=times, nrow=nrow, &
  ncol=ncol, fedof=obj%fedofs(ivar)%ptr, geofedof=obj%geofedofs(ivar)%ptr)

CALL obj%Set( &
  globalNode=obj%nodeNum(1:nrow), VALUE=obj%nodalValue(1:nrow, 1), &
  ivar=ivar, spaceCompo=spaceCompo, timeCompo=math%one_i, islocal=math%yes)

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
INTEGER(I4B) :: tTimeCompo, tSpaceCompo
#endif

INTEGER(I4B), PARAMETER :: expandFactor = 2
LOGICAL(LGT) :: isok
INTEGER(I4B) :: ibc, spaceCompo, tbc, nrow, ncol

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
tTimeCompo = obj%dof.timeComponents.ivar
isok = tTimeCompo .EQ. 1
CALL AssertError1(isok, myName, &
                  "timeComponents is not equal to 1")
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%fedofs(ivar)%ptr)
CALL AssertError1(isok, myName, &
                  "obj%fedofs(ivar)%ptr is not associated")
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%geofedofs(ivar)%ptr)
CALL AssertError1(isok, myName, &
                  "obj%geofedofs(ivar)%ptr is not associated")
#endif

#ifdef DEBUG_VER
tSpaceCompo = obj%dof.spaceComponents.ivar
#endif

CALL obj%SetMaxTotalNodeNumForBC(dbcvec=dbc, ivar=ivar)
nrow = obj%GetMaxTotalNodeNumForBC(ivar=ivar)
ncol = 1

CALL Reallocate(obj%nodalvalue, nrow, ncol, isExpand=math%yes, &
                expandFactor=expandFactor)
CALL Reallocate(obj%nodenum, nrow, isExpand=math%yes, &
                expandFactor=expandFactor)

tbc = SIZE(dbc)

DO ibc = 1, tbc
  isok = ASSOCIATED(dbc(ibc)%ptr)
  IF (.NOT. isok) CYCLE

  spaceCompo = dbc(ibc)%ptr%GetDOFNo()

#ifdef DEBUG_VER
  isok = spaceCompo .LE. tSpaceCompo
  CALL AssertError1( &
    isok, myName, "dbc%GetDOFNo() is greater than obj%dof.spaceComponents")
#endif

  CALL dbc(ibc)%ptr%Get( &
    nodalValue=obj%nodalValue, nodeNum=obj%nodeNum, times=times, nrow=nrow, &
    ncol=ncol, fedof=obj%fedofs(ivar)%ptr, geofedof=obj%geofedofs(ivar)%ptr)

  CALL obj%Set( &
    globalNode=obj%nodeNum(1:nrow), VALUE=obj%nodalValue(1:nrow, 1), &
    ivar=ivar, spaceCompo=spaceCompo, timeCompo=math%one_i, islocal=math%yes)

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ApplyDirichletBC2

!----------------------------------------------------------------------------
!                                                            ApplyDirichletBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyDirichletBC3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyDirichletBC3()"
LOGICAL(LGT) :: isok
INTEGER(I4B) :: tSpaceCompo
#endif

INTEGER(I4B), PARAMETER :: expandFactor = 2
INTEGER(I4B) :: spaceCompo, nrow, ncol, tTimeCompo, idof

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tTimeCompo = obj%dof.timeComponents.ivar
spaceCompo = dbc%GetDOFNo()

#ifdef DEBUG_VER
tSpaceCompo = obj%dof.spaceComponents.ivar
isok = spaceCompo .LE. tSpaceCompo
CALL AssertError1(isok, myName, &
                  "dbc%GetDOFNo() is greater than obj%dof.spaceComponents")
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%fedofs(ivar)%ptr)
CALL AssertError1(isok, myName, &
                  "obj%fedofs(ivar)%ptr is not associated")
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%geofedofs(ivar)%ptr)
CALL AssertError1(isok, myName, &
                  "obj%geofedofs(ivar)%ptr is not associated")
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%timefedofs(ivar)%ptr)
CALL AssertError1(isok, myName, &
                  "obj%timefedofs(ivar)%ptr is not associated")
#endif

CALL obj%SetMaxTotalNodeNumForBC(dbc=dbc, ivar=ivar)
nrow = obj%GetMaxTotalNodeNumForBC(ivar=ivar)
ncol = tTimeCompo

CALL Reallocate(obj%nodalvalue, nrow, ncol, isExpand=math%yes, &
                expandFactor=expandFactor)
CALL Reallocate(obj%nodenum, nrow, isExpand=math%yes, &
                expandFactor=expandFactor)

CALL dbc%Get( &
  nodalValue=obj%nodalValue, nodeNum=obj%nodeNum, times=times, nrow=nrow, &
  ncol=ncol, fedof=obj%fedofs(ivar)%ptr, geofedof=obj%geofedofs(ivar)%ptr, &
  timefedof=obj%timefedofs(ivar)%ptr)

#ifdef DEBUG_VER
isok = ncol .EQ. tTimeCompo
CALL AssertError1(isok, myName, "tTimeCompo not same as ncol")
#endif

DO idof = 1, tTimeCompo
  CALL obj%Set( &
    globalNode=obj%nodeNum(1:nrow), VALUE=obj%nodalValue(1:nrow, idof), &
    ivar=ivar, spaceCompo=spaceCompo, timeCompo=idof, islocal=math%yes)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ApplyDirichletBC3

!----------------------------------------------------------------------------
!                                                            ApplyDirichletBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyDirichletBC4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyDirichletBC4()"
#endif

INTEGER(I4B), PARAMETER :: expandFactor = 2
LOGICAL(LGT) :: isok
INTEGER(I4B) :: ibc, spaceCompo, tbc, nrow, ncol, tTimeCompo, idof, &
                tSpaceCompo

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tTimeCompo = obj%dof.timeComponents.ivar
tSpaceCompo = obj%dof.spaceComponents.ivar

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%fedofs(ivar)%ptr)
CALL AssertError1(isok, myName, &
                  "obj%fedofs(ivar)%ptr is not associated")
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%geofedofs(ivar)%ptr)
CALL AssertError1(isok, myName, &
                  "obj%geofedofs(ivar)%ptr is not associated")
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%timefedofs(ivar)%ptr)
CALL AssertError1(isok, myName, &
                  "obj%timefedofs(ivar)%ptr is not associated")
#endif

CALL obj%SetMaxTotalNodeNumForBC(dbcvec=dbc, ivar=ivar)
nrow = obj%GetMaxTotalNodeNumForBC(ivar=ivar)
ncol = tTimeCompo

CALL Reallocate(obj%nodalvalue, nrow, ncol, isExpand=math%yes, &
                expandFactor=expandFactor)
CALL Reallocate(obj%nodenum, nrow, isExpand=math%yes, &
                expandFactor=expandFactor)

tbc = SIZE(dbc)

DO ibc = 1, tbc
  isok = ASSOCIATED(dbc(ibc)%ptr)
  IF (.NOT. isok) CYCLE

  spaceCompo = dbc(ibc)%ptr%GetDOFNo()

#ifdef DEBUG_VER
  isok = spaceCompo .LE. tSpaceCompo
  CALL AssertError1( &
    isok, myName, "dbc%GetDOFNo() is greater than obj%dof.spaceComponents")
#endif

  CALL dbc(ibc)%ptr%Get( &
    nodalValue=obj%nodalValue, nodeNum=obj%nodeNum, times=times, nrow=nrow, &
    ncol=ncol, fedof=obj%fedofs(ivar)%ptr, geofedof=obj%geofedofs(ivar)%ptr, &
    timefedof=obj%timefedofs(ivar)%ptr)

#ifdef DEBUG_VER
  isok = ncol .EQ. tTimeCompo
  CALL AssertError1(isok, myName, "tTimeCompo not same as ncol")
#endif

  DO idof = 1, tTimeCompo
    CALL obj%Set( &
      globalNode=obj%nodeNum(1:nrow), VALUE=obj%nodalValue(1:nrow, idof), &
      ivar=ivar, spaceCompo=spaceCompo, timeCompo=idof, islocal=math%yes)
  END DO

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ApplyDirichletBC4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE DBCMethods
