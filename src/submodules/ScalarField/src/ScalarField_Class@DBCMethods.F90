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

SUBMODULE(ScalarField_Class) DBCMethods
USE Display_Method, ONLY: ToString, Display
USE ReallocateUtility, ONLY: Reallocate
USE InputUtility, ONLY: Input
USE BaseType, ONLY: math => TypeMathOpt
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "ScalarField_Class@DBCMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                           ApplyDirichletBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyDirichletBC1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyDirichletBC1()"
#endif

INTEGER(I4B) :: nrow, ncol
INTEGER(I4B), PARAMETER :: expandFactor = 2

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%SetMaxTotalNodeNumForBC(dbc=dbc)

ncol = 1
nrow = obj%GetMaxTotalNodeNumForBC()

CALL Reallocate(obj%nodalvalue, nrow, ncol, isExpand=math%yes, &
                expandFactor=expandFactor)
CALL Reallocate(obj%nodenum, nrow, isExpand=math%yes, &
                expandFactor=expandFactor)

CALL dbc%Get( &
  nodalvalue=obj%nodalvalue, nodenum=obj%nodenum, times=times, nrow=nrow, &
  ncol=ncol, fedof=obj%fedof, geofedof=obj%geofedof)

CALL obj%Set( &
  globalNode=obj%nodenum(1:nrow), VALUE=obj%nodalvalue(1:nrow, 1), &
  islocal=math%yes)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ApplyDirichletBC1

!----------------------------------------------------------------------------
!                                                           ApplyDirichletBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyDirichletBC2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyDirichletBC2()"
#endif

INTEGER(I4B), PARAMETER :: expandFactor = 2
LOGICAL(LGT) :: isok
INTEGER(I4B) :: ibc, nrow, ncol, tbc

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%SetMaxTotalNodeNumForBC(dbcvec=dbc)
ncol = 1
nrow = obj%GetMaxTotalNodeNumForBC()

CALL Reallocate(obj%nodalvalue, nrow, ncol, isExpand=math%yes, &
                expandFactor=expandFactor)
CALL Reallocate(obj%nodenum, nrow, isExpand=math%yes, &
                expandFactor=expandFactor)

tbc = SIZE(dbc)
DO ibc = 1, tbc
  isok = ASSOCIATED(dbc(ibc)%ptr)
  IF (.NOT. isok) CYCLE

  CALL dbc(ibc)%ptr%Get( &
    nodalvalue=obj%nodalvalue, nodenum=obj%nodenum, times=times, nrow=nrow, &
    ncol=ncol, fedof=obj%fedof, geofedof=obj%geofedof)

  CALL obj%Set( &
    globalNode=obj%nodenum(1:nrow), VALUE=obj%nodalvalue(1:nrow, 1), &
    islocal=math%yes)
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
!                                                       ApplyZeroDirichletBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyZeroDirichletBC1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyZeroDirichletBC1()"
#endif

INTEGER(I4B) :: tsize, iNodeOnNode, iNodeOnEdge, iNodeOnFace
INTEGER(I4B), PARAMETER :: expandFactor = 2

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%SetMaxTotalNodeNumForBC(dbc=dbc)

tsize = obj%GetMaxTotalNodeNumForBC()

CALL Reallocate(obj%nodenum, tsize, isExpand=math%yes, &
                expandFactor=expandFactor)

CALL dbc%GetNodeNumber(fedof=obj%fedof, nodeNum=obj%nodeNum, tsize=tsize, &
                       iNodeOnNode=iNodeOnNode, iNodeOnFace=iNodeOnFace, &
                       iNodeOnEdge=iNodeOnEdge)

CALL obj%Set( &
  globalNode=obj%nodenum(1:tsize), VALUE=math%zero, islocal=math%yes)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ApplyZeroDirichletBC1

!----------------------------------------------------------------------------
!                                                      ApplyZeroDirichletBC2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyZeroDirichletBC2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyZeroDirichletBC2()"
#endif

INTEGER(I4B), PARAMETER :: expandFactor = 2
LOGICAL(LGT) :: isok
INTEGER(I4B) :: ibc, tbc, tsize, iNodeOnNode, iNodeOnEdge, iNodeOnFace

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%SetMaxTotalNodeNumForBC(dbcvec=dbc)
tsize = obj%GetMaxTotalNodeNumForBC()

CALL Reallocate(obj%nodenum, tsize, isExpand=math%yes, &
                expandFactor=expandFactor)

tbc = SIZE(dbc)
DO ibc = 1, tbc
  isok = ASSOCIATED(dbc(ibc)%ptr)
  IF (.NOT. isok) CYCLE

  CALL dbc(ibc)%ptr%GetNodeNumber( &
    fedof=obj%fedof, nodeNum=obj%nodeNum, tsize=tsize, &
    iNodeOnNode=iNodeOnNode, iNodeOnFace=iNodeOnFace, &
    iNodeOnEdge=iNodeOnEdge)

  CALL obj%Set(globalNode=obj%nodenum(1:tsize), VALUE=math%zero, &
               islocal=math%yes)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ApplyZeroDirichletBC2

!----------------------------------------------------------------------------
!                                                           ApplyDirichletBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyZeroDirichletBC3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyZeroDirichletBC3()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%ApplyZeroDirichletBC(dbc=obj%dbc)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ApplyZeroDirichletBC3

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE DBCMethods
