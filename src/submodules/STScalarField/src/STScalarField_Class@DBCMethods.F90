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

SUBMODULE(STScalarField_Class) DBCMethods
USE Display_Method, ONLY: ToString
USE ReallocateUtility, ONLY: Reallocate
USE BaseType, ONLY: math => TypeMathOpt

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 ApplyDBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyDirichletBC1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyDirichletBC1()"
! LOGICAL(LGT) :: isok
! INTEGER(I4B) :: aint
#endif

INTEGER(I4B), PARAMETER :: expandFactor = 2
INTEGER(I4B) :: nrow, ncol, idof

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%SetMaxTotalNodeNumForBC(dbc=dbc)
ncol = obj%timeCompo
nrow = obj%GetMaxTotalNodeNumForBC()

CALL Reallocate(obj%nodalvalue, nrow, ncol, isExpand=math%yes, &
                expandFactor=expandFactor)
CALL Reallocate(obj%nodenum, nrow, isExpand=math%yes, &
                expandFactor=expandFactor)

CALL dbc%Get( &
  nodalvalue=obj%nodalvalue, nodenum=obj%nodenum, times=times, nrow=nrow, &
  ncol=ncol, fedof=obj%fedof, geofedof=obj%geofedof, timefedof=obj%timefedof)

DO idof = 1, ncol
  CALL obj%Set( &
    globalNode=obj%nodenum(1:nrow), VALUE=obj%nodalvalue(1:nrow, idof), &
    timecompo=idof, islocal=math%yes)
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
INTEGER(I4B) :: idof, ii, tbc, nrow, ncol

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%SetMaxTotalNodeNumForBC(dbcvec=dbc)
ncol = obj%timeCompo
nrow = obj%GetMaxTotalNodeNumForBC()

CALL Reallocate(obj%nodalvalue, nrow, ncol, isExpand=math%yes, &
                expandFactor=expandFactor)
CALL Reallocate(obj%nodenum, nrow, isExpand=math%yes, &
                expandFactor=expandFactor)

tbc = SIZE(dbc)

DO ii = 1, tbc

  CALL dbc(ii)%ptr%Get( &
    nodalvalue=obj%nodalvalue, nodenum=obj%nodenum, times=times, nrow=nrow, &
    ncol=ncol, fedof=obj%fedof, geofedof=obj%geofedof, &
    timefedof=obj%timefedof)

  DO idof = 1, ncol
    CALL obj%Set( &
      globalNode=obj%nodenum(1:nrow), VALUE=obj%nodalvalue(1:nrow, idof), &
      timecompo=idof, islocal=math%yes)
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
!                                                               Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE DBCMethods
