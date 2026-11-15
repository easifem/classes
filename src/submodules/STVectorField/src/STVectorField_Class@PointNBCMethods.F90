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
USE AbstractOneDimFE_Class, ONLY: AbstractOneDimFE_
USE MassMatrix_Method, ONLY: MassMatrix_
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: ElemShapeData_
USE Display_Method, ONLY: ToString, Display
USE ReallocateUtility, ONLY: Reallocate
USE NeumannBC_Class, ONLY: NeumannBC_
USE BaseType, ONLY: math => TypeMathOpt

#ifdef DEBUG_VER
USE Display_Method, ONLY: Display
#endif
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "STVectorField_Class@PointNBCMethods.F90"
#endif
CONTAINS

!----------------------------------------------------------------------------
!                                                         ApplyPointNeumannBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyPointNeumannBC
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyPointNeumannBC1()"
#endif

INTEGER(I4B), PARAMETER :: expandFactor = 2
INTEGER(I4B) :: idof, nrow, ncol, tbc, ibc, spaceCompo, nnt
CLASS(NeumannBC_), POINTER :: nbcptr
CLASS(AbstractOneDimFE_), POINTER :: timefeptr
LOGICAL(LGT) :: isok
TYPE(QuadraturePoint_) :: timeQuad
TYPE(ElemShapeData_) :: timeElemsd, timeGeoElemsd
REAL(DFP), ALLOCATABLE :: ct(:, :)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tbc = obj%GetTotalPointNBC()
isok = tbc .GT. 0

IF (.NOT. isok) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

ncol = obj%timeCompo
nrow = obj%GetMaxTotalNodeNumForBC()

CALL Reallocate(obj%nodalvalue, nrow, ncol, isExpand=math%yes, &
                expandFactor=expandFactor)
CALL Reallocate(obj%nodenum, nrow, isExpand=math%yes, &
                expandFactor=expandFactor)

CALL obj%timefedof%SetFE()
timefeptr => obj%timefedof%GetFEPointer()
CALL timefeptr%GetGlobalTimeElemShapeData( &
  elemsd=timeElemsd, geoelemsd=timegeoelemsd, quad=timeQuad, times=times)

nnt = timeElemsd%nns

CALL Reallocate(ct, nnt, nnt)

CALL MassMatrix_(test=timeElemsd, trial=timeElemsd, ans=ct, &
                 nrow=nrow, ncol=ncol)

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

  DO idof = 1, nrow
    obj%nodalValue(idof, 1:ncol) = &
      MATMUL(ct(1:nnt, 1:nnt), obj%nodalValue(idof, 1:nnt))
  END DO

  DO idof = 1, ncol
    CALL obj%Set( &
      globalNode=obj%nodenum(1:nrow), VALUE=obj%nodalvalue(1:nrow, idof), &
      scale=scale, addContribution=math%yes, islocal=math%yes, &
      spaceCompo=spaceCompo, timeCompo=idof)
  END DO
END DO

nbcptr => NULL()
timefeptr => NULL()

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
