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

SUBMODULE(ScalarField_Class) PointNBCMethods
USE Display_Method, ONLY: ToString
USE ReallocateUtility, ONLY: Reallocate
USE NeumannBC_Class, ONLY: NeumannBC_
USE BaseType, ONLY: math => TypeMathOpt
USE InputUtility, ONLY: Input

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                         ApplyPointNeumannBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyPointNeumannBC
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyPointNeumannBC()"
#endif

! Internal variables
INTEGER(I4B), PARAMETER :: expandFactor = 2

REAL(DFP) :: times0(1)
REAL(DFP), ALLOCATABLE :: nodalvalue(:, :)
INTEGER(I4B), ALLOCATABLE :: nodenum(:)
INTEGER(I4B) :: idof, nrow, ncol, tsize, ibc
CLASS(NeumannBC_), POINTER :: nbcptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

times0(1) = Input(option=times, default=math%zero)

tsize = SIZE(obj%nbc_point)

ncol = 1
DO ibc = 1, tsize
  nrow = obj%nbc_point(ibc)%ptr%GetTotalNodeNum(fedof=obj%fedof)

  CALL Reallocate( &
    nodalvalue, nrow, ncol, isExpand=math%yes, expandFactor=expandFactor)

  CALL Reallocate( &
    nodenum, nrow, isExpand=math%yes, expandFactor=expandFactor)
END DO

DO ibc = 1, tsize
  nbcptr => obj%nbc_point(ibc)%ptr

  CALL nbcptr%Get( &
    nodalvalue=nodalvalue, nodenum=nodenum, times=times0, nrow=nrow, &
    ncol=ncol, fedof=obj%fedof, geofedof=obj%geofedof)

  DO idof = 1, ncol
    CALL obj%Set( &
      globalNode=nodenum(1:nrow), VALUE=nodalvalue(1:nrow, idof), &
      scale=scale, addContribution=math%yes, islocal=math%yes)
  END DO
END DO

IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)
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
