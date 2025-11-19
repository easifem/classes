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

SUBMODULE(STScalarField_Class) PointNBCMethods
! USE Display_Method, ONLY: ToString
! USE ReallocateUtility, ONLY: Reallocate
! USE NeumannBC_Class, ONLY: NeumannBC_
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                         ApplyPointNeumannBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyPointNeumannBC1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyPointNeumannBC1()"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

! INTEGER(I4B), PARAMETER :: expandFactor = 2
! LOGICAL(LGT), PARAMETER :: yes = .TRUE.
!
! REAL(DFP), ALLOCATABLE :: nodalvalue(:, :)
! INTEGER(I4B), ALLOCATABLE :: nodenum(:)
! LOGICAL(LGT) :: istimes
! INTEGER(I4B) :: idof, nrow, ncol, tsize, ibc
! CLASS(NeumannBC_), POINTER :: nbcptr
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         '[START] ')
! #endif
!
! istimes = PRESENT(times)
!
! #ifdef DEBUG_VER
! tsize = 0
! IF (istimes) THEN
!   tsize = SIZE(times)
!   isok = tsize .EQ. 1
!   CALL AssertError1(isok, myName, &
!                    'SIZE(times) is '//ToString(tsize)//', but it should be 1')
! END IF
! #endif
!
! tsize = SIZE(obj%nbc_point)
!
! ncol = 1
! DO ibc = 1, tsize
!   nrow = obj%nbc_point(ibc)%ptr%GetTotalNodeNum(fedof=obj%fedof)
!   CALL Reallocate(nodalvalue, nrow, ncol, isExpand=yes, &
!                   expandFactor=expandFactor)
!   CALL Reallocate(nodenum, nrow, isExpand=yes, &
!                   expandFactor=expandFactor)
! END DO
!
! IF (istimes) THEN
!   DO ibc = 1, tsize
!     nbcptr => obj%nbc_point(ibc)%ptr
!     CALL nbcptr%Get(nodalvalue=nodalvalue, nodenum=nodenum, &
!                     times=times, nrow=nrow, ncol=ncol, fedof=obj%fedof, &
!                     geofedof=obj%geofedof)
!     DO idof = 1, ncol
!       CALL obj%Set(globalNode=nodenum(1:nrow), &
!                    VALUE=nodalvalue(1:nrow, idof), &
!                    scale=scale, addContribution=yes, &
!                    islocal=yes)
!     END DO
!   END DO
!
!   IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
!   IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)
!   nbcptr => NULL()
!
! #ifdef DEBUG_VER
!   CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                           '[END] ')
! #endif
!
!   RETURN
! END IF
!
! DO ibc = 1, tsize
!   nbcptr => obj%nbc_point(ibc)%ptr
!   CALL nbcptr%Get(nodalvalue=nodalvalue, nodenum=nodenum, nrow=nrow, &
!                   ncol=ncol, fedof=obj%fedof, geofedof=obj%geofedof)
!
!   CALL obj%Set(globalNode=nodenum(1:nrow), VALUE=nodalvalue(1:nrow, 1), &
!                scale=scale, addContribution=yes, islocal=yes)
! END DO
!
! nbcptr => NULL()
! IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
! IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ApplyPointNeumannBC1

!----------------------------------------------------------------------------
!                                                             Include Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE PointNBCMethods
