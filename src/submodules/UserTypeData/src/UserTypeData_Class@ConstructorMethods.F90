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

SUBMODULE(UserTypeData_Class) ConstructorMethods
USE ExceptionHandler_Class, ONLY: e

IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = __FILE__
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                              AllocateFields
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AllocateFields
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AllocateFields()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = .NOT. ALLOCATED(obj%fields)
CALL AssertError1(isok, myName, &
                  "obj%fields is already allocated")
#endif

ALLOCATE (obj%fields(tsize))

DO ii = 1, tsize
  obj%fields(ii)%ptr => NULL()
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AllocateFields

!----------------------------------------------------------------------------
!                                                            AllocateMethods
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AllocateMethods
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AllocateMethods()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = .NOT. ALLOCATED(obj%methods)
CALL AssertError1(isok, myName, &
                  "obj%methods is already allocated")
#endif

ALLOCATE (obj%methods(tsize))

DO ii = 1, tsize
  obj%methods(ii)%ptr => NULL()
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AllocateMethods

!----------------------------------------------------------------------------
!                                                             Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
