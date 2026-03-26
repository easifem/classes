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

SUBMODULE(GmshPlaneSurface_Class) GmshMethods
USE ExceptionHandler_Class, ONLY: e
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: ToString
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "GmshPlaneSurface_Class@GmshMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                            CreateGmshModel
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CreateGmshModel
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_CreateGmshModel()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: ierr, ii
INTEGER(I4B), ALLOCATABLE :: wireTags(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%outerloop%CreateGmshModel(gmsh=gmsh)

DO ii = 1, obj%innerloopsSize
  isok = ASSOCIATED(obj%innerloops(ii)%ptr)
  IF (.NOT. isok) CYCLE
  CALL obj%innerloops(ii)%ptr%CreateGmshModel(gmsh=gmsh)
END DO

CALL Reallocate(wireTags, obj%innerloopsSize + 1)
wireTags(1) = obj%outerloop%GetIndx()

DO ii = 1, obj%innerloopsSize
  isok = ASSOCIATED(obj%innerloops(ii)%ptr)
  IF (.NOT. isok) CYCLE
  wireTags(ii + 1) = obj%innerloops(ii)%ptr%GetIndx()
END DO

isok = obj%indx .NE. 0
IF (isok) THEN
  ierr = gmsh%model%geo%AddPlaneSurface(wireTags=wireTags, tag=obj%indx)
ELSE
  obj%indx = gmsh%model%geo%AddPlaneSurface(wireTags=wireTags)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_CreateGmshModel

!----------------------------------------------------------------------------
!                                                             Include Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GmshMethods
