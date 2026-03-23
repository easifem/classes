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

SUBMODULE(GmshModelOccMesh_Class) Methods
USE GlobalData, ONLY: DFP, I4B, LGT
USE ExceptionHandler_Class, ONLY: e
USE GmshBasicInterface, ONLY: GMSH_API_MAX_STR_LEN
USE GmshModelOccMeshInterface, ONLY: GmshModelOccMeshSetSize
USE ISO_C_BINDING, ONLY: C_INT
USE ISO_C_BINDING, ONLY: C_SIZE_T
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(LEN=*), PARAMETER :: modName = "GmshModelOccMesh_Class@Methods.F90"
#endif

INTEGER(I4B), PARAMETER :: maxStrLen = GMSH_API_MAX_STR_LEN
INTEGER(C_INT) :: ierr

CONTAINS

!----------------------------------------------------------------------------
!                                                                obj_SetSize
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSize
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetSize()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelOccMeshSetSize( &
  dimTags, SIZE(dimTags, kind=C_SIZE_T), meshSize, ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetSize

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
