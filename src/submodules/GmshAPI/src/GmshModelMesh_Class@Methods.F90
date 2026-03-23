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

SUBMODULE(GmshModelMesh_Class) Methods
USE ReallocateUtility, ONLY: Reallocate
USE ExceptionHandler_Class, ONLY: e
USE GmshBasicInterface, ONLY: GMSH_API_MAX_STR_LEN
USE GmshModelMeshInterface, ONLY: GmshModelMeshGenerate
USE GmshModelMeshInterface, ONLY: GmshModelMeshPartition
USE GmshModelMeshInterface, ONLY: GmshModelMeshUnpartition
USE GmshModelMeshInterface, ONLY: GmshModelMeshOptimize
USE GmshModelMeshInterface, ONLY: GmshModelMeshRecombine
USE GmshModelMeshInterface, ONLY: GmshModelMeshRefine
USE GmshModelMeshInterface, ONLY: GmshModelMeshSetOrder
USE CInterface, ONLY: C_PTR_TO_INT_VEC
USE ISO_C_BINDING, ONLY: C_INT
USE ISO_C_BINDING, ONLY: C_NULL_CHAR
USE ISO_C_BINDING, ONLY: C_LOC
USE ISO_C_BINDING, ONLY: C_SIZE_T

IMPLICIT NONE

CHARACTER(*), PARAMETER :: modName = "GmshModelMesh_Class@Methods.F90"
INTEGER(I4B), PARAMETER :: maxStrLen = GMSH_API_MAX_STR_LEN
INTEGER(C_INT) :: ierr

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Generate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Generate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelMeshGenerate(dim, ierr)
ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Generate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Partition
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Partition()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelMeshPartition(numPart, ierr)
ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Partition

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Unpartition
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Unpartition()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelMeshUnpartition(ierr)
ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Unpartition

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Optimize
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Optimize()"
#endif
CHARACTER(maxStrLen), TARGET :: method_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

method_ = TRIM(method)//C_NULL_CHAR

CALL GmshModelMeshOptimize(C_LOC(method_), force, niter, &
                           dimTags, SIZE(dimTags, KIND=C_SIZE_T), ierr)
ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Optimize

!----------------------------------------------------------------------------
!                                                                 Recombine
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Recombine
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Recombine()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelMeshRecombine(ierr)
ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Recombine

!----------------------------------------------------------------------------
!                                                                 Refine
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Refine
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Refine()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelMeshRefine(ierr)
ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Refine

!----------------------------------------------------------------------------
!                                                                 SetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetOrder()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelMeshSetOrder(order, ierr)
ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetOrder

!----------------------------------------------------------------------------
!                                                             Include Error
!----------------------------------------------------------------------------

#include  "../../include/errors.F90"

END SUBMODULE Methods

