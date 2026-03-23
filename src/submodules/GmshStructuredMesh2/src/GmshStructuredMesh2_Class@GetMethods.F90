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

SUBMODULE(GmshStructuredMesh2_Class) GetMethods

IMPLICIT NONE
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: &
  modName = "GmshStructuredMesh2_Class@GetMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                              GetNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeNumber
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeNumber()"
#endif
INTEGER(I4B) :: ni

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ni = obj%tPoints(1)
ans = (j - 1) * ni + i

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNodeNumber

!----------------------------------------------------------------------------
!                                                      GetEdgeNumberOnAxis1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetEdgeNumberOnAxis1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetEdgeNumberOnAxis1()"
#endif
INTEGER(I4B) :: ni

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ni = obj%tPoints(1) - 1
ans = (j - 1) * ni + i

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetEdgeNumberOnAxis1

!----------------------------------------------------------------------------
!                                                      GetEdgeNumberOnAxis2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetEdgeNumberOnAxis2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetEdgeNumberOnAxis2()"
#endif
INTEGER(I4B) :: ni

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ni = obj%tPoints(1) - 1
ans = obj%tEdges1 + (j - 1) * (ni + 1) + i

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetEdgeNumberOnAxis2

!----------------------------------------------------------------------------
!                                                            GetMeshTypeName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMeshTypeName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMeshTypeName()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

SELECT CASE (meshType)
CASE (TypeGmshStructuredMesh2Opt%progression)
  ans = TypeGmshStructuredMesh2Opt%progression_char
CASE (TypeGmshStructuredMesh2Opt%bump)
  ans = TypeGmshStructuredMesh2Opt%bump_char
CASE DEFAULT
  ans = TypeGmshStructuredMesh2Opt%bump_char
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMeshTypeName

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include  "../../include/errors.F90"

END SUBMODULE GetMethods
