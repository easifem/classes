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

SUBMODULE(AbstractMesh_Class) ElementDataMethods
USE ReallocateUtility
USE Display_Method
USE ReferenceElement_Method, ONLY: REFELEM_MAX_FACES
USE AbstractMeshUtility, ONLY: InitiateElementToElements3D, &
  & InitiateElementToElements2D
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                 InitiateElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateElementToElements
CHARACTER(*), PARAMETER :: myName = "obj_InitiateElementToElements()"
LOGICAL(LGT) :: problem

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

problem = .NOT. ALLOCATED(obj%elementData)

SELECT CASE (obj%xidim)
CASE (1_I4B)
CASE (2_I4B)
  IF (problem) THEN
    CALL obj%InitiateEdgeConnectivity()
  END IF
  CALL InitiateElementToElements2D(elementData=obj%elementData,  &
    & tEdgeInMesh=obj%tEdges)
CASE (3_I4B)
  IF (problem) THEN
    CALL obj%InitiateFaceConnectivity()
  END IF
  CALL InitiateElementToElements3D(elementData=obj%elementData,  &
    & tFaceInMesh=obj%tFaces)
CASE default
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: No case found.')
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_InitiateElementToElements

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ElementDataMethods
