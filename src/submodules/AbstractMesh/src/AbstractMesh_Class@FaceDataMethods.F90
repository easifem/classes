! This program is a part of EASIFEM library
! Copyright (C) (Since 2020)  Vikas Sharma, Ph.D
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

SUBMODULE(AbstractMesh_Class) FaceDataMethods

USE ReferenceElement_Method, ONLY: PARAM_REFELEM_MAX_FACES,  &
  & PARAM_REFELEM_MAX_POINTS,  &
  & RefElemGetGeoParam,  &
  & IsQuadrangle

USE ReferenceTriangle_Method, ONLY: FaceShapeMetaData_Triangle

USE ReferenceQuadrangle_Method, ONLY: FaceShapeMetaData_Quadrangle

USE ReallocateUtility, ONLY: Reallocate
USE FaceData_Class
USE FaceDataBinaryTree_Class
USE SortUtility
USE GlobalData, ONLY: INT8
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateFaceConnectivity
CHARACTER(*), PARAMETER :: myName = "obj_InitiateFaceConnectivity()"
INTEGER(I4B) :: tElements, iel, elemType, tFaces,  &
  & localFaces(4_I4B, PARAM_REFELEM_MAX_FACES), face(4), sorted_face(4),  &
  & tNodes, tsize1, tsize2, iface,  &
  & faceElemType(PARAM_REFELEM_MAX_FACES), tFaceNodes(PARAM_REFELEM_MAX_FACES),  &
  & aint, faceOrient(3_I4B)
LOGICAL(LGT) :: problem, abool
TYPE(FaceDataBinaryTree_) :: faceTree
TYPE(FaceData_) :: faceValue
TYPE(FaceData_), POINTER :: facePtr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

problem = obj%isFaceConnectivityInitiated
IF (problem) THEN
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & 'AbstractMesh_::obj face connectivity is already initiated.')
  RETURN
END IF

#ifdef DEBUG_VER
problem = .NOT. ALLOCATED(obj%elementData)

IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractMesh_::obj%elementData not allocated')
  RETURN
END IF
#endif

tElements = obj%GetTotalElements()

CALL faceTree%Initiate()

obj%isFaceConnectivityInitiated = .TRUE.

DO iel = 1, tElements

  problem = .NOT. obj%elementData(iel)%isActive
  IF (problem) CYCLE
  elemType = obj%elementData(iel)%name
  CALL RefElemGetGeoParam(elemType=elemType, &
    & tFaces=tFaces, tNodes=tNodes, faceCon=localFaces, &
    & faceOpt=1_I4B, faceElemType=faceElemType, &
    & tFaceNodes=tFaceNodes, order=1_I4B)

  CALL Reallocate(obj%elementData(iel)%globalFaces, tFaces)
  CALL Reallocate(obj%elementData(iel)%faceOrient, 3_I4B, tFaces)

  DO iface = 1, tFaces

    aint = tFaceNodes(iface)

    face(1:aint) = obj%elementData(iel)%globalNodes(localFaces(1:aint, iface))

    abool = IsQuadrangle(faceElemType(iface))
    IF (abool) THEN
      CALL FaceShapeMetaData_Quadrangle(face=face(1:aint),  &
        & sorted_face=sorted_face(1:aint), faceOrient=faceOrient)
    ELSE
      CALL FaceShapeMetaData_Triangle(face=face(1:aint),  &
        & sorted_face=sorted_face(1:aint), faceOrient=faceOrient)
    END IF

    facePtr => FaceData_Pointer(sorted_face)

    tsize1 = faceTree%SIZE()
    CALL faceTree%Insert(facePtr)
    tsize2 = faceTree%SIZE()

    obj%elementData(iel)%faceOrient(:, iface) = INT(faceOrient, kind=INT8)
    obj%tFaces = tsize2

    IF (tsize1 .NE. tsize2) THEN
      obj%elementData(iel)%globalFaces(iface) = tsize2
      facePtr%id = tsize2
    ELSE
      CALL Initiate(faceValue, sorted_face)
      facePtr => faceTree%GetValuePointer(faceValue)
      obj%elementData(iel)%globalFaces(iface) = facePtr%id
    END IF

  END DO

END DO

CALL faceTree%DEALLOCATE()
facePtr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_InitiateFaceConnectivity

END SUBMODULE FaceDataMethods
