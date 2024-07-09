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
USE ReferenceElement_Method, ONLY: &
  REFELEM_MAX_FACES => PARAM_REFELEM_MAX_FACES, &
  REFELEM_MAX_POINTS => PARAM_REFELEM_MAX_POINTS, &
  RefElemGetGeoParam, &
  IsQuadrangle

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
CHARACTER(*), PARAMETER :: myName = "obj_IntiiateFaceConnectivity()"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

SELECT CASE (obj%xidim)
CASE (2_I4B)
  CALL InitiateFaceConnectivity2D(obj)
CASE (3_I4B)
  CALL InitiateFaceConnectivity3D(obj)
CASE DEFAULT
  CALL e%RaiseError(modName//'::obj_InitiateFaceConnectivity - '// &
    & '[INTERNAL ERROR] :: Invalid dimension')
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_InitiateFaceConnectivity

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE InitiateFaceConnectivity2D(obj)
  CLASS(AbstractMesh_), INTENT(INOUT) :: obj

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "InitiateFaceConnectivity2D()"
  INTEGER(I4B) :: tElements, iel, elemType, tFaces, &
             localFaces(2_I4B, REFELEM_MAX_FACES), face0(2), sorted_face(2), &
                  tsize1, tsize2, iface

  LOGICAL(LGT) :: problem
  TYPE(FaceDataBinaryTree_) :: faceTree
  TYPE(FaceData_) :: faceValue
  TYPE(FaceData_), POINTER :: facePtr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  problem = obj%isFaceConnectivityInitiated
  IF (problem) RETURN

  problem = .NOT. ALLOCATED(obj%elementData)
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
           '[INTERNAL ERROR] :: AbstractMesh_::obj%elementData not allocated')
    RETURN
  END IF

  tElements = obj%GetTotalElements()

  CALL faceTree%Initiate()

  obj%isFaceConnectivityInitiated = .TRUE.

  DO iel = 1, tElements

    problem = .NOT. obj%elementData(iel)%ptr%isActive
    IF (problem) CYCLE

    elemType = obj%elementData(iel)%ptr%name

    CALL RefElemGetGeoParam(elemType=elemType, tFaces=tFaces, &
                            faceCon=localFaces, faceOpt=1_I4B, order=1_I4B)

    CALL Reallocate(obj%elementData(iel)%ptr%globalFaces, tFaces)
    CALL Reallocate(obj%elementData(iel)%ptr%faceOrient, 1_I4B, tFaces)

    DO iface = 1, tFaces

      face0 = &
        obj%elementData(iel)%ptr%globalNodes(localFaces(:, iface))

      sorted_face = Sort(face0)

      facePtr => FaceData_Pointer(sorted_face)

      tsize1 = faceTree%SIZE()
      CALL faceTree%Insert(facePtr)
      tsize2 = faceTree%SIZE()

      obj%tFaces = tsize2

      IF (face0(1) .GT. face0(2)) THEN
        obj%elementData(iel)%ptr%faceOrient(1, iface) = -1_INT8
      ELSE
        obj%elementData(iel)%ptr%faceOrient(1, iface) = 1_INT8
      END IF

      IF (tsize1 .NE. tsize2) THEN
        obj%elementData(iel)%ptr%globalFaces(iface) = tsize2
        facePtr%id = tsize2
      ELSE
        CALL Initiate(faceValue, sorted_face)
        facePtr => faceTree%GetValuePointer(faceValue)
        obj%elementData(iel)%ptr%globalFaces(iface) = facePtr%id
      END IF

    END DO

  END DO

  CALL faceTree%DEALLOCATE()
  facePtr => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif

END SUBROUTINE InitiateFaceConnectivity2D

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE InitiateFaceConnectivity3D(obj)
  CLASS(AbstractMesh_), INTENT(INOUT) :: obj

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "InitiateFaceConnectivity3D()"
  INTEGER(I4B) :: tElements, iel, elemType, tFaces, &
             localFaces(4_I4B, REFELEM_MAX_FACES), face0(4), sorted_face(4), &
                  tNodes, tsize1, tsize2, iface, &
             faceElemType(REFELEM_MAX_FACES), tFaceNodes(REFELEM_MAX_FACES), &
                  aint, faceOrient(3_I4B)
  LOGICAL(LGT) :: problem, abool
  TYPE(FaceDataBinaryTree_) :: faceTree
  TYPE(FaceData_) :: faceValue
  TYPE(FaceData_), POINTER :: facePtr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif

  problem = obj%isFaceConnectivityInitiated
  IF (problem) RETURN

  problem = .NOT. ALLOCATED(obj%elementData)
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: AbstractMesh_::obj%elementData not allocated')
    RETURN
  END IF

  tElements = obj%GetTotalElements()

  CALL faceTree%Initiate()

  obj%isFaceConnectivityInitiated = .TRUE.

  DO iel = 1, tElements

    problem = .NOT. obj%elementData(iel)%ptr%isActive
    IF (problem) CYCLE

    elemType = obj%elementData(iel)%ptr%name

    CALL RefElemGetGeoParam(elemType=elemType, &
      & tFaces=tFaces, tNodes=tNodes, faceCon=localFaces, &
      & faceOpt=1_I4B, faceElemType=faceElemType, &
      & tFaceNodes=tFaceNodes, order=1_I4B)

    CALL Reallocate(obj%elementData(iel)%ptr%globalFaces, tFaces)
    CALL Reallocate(obj%elementData(iel)%ptr%faceOrient, 3_I4B, tFaces)

    DO iface = 1, tFaces

      aint = tFaceNodes(iface)

      face0(1:aint) = &
        obj%elementData(iel)%ptr%globalNodes(localFaces(1:aint, iface))

      abool = IsQuadrangle(faceElemType(iface))
      IF (abool) THEN
        CALL FaceShapeMetaData_Quadrangle(face=face0(1:aint),  &
          & sorted_face=sorted_face(1:aint), faceOrient=faceOrient)
      ELSE
        CALL FaceShapeMetaData_Triangle(face=face0(1:aint),  &
          & sorted_face=sorted_face(1:aint), faceOrient=faceOrient)
      END IF

      facePtr => FaceData_Pointer(sorted_face)

      tsize1 = faceTree%SIZE()
      CALL faceTree%Insert(facePtr)
      tsize2 = faceTree%SIZE()

      obj%elementData(iel)%ptr%faceOrient(:, iface) = &
        INT(faceOrient, kind=INT8)
      obj%tFaces = tsize2

      IF (tsize1 .NE. tsize2) THEN
        obj%elementData(iel)%ptr%globalFaces(iface) = tsize2
        facePtr%id = tsize2
      ELSE
        CALL Initiate(faceValue, sorted_face)
        facePtr => faceTree%GetValuePointer(faceValue)
        obj%elementData(iel)%ptr%globalFaces(iface) = facePtr%id
      END IF

    END DO

  END DO

  CALL faceTree%DEALLOCATE()
  facePtr => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif

END SUBROUTINE InitiateFaceConnectivity3D

END SUBMODULE FaceDataMethods
