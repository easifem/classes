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
USE BaseType, ONLY: TypeRefelemOpt
USE BaseType, ONLY: math => TypeMathOpt
USE FaceData_Class, ONLY: FaceData_
USE FaceData_Class, ONLY: FaceData_Pointer
USE FaceData_Class, ONLY: Initiate
USE FaceDataBinaryTree_Class, ONLY: FaceDataBinaryTree_
USE GlobalData, ONLY: INT8
USE ReallocateUtility, ONLY: Reallocate
USE ReferenceElement_Method, ONLY: RefElemGetGeoParam
USE ReferenceElement_Method, ONLY: IsQuadrangle
USE ReferenceQuadrangle_Method, ONLY: FaceShapeMetaData_Quadrangle
USE ReferenceTriangle_Method, ONLY: FaceShapeMetaData_Triangle
USE SortUtility, ONLY: Sort
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "AbstractMesh_Class@FaceDataMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateFaceConnectivity
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_IntiiateFaceConnectivity()"
#endif

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

#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, "No case found")
#endif

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_InitiateFaceConnectivity

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! The faces (edges) are found with buckets keyed by the smaller global node
! of the face: head(a) is the last face whose smaller node is a, nextFace
! links the faces of the same bucket, and faceNode2 is the larger node.
! A node has only a few faces, so a lookup is O(1). A binary tree is not
! used, because it is not balanced and it becomes a linked list when the
! faces are inserted in sorted order (structured meshes), O(n^2) in total.
!
! The id of a face is the order in which it is found first.

SUBROUTINE InitiateFaceConnectivity2D(obj)
  CLASS(AbstractMesh_), INTENT(INOUT) :: obj

  ! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "InitiateFaceConnectivity2D()"
#endif
  INTEGER(I4B) :: tElements, iel, elemType, tFaces, face0(2), &
    sorted_face(2), iface, faceId, totalFaces, &
    localFaces(2_I4B, TypeRefelemOpt%maxFaces)
  INTEGER(I4B), ALLOCATABLE :: head(:), nextFace(:), faceNode2(:)
  !! buckets of faces, see the comment above

  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  isok = obj%isFaceConnectivityInitiated
  IF (isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

#ifdef DEBUG_VER
  isok = ALLOCATED(obj%elementData)
  CALL AssertError1(isok, myName, "obj%elementData is not allocated.")
#endif

  tElements = obj%GetTotalElements()

  ! every face of every element is a new face in the worst case
  CALL Reallocate(head, obj%maxNptrs)
  CALL Reallocate(nextFace, tElements * TypeRefelemOpt%maxFaces)
  CALL Reallocate(faceNode2, tElements * TypeRefelemOpt%maxFaces)
  totalFaces = 0

  obj%isFaceConnectivityInitiated = .TRUE.

  loop1: DO iel = 1, tElements

    isok = .NOT. obj%elementData(iel)%ptr%isActive
    IF (isok) CYCLE loop1

    elemType = obj%elementData(iel)%ptr%name

    CALL RefElemGetGeoParam(elemType=elemType, tFaces=tFaces, &
                            faceCon=localFaces, faceOpt=1_I4B, order=1_I4B)

    CALL Reallocate(obj%elementData(iel)%ptr%globalFaces, tFaces)
    CALL Reallocate(obj%elementData(iel)%ptr%faceOrient, 1_I4B, tFaces)

    DO iface = 1, tFaces

      face0 = obj%elementData(iel)%ptr%globalNodes(localFaces(:, iface))

      sorted_face = Sort(face0)

      IF (face0(1) .GT. face0(2)) THEN
        obj%elementData(iel)%ptr%faceOrient(1, iface) = -1_INT8
      ELSE
        obj%elementData(iel)%ptr%faceOrient(1, iface) = 1_INT8
      END IF

      ! search the bucket of the smaller node
      faceId = head(sorted_face(1))
      DO
        IF (faceId .EQ. 0) EXIT
        IF (faceNode2(faceId) .EQ. sorted_face(2)) EXIT
        faceId = nextFace(faceId)
      END DO

      ! new face
      IF (faceId .EQ. 0) THEN
        totalFaces = totalFaces + 1
        faceId = totalFaces
        faceNode2(faceId) = sorted_face(2)
        nextFace(faceId) = head(sorted_face(1))
        head(sorted_face(1)) = faceId
      END IF

      obj%elementData(iel)%ptr%globalFaces(iface) = faceId

    END DO

  END DO loop1

  obj%tFaces = totalFaces

  DEALLOCATE (head, nextFace, faceNode2)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif

END SUBROUTINE InitiateFaceConnectivity2D

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE InitiateFaceConnectivity3D(obj)
  CLASS(AbstractMesh_), INTENT(INOUT) :: obj

  ! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "InitiateFaceConnectivity3D()"
#endif
  INTEGER(I4B) :: tElements, iel, elemType, tFaces, &
    localFaces(4_I4B, TypeRefelemOpt%maxFaces), face0(4), sorted_face(4), &
    tNodes, tsize1, tsize2, iface, faceElemType(TypeRefelemOpt%maxFaces), &
    tFaceNodes(TypeRefelemOpt%maxFaces), aint, faceOrient(3_I4B)
  LOGICAL(LGT) :: isok
  TYPE(FaceDataBinaryTree_) :: faceTree
  TYPE(FaceData_) :: faceValue
  TYPE(FaceData_), POINTER :: facePtr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  isok = obj%isFaceConnectivityInitiated
  IF (isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

#ifdef DEBUG_VER
  isok = ALLOCATED(obj%elementData)
  CALL AssertError1(isok, myName, "obj%elementData is not allocated.")
#endif

  tElements = obj%GetTotalElements()

  CALL faceTree%Initiate()

  obj%isFaceConnectivityInitiated = .TRUE.

  loop1: DO iel = 1, tElements

    isok = .NOT. obj%elementData(iel)%ptr%isActive
    IF (isok) CYCLE loop1

    elemType = obj%elementData(iel)%ptr%name

    CALL RefElemGetGeoParam(elemType=elemType, tFaces=tFaces, tNodes=tNodes, &
                            faceCon=localFaces, faceOpt=1_I4B, &
                            faceElemType=faceElemType, &
                            tFaceNodes=tFaceNodes, order=1_I4B)

    CALL Reallocate(obj%elementData(iel)%ptr%globalFaces, tFaces)
    CALL Reallocate(obj%elementData(iel)%ptr%faceOrient, 3_I4B, tFaces)

    DO iface = 1, tFaces

      aint = tFaceNodes(iface)

      face0(1:aint) = &
        obj%elementData(iel)%ptr%globalNodes(localFaces(1:aint, iface))

      isok = IsQuadrangle(faceElemType(iface))
      IF (isok) THEN
        CALL FaceShapeMetaData_Quadrangle(face=face0(1:aint), &
                                          sorted_face=sorted_face(1:aint), &
                                          faceOrient=faceOrient)
      ELSE
        CALL FaceShapeMetaData_Triangle(face=face0(1:aint), &
                                        sorted_face=sorted_face(1:aint), &
                                        faceOrient=faceOrient)
      END IF

      facePtr => FaceData_Pointer(sorted_face)

      tsize1 = faceTree%SIZE()
      CALL faceTree%Insert(facePtr)
      tsize2 = faceTree%SIZE()

      obj%elementData(iel)%ptr%faceOrient(:, iface) = &
        INT(faceOrient, KIND=INT8)
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

  END DO loop1

  CALL faceTree%DEALLOCATE()
  facePtr => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif

END SUBROUTINE InitiateFaceConnectivity3D

!----------------------------------------------------------------------------
!                                                             Include Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE FaceDataMethods
