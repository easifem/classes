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

MODULE ElemData_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE Display_Method, ONLY: Display
USE ReferenceElement_Method, ONLY: ElementName
IMPLICIT NONE
PRIVATE

PUBLIC :: ElemData_
PUBLIC :: Display
PUBLIC :: TypeElem
PUBLIC :: ElemDataDeallocate
PUBLIC :: ElemDataSet
PUBLIC :: ElemData_Pointer
PUBLIC :: ElemData_Deallocate
PUBLIC :: ElemData_Display
PUBLIC :: ElemData_lt
PUBLIC :: ElemData_eq
PUBLIC :: ElemData_SetID

INTEGER(I4B), PARAMETER, PUBLIC :: INTERNAL_ELEMENT = 1
INTEGER(I4B), PARAMETER, PUBLIC :: BOUNDARY_ELEMENT = -1
INTEGER(I4B), PARAMETER, PUBLIC :: DOMAIN_BOUNDARY_ELEMENT = -2
INTEGER(I4B), PARAMETER, PUBLIC :: GHOST_ELEMENT = -4

INTERFACE Display
  MODULE PROCEDURE ElemData_Display
END INTERFACE Display

INTERFACE ElemDataDeallocate
  MODULE PROCEDURE ElemData_Deallocate
END INTERFACE ElemDataDeallocate

!----------------------------------------------------------------------------
!                                                                 ElemData_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-26
! summary: Data type for storing element data

TYPE :: ElemData_
  INTEGER(I4B) :: globalElemNum = 0_I4B
    !! global element number
  INTEGER(I4B) :: localElemNum = 0_I4B
    !! local element number
  INTEGER(I4B) :: elementType = INTERNAL_ELEMENT
    !! BOUNDARY_ELEMENT: If the element contqains the boundary node
    !! it will be called the boundary element
    !! INTERNAL_ELEMENT: If the element does not contain the boundary node
    !! then it will be called the internal element
  INTEGER(I4B) :: name = 0
    !! This is name of the element
    !! It can be Triangle, Triangle3, Triangle6, etc.
  INTEGER(I4B), ALLOCATABLE :: globalNodes(:)
    !! nodes contained in the element, connectivity
  INTEGER(I4B), ALLOCATABLE :: globalElements(:)
    !! Contains the information about the element surrounding an element
    !! Lets us say that `globalElem1`, `globalElem2`, `globalElem3`
    !! surrounds a local element ielem (its global element number is
    !! globalElem), then
    !! - globalElements( [1,2,3] ) contains globalElem1, pFace, nFace
    !! - globalElements( [4,5,6] ) contains globalElem2, pFace, nFace
    !! - globalElements( [7,8,9] ) contains globalElem3, pFace, nFace.
    !! Here,
    !! - pFace is the local facet number of parent element
    !! globalElem (ielem) which is connected to the nFace of the neighbor
    !! element
    !! All element numbers are global element number
  INTEGER(I4B), ALLOCATABLE :: boundaryData(:)
    !! If `iel` is boundary element, then boudnaryData contains
    !! the local facet number of iel which concides with the
    !! mesh boundary.
    !! If an element contains the boundary node then it is considered
    !! as a boundary element.
    !! It may happen that a boundary element has no boundary face, in which
    !! case boundaryData will have zero size
END TYPE ElemData_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE ElemDataType_
  INTEGER(I4B) :: internal = INTERNAL_ELEMENT
  INTEGER(I4B) :: boundary = BOUNDARY_ELEMENT
  INTEGER(I4B) :: domainBoundary = DOMAIN_BOUNDARY_ELEMENT
  INTEGER(I4B) :: ghost = GHOST_ELEMENT
END TYPE ElemDataType_

TYPE(ElemDataType_), PARAMETER :: TypeElem = ElemDataType_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-26
! summary: Display a single instance of element data

SUBROUTINE ElemData_Display(obj, msg, unitno)
  TYPE(ElemData_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: msg
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno

  CALL Display(TRIM(msg), unitno=unitno)
  CALL Display(obj%globalElemNum, msg="globalElemNum: ", unitno=unitno)
  CALL Display(obj%localElemNum, msg="localElemNum: ", unitno=unitno)
  CALL Display(ElemData_ElemType2String(obj%elementType), "elementType: ",  &
    & unitno=unitno)
  CALL Display(ElementName(obj%name), "elementName: ", unitno=unitno)

  ! globalNodes
  IF (ALLOCATED(obj%globalNodes)) THEN
    CALL Display(obj%globalNodes, msg="globalNodes: ", unitno=unitno)
  END IF

  ! globalElements
  IF (ALLOCATED(obj%globalElements)) THEN
    CALL Display(obj%globalElements, msg="globalElements: ", unitno=unitno)
  END IF

  ! boundaryData
  IF (ALLOCATED(obj%boundaryData)) THEN
    CALL Display(obj%boundaryData, msg="boundaryData: ", unitno=unitno)
  END IF
END SUBROUTINE ElemData_Display

!----------------------------------------------------------------------------
!                                                   ElemData_ElemType2String
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-29
! summary:  convert elementType to name

FUNCTION ElemData_ElemType2String(elementType) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: elementType
  CHARACTER(:), ALLOCATABLE :: ans

  SELECT CASE (elementType)
  CASE (INTERNAL_ELEMENT)
    ans = "INTERNAL_ELEMENT"
  CASE (BOUNDARY_ELEMENT)
    ans = "BOUNDARY_ELEMENT"
  CASE (DOMAIN_BOUNDARY_ELEMENT)
    ans = "DOMAIN_BOUNDARY_ELEMENT"
  CASE (GHOST_ELEMENT)
    ans = "GHOST_ELEMENT"
  CASE DEFAULT
    ans = "NONE"
  END SELECT
END FUNCTION ElemData_ElemType2String

!----------------------------------------------------------------------------
!                                                         ElemDataDeallocate
!----------------------------------------------------------------------------

SUBROUTINE ElemData_Deallocate(obj)
  TYPE(ElemData_), INTENT(INOUT) :: obj
  obj%globalElemNum = 0
  obj%localElemNum = 0
  obj%elementType = INTERNAL_ELEMENT
  IF (ALLOCATED(obj%globalNodes)) DEALLOCATE (obj%globalNodes)
  IF (ALLOCATED(obj%globalElements)) DEALLOCATE (obj%globalElements)
  IF (ALLOCATED(obj%boundaryData)) DEALLOCATE (obj%boundaryData)
END SUBROUTINE ElemData_Deallocate

!----------------------------------------------------------------------------
!                                                           ElemDataInitiate
!----------------------------------------------------------------------------

SUBROUTINE ElemDataSet(obj, globalElemNum, localElemNum,  &
  & elementType, globalNodes, globalElements, boundaryData)
  TYPE(ElemData_), INTENT(INOUT) :: obj
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalElemNum
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: localElemNum
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: elementType
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalNodes(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalElements(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: boundaryData(:)

  IF (PRESENT(globalElemNum)) obj%globalElemNum = globalElemNum
  IF (PRESENT(localElemNum)) obj%localElemNum = localElemNum
  IF (PRESENT(elementType)) obj%elementType = elementType
  IF (PRESENT(globalNodes)) obj%globalNodes = globalNodes
  IF (PRESENT(globalElements)) obj%globalElements = globalElements
  IF (PRESENT(boundaryData)) obj%boundaryData = boundaryData
END SUBROUTINE ElemDataSet

!----------------------------------------------------------------------------
!                                                          ElemData_Pointer
!----------------------------------------------------------------------------

FUNCTION ElemData_Pointer() RESULT(ans)
  CLASS(ElemData_), POINTER :: ans
  ALLOCATE (ElemData_ :: ans)
END FUNCTION ElemData_Pointer

!----------------------------------------------------------------------------
!                                                               ElemData_lt
!----------------------------------------------------------------------------

FUNCTION ElemData_lt(obj, obj2) RESULT(ans)
  TYPE(ElemData_), INTENT(IN) :: obj
  TYPE(ElemData_), INTENT(IN) :: obj2
  LOGICAL(LGT) :: ans
  ans = obj%globalElemNum .GT. obj2%globalElemNum
END FUNCTION ElemData_lt

!----------------------------------------------------------------------------
!                                                               ElemData_eq
!----------------------------------------------------------------------------

FUNCTION ElemData_eq(obj, obj2) RESULT(ans)
  TYPE(ElemData_), INTENT(IN) :: obj
  TYPE(ElemData_), INTENT(IN) :: obj2
  LOGICAL(LGT) :: ans
  ans = obj%globalElemNum .EQ. obj2%globalElemNum
END FUNCTION ElemData_eq

!----------------------------------------------------------------------------
!                                                         ElemData_SetID
!----------------------------------------------------------------------------

SUBROUTINE ElemData_SetID(obj, id)
  TYPE(ElemData_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: id
  obj%localElemNum = id
END SUBROUTINE ElemData_SetID

END MODULE ElemData_Class
