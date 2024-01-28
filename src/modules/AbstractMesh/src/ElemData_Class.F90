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
USE GlobalData, ONLY: I4B
USE Display_Method, ONLY: Display
IMPLICIT NONE
PRIVATE

PUBLIC :: ElemData_
PUBLIC :: ElemData_Display
PUBLIC :: TypeElem

INTEGER(I4B), PARAMETER, PUBLIC :: INTERNAL_ELEMENT = 1
INTEGER(I4B), PARAMETER, PUBLIC :: BOUNDARY_ELEMENT = -1
INTEGER(I4B), PARAMETER, PUBLIC :: DOMAIN_BOUNDARY_ELEMENT = -2
INTEGER(I4B), PARAMETER, PUBLIC :: GHOST_ELEMENT = -4

!----------------------------------------------------------------------------
!                                                                 ElemData_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-26
! summary: Data type for storing element data

TYPE :: ElemData_
  INTEGER(I4B) :: globalElemNum = 0
    !! global element number
  INTEGER(I4B) :: localElemNum = 0
    !! local element number
  INTEGER(I4B) :: elementType = INTERNAL_ELEMENT
    !! BOUNDARY_ELEMENT: If the element contqains the boundary node
    !! it will be called the boundary element
    !! INTERNAL_ELEMENT: If the element does not contain the boundary node
    !! then it will be called the internal element
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

  SELECT CASE (obj%elementType)
  CASE (INTERNAL_ELEMENT)
    CALL Display("elementType=INTERNAL_ELEMENT", unitno=unitno)
  CASE (BOUNDARY_ELEMENT)
    CALL Display("elementType=BOUNDARY_ELEMENT", unitno=unitno)
  CASE (DOMAIN_BOUNDARY_ELEMENT)
    CALL Display("elementType=DOMAIN_BOUNDARY_ELEMENT", unitno=unitno)
  CASE (GHOST_ELEMENT)
    CALL Display("elementType=GHOST_ELEMENT", unitno=unitno)
  END SELECT

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

END MODULE ElemData_Class
