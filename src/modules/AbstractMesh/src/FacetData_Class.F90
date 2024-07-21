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

MODULE FacetData_Class
USE GlobalData, ONLY: I4B, LGT
USE Display_Method, ONLY: Display
USE ElemData_Class, ONLY: INTERNAL_ELEMENT, BOUNDARY_ELEMENT,  &
  & DOMAIN_BOUNDARY_ELEMENT, GHOST_ELEMENT
IMPLICIT NONE
PRIVATE

PUBLIC :: FacetData_
PUBLIC :: FacetData_Display
PUBLIC :: FacetData_Display_filter
PUBLIC :: FacetData_Iselement
PUBLIC :: FacetData_GetParam
PUBLIC :: FacetData_SetParam

PUBLIC :: InternalFacetData_
PUBLIC :: InternalFacetData_Display

PUBLIC :: BoundaryFacetData_
PUBLIC :: BoundaryFacetData_Display

!----------------------------------------------------------------------------
!                                                                FacetData_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-26
! summary: Data storage for internal facets of mesh

TYPE FacetData_
  INTEGER(I4B) :: masterCellNumber = 0
    !! master cell nubmer
  INTEGER(I4B) :: slaveCellNumber = 0
    !! slave cell number
  INTEGER(I4B) :: masterLocalFacetID = 0
    !! local facet ID in master cell
  INTEGER(I4B) :: slaveLocalFacetID = 0
    !! slave facet ID in slave cell
  INTEGER(I4B) :: elementType = 0
  !! INTERNAL_ELEMENT
  !! BOUNDARY_ELEMENT
  !! DOMAIN_BOUNDARY_ELEMENT
END TYPE FacetData_

!----------------------------------------------------------------------------
!                                                         InternalFacetData_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-26
! summary: Data storage for internal facets of mesh

TYPE InternalFacetData_
  INTEGER(I4B) :: masterCellNumber = 0
  !! master cell nubmer
  INTEGER(I4B) :: slaveCellNumber = 0
  !! slave cell number
  INTEGER(I4B) :: masterLocalFacetID = 0
  !! local facet ID in master cell
  INTEGER(I4B) :: slaveLocalFacetID = 0
  !! slave facet ID in slave cell
END TYPE InternalFacetData_

!----------------------------------------------------------------------------
!                                                         BoundaryFacetData_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 May 2022
! summary: Data storage for Domain facet data
!
!# Introduction
!
! DomainFacet elements are those boundary elements which are located
! on the boundary of the domain. Ofcourse domainFacet elements are
! located on the mesh Boundary with only difference that these elements do
! not have slaveCellNumber

TYPE BoundaryFacetData_
  INTEGER(I4B) :: masterCellNumber = 0
  INTEGER(I4B) :: masterLocalFacetID = 0
  INTEGER(I4B) :: elementType = 0
END TYPE BoundaryFacetData_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-14
! summary: Display the instance of InternalFacetData

SUBROUTINE FacetData_SetParam(obj, elementType, masterCellNumber, &
                       slaveCellNumber, masterLocalFacetID, slaveLocalFacetID)
  CLASS(FacetData_), INTENT(INOUT) :: obj
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: elementType
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: masterCellNumber
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: slaveCellNumber
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: masterLocalFacetID
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: slaveLocalFacetID

  IF (PRESENT(elementType)) obj%elementType = elementType
  IF (PRESENT(masterCellNumber)) obj%masterCellNumber = masterCellNumber
  IF (PRESENT(slaveCellNumber)) obj%slaveCellNumber = slaveCellNumber
  IF (PRESENT(masterLocalFacetID)) obj%masterLocalFacetID = masterLocalFacetID
  IF (PRESENT(slaveLocalFacetID)) obj%slaveLocalFacetID = slaveLocalFacetID
END SUBROUTINE FacetData_SetParam

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-14
! summary: Display the instance of InternalFacetData

SUBROUTINE FacetData_GetParam(obj, elementType, masterCellNumber, &
                       slaveCellNumber, masterLocalFacetID, slaveLocalFacetID)
  CLASS(FacetData_), INTENT(IN) :: obj
  INTEGER(I4B), OPTIONAL, INTENT(OUT) :: elementType
  INTEGER(I4B), OPTIONAL, INTENT(OUT) :: masterCellNumber
  INTEGER(I4B), OPTIONAL, INTENT(OUT) :: slaveCellNumber
  INTEGER(I4B), OPTIONAL, INTENT(OUT) :: masterLocalFacetID
  INTEGER(I4B), OPTIONAL, INTENT(OUT) :: slaveLocalFacetID

  IF (PRESENT(elementType)) elementType = obj%elementType
  IF (PRESENT(masterCellNumber)) masterCellNumber = obj%masterCellNumber
  IF (PRESENT(slaveCellNumber)) slaveCellNumber = obj%slaveCellNumber
  IF (PRESENT(masterLocalFacetID)) masterLocalFacetID = obj%masterLocalFacetID
  IF (PRESENT(slaveLocalFacetID)) slaveLocalFacetID = obj%slaveLocalFacetID
END SUBROUTINE FacetData_GetParam

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-14
! summary: Display the instance of InternalFacetData

SUBROUTINE FacetData_Display(obj, msg, unitno)
  CLASS(FacetData_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: msg
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno

  CALL Display(TRIM(msg), unitno=unitno)

  SELECT CASE (obj%elementType)
  CASE (INTERNAL_ELEMENT)
    CALL Display("elementType: INTERNAL_FACET_ELEMENT", unitno=unitno)
  CASE (BOUNDARY_ELEMENT)
    CALL Display("elementType: BOUNDARY_FACET_ELEMENT", unitno=unitno)
  CASE (DOMAIN_BOUNDARY_ELEMENT)
    CALL Display("elementType: DOMAIN_FACET_ELEMENT", unitno=unitno)
  CASE DEFAULT
    CALL Display("elementType: UNKNOWN_FACET_ELEMENT", unitno=unitno)
  END SELECT

  CALL Display(obj%masterCellNumber, msg="masterCellNumber: ", &
    & unitno=unitno)
  CALL Display(obj%slaveCellNumber, msg="slaveCellNumber: ", &
    & unitno=unitno)
  CALL Display(obj%masterlocalFacetID, msg="masterlocalFacetID: ", &
    & unitno=unitno)
  CALL Display(obj%slavelocalFacetID, msg="slavelocalFacetID: ", &
    & unitno=unitno)

END SUBROUTINE FacetData_Display

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-14
! summary: Display the instance of InternalFacetData

SUBROUTINE FacetData_Display_Filter(obj, filter, msg, unitno)
  CLASS(FacetData_), INTENT(IN) :: obj
  INTEGER(I4B), INTENT(IN) :: filter
  CHARACTER(*), INTENT(IN) :: msg
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno

  SELECT CASE (filter)
  CASE (INTERNAL_ELEMENT)
    CALL FacetData_Display(obj, msg, unitno)
  CASE (BOUNDARY_ELEMENT)
    CALL FacetData_Display(obj, msg, unitno)
  CASE (DOMAIN_BOUNDARY_ELEMENT)
    CALL FacetData_Display(obj, msg, unitno)
  END SELECT

END SUBROUTINE FacetData_Display_Filter

!----------------------------------------------------------------------------
!                                                       IsElement@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-14
! summary: Display the instance of InternalFacetData

PURE ELEMENTAL FUNCTION FacetData_Iselement(obj, filter) RESULT(ans)
  CLASS(FacetData_), INTENT(IN) :: obj
  INTEGER(I4B), INTENT(IN) :: filter
  LOGICAL(LGT) :: ans
  ans = obj%elementType .EQ. filter
END FUNCTION FacetData_Iselement

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 April 2022
! summary: Display the instance of InternalFacetData

SUBROUTINE InternalFacetData_Display(obj, msg, unitno)
  CLASS(InternalFacetData_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: msg
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno

  CALL Display(TRIM(msg), unitno=unitno)
  CALL Display("elementType: INTERNAL_ELEMENT", unitno=unitno)
  CALL Display(obj%masterCellNumber, msg="masterCellNumber: ", &
    & unitno=unitno)
  CALL Display(obj%slaveCellNumber, msg="slaveCellNumber: ", &
    & unitno=unitno)
  CALL Display(obj%masterlocalFacetID, msg="masterlocalFacetID: ", &
    & unitno=unitno)
  CALL Display(obj%slavelocalFacetID, msg="slavelocalFacetID: ", &
    & unitno=unitno)
END SUBROUTINE InternalFacetData_Display

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 April 2022
! summary: Display the instance of DomainFacetData_

SUBROUTINE BoundaryFacetData_Display(obj, msg, unitno)
  CLASS(BoundaryFacetData_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: msg
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno

  CALL Display(TRIM(msg), unitno=unitno)
  SELECT CASE (obj%elementType)
  CASE (INTERNAL_ELEMENT)
    CALL Display("elementType: INTERNAL_ELEMENT", unitno=unitno)
  CASE (BOUNDARY_ELEMENT)
    CALL Display("elementType: BOUNDARY_ELEMENT", unitno=unitno)
  CASE (DOMAIN_BOUNDARY_ELEMENT)
    CALL Display("elementType: DOMAIN_BOUNDARY_ELEMENT", unitno=unitno)
  END SELECT
  CALL Display(obj%masterCellNumber, msg="masterCellNumber: ", &
    & unitno=unitno)
  CALL Display(obj%masterLocalFacetID, msg="masterlocalFacetID: ", &
    & unitno=unitno)
END SUBROUTINE BoundaryFacetData_Display

END MODULE FacetData_Class
