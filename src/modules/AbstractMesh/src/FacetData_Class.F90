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
USE GlobalData, ONLY: I4B
USE Display_Method, ONLY: Display
USE ElemData_Class, ONLY: INTERNAL_ELEMENT, BOUNDARY_ELEMENT,  &
  & DOMAIN_BOUNDARY_ELEMENT, GHOST_ELEMENT
IMPLICIT NONE
PRIVATE

PUBLIC :: InternalFacetData_
PUBLIC :: BoundaryFacetData_
PUBLIC :: InternalFacetData_Display
PUBLIC :: BoundaryFacetData_Display

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
