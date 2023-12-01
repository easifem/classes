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

SUBMODULE(Mesh_Class) ShapeDataMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            InitiateElemSD
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_initiateElemSD1

! main
obj%quadTypeForSpace = TRIM(quadTypeForSpace)
obj%continuityTypeForSpace = TRIM(continuityTypeForSpace)
obj%interpolTypeForSpace = TRIM(interpolTypeForSpace)
obj%orderSpace = orderSpace

CALL Initiate(obj=obj%quadForSpace, &
  & refelem=spaceElem, &
  & order=orderSpace, &
  & QuadratureType=quadTypeForSpace)

CALL Initiate(obj=obj%linSpaceElemSD, &
  & quad=obj%quadForSpace, &
  & refelem=linSpaceElem, &
  & ContinuityType=continuityTypeForSpace, &
  & InterpolType=interpolTypeForSpace)

CALL Initiate(obj=obj%spaceElemSD, &
  & quad=obj%quadForSpace, &
  & refelem=spaceElem, &
  & ContinuityType=continuityTypeForSpace, &
  & InterpolType=interpolTypeForSpace)

END PROCEDURE mesh_initiateElemSD1

!----------------------------------------------------------------------------
!                                                             InitiateElemSD
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_initiateElemSD2
!
CALL obj%InitiateElemSD( &
  & orderSpace=orderSpace, &
  & linSpaceElem=linSpaceElem, &
  & spaceElem=spaceElem, &
  & quadTypeForSpace=quadTypeForSpace, &
  & continuityTypeForSpace=continuityTypeForSpace, &
  & interpolTypeForSpace=interpolTypeForSpace, &
  & orderTime=orderTime, &
  & linTimeElem=linTimeElem, &
  & timeElem=timeElem, &
  & quadTypeForTime=quadTypeForTime, &
  & continuityTypeForTime=continuityTypeForTime, &
  & interpolTypeForTime=interpolTypeForTime)
!
CALL obj%InitiateElemSD(tvec=tvec)
!
END PROCEDURE mesh_initiateElemSD2

!----------------------------------------------------------------------------
!                                                             InitiateElemSD
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_initiateElemSD3
!
CALL obj%InitiateElemSD( &
  & orderSpace=orderSpace, &
  & linSpaceElem=linSpaceElem, &
  & spaceElem=spaceElem, &
  & quadTypeForSpace=quadTypeForSpace, &
  & continuityTypeForSpace=continuityTypeForSpace, &
  & interpolTypeForSpace=interpolTypeForSpace)
!
obj%quadTypeForTime = TRIM(quadTypeForTime)
obj%continuityTypeForTime = TRIM(continuityTypeForTime)
obj%interpolTypeForTime = TRIM(interpolTypeForTime)
obj%orderTime = orderTime
!
CALL Initiate(obj=obj%quadForTime, &
  & refelem=timeElem, &
  & order=orderTime, &
  & QuadratureType=quadTypeForTime)
!
CALL Initiate(obj=obj%linTimeElemSD, &
  & quad=obj%quadForTime, &
  & refelem=linTimeElem, &
  & ContinuityType=continuityTypeForTime, &
  & InterpolType=interpolTypeForTime)
!
CALL Initiate(obj=obj%timeElemSD, &
  & quad=obj%quadForTime, &
  & refelem=timeElem, &
  & ContinuityType=continuityTypeForTime, &
  & InterpolType=interpolTypeForTime)
!
END PROCEDURE mesh_initiateElemSD3

!----------------------------------------------------------------------------
!                                                             InitiateElemSD
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_initiateElemSD4
!
INTEGER(I4B) :: ii
!
CALL Set(obj=obj%timeElemSD, &
  & val=RESHAPE(tvec, [1, SIZE(tvec)]), &
  & N=obj%linTimeElemSD%N, &
  & dNdXi=obj%linTimeElemSD%dNdXi)
!
CALL Initiate(obj=obj%stelemsd, elemsd=obj%timeElemSD)
!
DO ii = 1, SIZE(obj%stelemsd)
  CALL Initiate(obj=obj%stelemsd(ii), &
    & quad=obj%quadForSpace, &
    & refelem=obj%spaceElemSD%refelem, &
    & ContinuityType=obj%continuityTypeForSpace%chars(), &
    & InterpolType=obj%interpolTypeForSpace%chars())
END DO
!
END PROCEDURE mesh_initiateElemSD4

!----------------------------------------------------------------------------
!                                                            InitiateElemSD
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_initiateFacetElemSD1
CHARACTER(*), PARAMETER :: myName = "mesh_initiateFacetElemSD1"
INTEGER(I4B) :: nn, ii
INTEGER(I4B), ALLOCATABLE :: faceNptrs(:)
REAL(DFP), ALLOCATABLE :: xijCell(:, :), quadPointsCell(:, :), &
  & points(:, :)

IF (.NOT. ALLOCATED(obj%facetElements)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'In Mesh_ object facetElements are not allocated!')
END IF

nn = SIZE(obj%facetElements)

IF (.NOT. ALLOCATED(obj%quadForFacet)) &
  & ALLOCATE (obj%quadForFacet(nn))

IF (.NOT. ALLOCATED(obj%quadForFacetCell)) &
  & ALLOCATE (obj%quadForFacetCell(nn))

IF (.NOT. ALLOCATED(obj%linFacetElemSD)) &
  & ALLOCATE (obj%linFacetElemSD(nn))

IF (.NOT. ALLOCATED(obj%linFacetCellElemSD)) &
  & ALLOCATE (obj%linFacetCellElemSD(nn))

IF (.NOT. ALLOCATED(obj%facetElemSD)) &
  & ALLOCATE (obj%facetElemSD(nn))

IF (.NOT. ALLOCATED(obj%facetCellElemSD)) &
  & ALLOCATE (obj%facetCellElemSD(nn))

obj%quadTypeForFacet = TRIM(quadTypeForSpace)
obj%continuityTypeForFacet = TRIM(continuityTypeForSpace)
obj%interpolTypeForFacet = TRIM(interpolTypeForSpace)
obj%orderFacet = orderSpace

DO ii = 1, nn
 
  CALL Initiate(obj=obj%quadForFacet(ii), &
    & refelem=spaceElem(ii), &
    & order=orderSpace, &
    & QuadratureType=quadTypeForSpace)
  
  CALL Initiate(obj=obj%linFacetElemSD(ii), &
    & quad=obj%quadForFacet(ii), &
    & refelem=linSpaceElem(ii), &
    & ContinuityType=continuityTypeForSpace, &
    & InterpolType=interpolTypeForSpace)
  
  CALL Initiate(obj=obj%facetElemSD(ii), &
    & quad=obj%quadForFacet(ii), &
    & refelem=spaceElem(ii), &
    & ContinuityType=continuityTypeForSpace, &
    & InterpolType=interpolTypeForSpace)
  
  faceNptrs = getConnectivity(obj%facetElements(ii))
  xijCell = LocalNodeCoord(obj%refelem)
  CALL getInterpolation(obj=obj%facetElemSD(ii), &
    & interpol=quadPointsCell, val=xijCell(:, faceNptrs))
  
  CALL Reallocate(points, obj%nsd + 1, SIZE(quadPointsCell, 2))
  points(1:obj%nsd, :) = quadPointsCell(1:obj%nsd, :)
  CALL Initiate(obj%quadForFacetCell(ii), points=points)
  
  CALL Initiate(obj=obj%linFacetCellElemSD(ii), &
    & quad=obj%quadForFacetCell(ii), &
    & refelem=obj%linSpaceElemSD%refelem, &
    & ContinuityType=continuityTypeForSpace, &
    & InterpolType=interpolTypeForSpace)
  
  CALL Initiate(obj=obj%facetCellElemSD(ii), &
    & quad=obj%quadForFacetCell(ii), &
    & refelem=obj%spaceElemSD%refelem, &
    & ContinuityType=continuityTypeForSpace, &
    & InterpolType=interpolTypeForSpace)
  
END DO

END PROCEDURE mesh_initiateFacetElemSD1

!----------------------------------------------------------------------------
!                                                             InitiateElemSD
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_initiateFacetElemSD2
!
CALL obj%InitiateFacetElemSD( &
  & orderSpace=orderSpace, &
  & linSpaceElem=linSpaceElem, &
  & spaceElem=spaceElem, &
  & quadTypeForSpace=quadTypeForSpace, &
  & continuityTypeForSpace=continuityTypeForSpace, &
  & interpolTypeForSpace=interpolTypeForSpace)
!
CALL obj%InitiateFacetElemSD(tvec=tvec)
!
END PROCEDURE mesh_initiateFacetElemSD2

!----------------------------------------------------------------------------
!                                                             InitiateElemSD
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_initiateFacetElemSD3
!
INTEGER(I4B) :: ii, jj, nn, mm
TYPE(STElemshapeData_), ALLOCATABLE :: stelemsd(:)
!
CALL Initiate(obj=stelemsd, elemsd=obj%timeElemSD)
!
mm = SIZE(stelemsd)
nn = SIZE(obj%facetElements)
!
IF (.NOT. ALLOCATED(obj%facetSTelemsd)) &
  & ALLOCATE (obj%facetSTelemsd(mm, nn))
!
DO jj = 1, nn
  !
  DO ii = 1, mm
    !
    obj%facetSTelemsd(ii, jj) = stelemsd(ii)
    !
    CALL Initiate( &
      & obj=obj%facetSTelemsd(ii, jj), &
      & quad=obj%quadForFacet(jj), &
      & refelem=obj%facetElemSD(jj)%refelem, &
      & ContinuityType=obj%continuityTypeForFacet%chars(), &
      & InterpolType=obj%interpolTypeForFacet%chars())
  END DO
  !
END DO
!
DEALLOCATE (stelemsd)
!
END PROCEDURE mesh_initiateFacetElemSD3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ShapeDataMethods
