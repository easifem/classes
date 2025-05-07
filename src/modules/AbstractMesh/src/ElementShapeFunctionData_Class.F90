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

MODULE ElementShapeFunctionData_Class
USE GlobalData, ONLY: I4B, LGT, DFP
USE BaseType, ONLY: QuadraturePoint_, ElemShapeData_, STElemShapeData_, &
                    ReferenceElement_, ReferenceLine_
USE String_Class
USE QuadraturePoint_Method
USE ElemshapeData_Method
USE ReferenceElement_Method
USE ReallocateUtility
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE
PRIVATE

CHARACTER(*), PARAMETER :: modName = "ElementShapeFunctionData_Class"

!----------------------------------------------------------------------------
!                                                 ElementShapeFunctionData_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-26
! summary:  Entire element shape function data

TYPE ElementShapeFunctionData_
  INTEGER(I4B) :: nsd
  !! number of spatial dimension
  TYPE(ReferenceElement_) :: refelem
    !! Reference element of the mesh (spatial)
  TYPE(ReferenceElement_), ALLOCATABLE :: facetElements(:)
    !! Facet Elements in the reference element

  ! Following variables are required during processing time
  TYPE(QuadraturePoint_), PUBLIC :: quadForTime
    !! quadrature point for time domain #STFEM
  TYPE(ElemshapeData_), PUBLIC :: linTimeElemSD
    !! Element shape data on linear time element #STFEM
  TYPE(ElemshapeData_), PUBLIC :: timeElemSD
    !! Element shape data on time element #STFEM
  TYPE(String) :: quadTypeForTime
    !! quadrature type for time
  TYPE(String) :: continuityTypeForTime
    !! continuity of base function for time
  TYPE(String) :: interpolTypeForTime
    !! interpolation of base function for time
  INTEGER(I4B) :: orderTime
    !! order for time

  ! space (cell)
  TYPE(QuadraturePoint_), PUBLIC :: quadForSpace
    !! quadrature point for space
  TYPE(ElemshapeData_), PUBLIC :: linSpaceElemSD
    !! Element shape data on linear space (simplex) element
  TYPE(ElemshapeData_), PUBLIC :: spaceElemSD
    !! Element shape data on space element
  TYPE(STElemshapeData_), ALLOCATABLE, PUBLIC :: stelemsd(:)
    !! Element shape data on space-time element
  TYPE(String) :: quadTypeForSpace
    !! quadrature type for space
  TYPE(String) :: continuityTypeForSpace
    !! continuity of base function for space
  TYPE(String) :: interpolTypeForSpace
    !! interoplation type of base function for space
  INTEGER(I4B) :: orderSpace
    !! order for space

  ! space (facets)
  TYPE(QuadraturePoint_), ALLOCATABLE, PUBLIC :: quadForFacet(:)
    !! quadrature point for facet elements
  TYPE(QuadraturePoint_), ALLOCATABLE, PUBLIC :: quadForFacetCell(:)
    !! quadrature point for facet-cell elements
  TYPE(ElemshapeData_), ALLOCATABLE, PUBLIC :: linFacetElemSD(:)
    !! Element shape data on linear facet (simplex) element
  TYPE(ElemshapeData_), ALLOCATABLE, PUBLIC :: linFacetCellElemSD(:)
    !! Element shape data on linear facet (simplex) cell element
  TYPE(ElemshapeData_), ALLOCATABLE, PUBLIC :: facetElemSD(:)
    !! Element shape data on facet element
  TYPE(ElemshapeData_), ALLOCATABLE, PUBLIC :: facetCellElemSD(:)
    !! Element shape data on facet cell element
  TYPE(String) :: quadTypeForFacet
    !! quadrature type for facet element
  TYPE(String) :: continuityTypeForFacet
    !! continuity of base function for facet element
  TYPE(String) :: interpolTypeForFacet
    !! interoplation type of base function for facet element
  INTEGER(I4B) :: orderFacet
    !! order for facet element
  TYPE(STElemshapeData_), ALLOCATABLE, PUBLIC :: facetSTelemsd(:, :)
    !! Element shape data on facet element

CONTAINS
  PRIVATE

  ! @ShapeDataMethods
  PROCEDURE, PASS(obj) :: InitiateElemSD1 => obj_initiateElemSD1
  PROCEDURE, PASS(obj) :: InitiateElemSD2 => obj_initiateElemSD2
  PROCEDURE, PASS(obj) :: InitiateElemSD3 => obj_initiateElemSD3
  PROCEDURE, PASS(obj) :: InitiateElemSD4 => obj_initiateElemSD4
  GENERIC, PUBLIC :: InitiateElemSD => &
    & InitiateElemSD1, &
    & InitiateElemSD2, &
    & InitiateElemSD3, &
    & InitiateElemSD4

  PROCEDURE, PASS(obj) :: InitiateFacetElemSD1 => obj_initiateFacetElemSD1
  PROCEDURE, PASS(obj) :: InitiateFacetElemSD2 => obj_initiateFacetElemSD2
  PROCEDURE, PASS(obj) :: InitiateFacetElemSD3 => obj_initiateFacetElemSD3
  GENERIC, PUBLIC :: InitiateFacetElemSD => &
    & InitiateFacetElemSD1, &
    & InitiateFacetElemSD2, &
    & InitiateFacetElemSD3
  !! Initiating local shape data for mesh
END TYPE ElementShapeFunctionData_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                            InitiateElemSD@ShapeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: Sets the local shape data for the mesh
!
!# Introduction
!
! This routine sets the local shape data in space (linSpaceElemSD and
! spaceElemSD) for the mesh. It also creates the quadrature points in space.

SUBROUTINE obj_InitiateElemSD1(obj, &
  & orderSpace,  &
  & linSpaceElem, &
  & spaceElem, &
  & quadTypeForSpace, &
  & continuityTypeForSpace, &
  & interpolTypeForSpace)
  CLASS(ElementShapeFunctionData_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: orderSpace
      !! integrand order in space
  CLASS(ReferenceElement_), TARGET, INTENT(IN) :: linSpaceElem
      !! linear (simplex) space element
  CLASS(ReferenceElement_), TARGET, INTENT(IN) :: spaceElem
      !! space element
  CHARACTER(*), INTENT(IN) :: quadTypeForSpace
      !! quadrature for space
  CHARACTER(*), INTENT(IN) :: continuityTypeForSpace
      !! continuity for base in space
  CHARACTER(*), INTENT(IN) :: interpolTypeForSpace
      !! interpolation type for base in space

  ! internal variables
  obj%quadTypeForSpace = quadTypeForSpace
  obj%continuityTypeForSpace = continuityTypeForSpace
  obj%interpolTypeForSpace = interpolTypeForSpace
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
END SUBROUTINE obj_InitiateElemSD1

!----------------------------------------------------------------------------
!                                            InitiateElemSD@ShapeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: Sets the local shape data for the mesh

SUBROUTINE obj_InitiateElemSD2(obj, &
  & orderSpace,  &
  & linSpaceElem, &
  & spaceElem, &
  & quadTypeForSpace, &
  & continuityTypeForSpace, &
  & interpolTypeForSpace, &
  & orderTime, &
  & linTimeElem, &
  & timeElem, &
  & quadTypeForTime, &
  & continuityTypeForTime, &
  & interpolTypeForTime, &
  & tvec)
  CLASS(ElementShapeFunctionData_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: orderSpace
      !! integrand order in space
  CLASS(ReferenceElement_), TARGET, INTENT(IN) :: linSpaceElem
      !! linear space element
  CLASS(ReferenceElement_), TARGET, INTENT(IN) :: spaceElem
      !! space element
  CHARACTER(*), INTENT(IN) :: quadTypeForSpace
      !! quadrature type for space
  CHARACTER(*), INTENT(IN) :: continuityTypeForSpace
      !! continuity type of base in space
  CHARACTER(*), INTENT(IN) :: interpolTypeForSpace
      !! interpol type of base in space
  INTEGER(I4B), INTENT(IN) :: orderTime
      !! integrand order in time
  TYPE(ReferenceLine_), INTENT(IN) :: linTimeElem
      !! linear time element
  TYPE(ReferenceLine_), INTENT(IN) :: timeElem
      !! time element
  CHARACTER(*), INTENT(IN) :: quadTypeForTime
      !! quadrature type of base in time
  CHARACTER(*), INTENT(IN) :: continuityTypeForTime
      !! continuity type of base in time
  CHARACTER(*), INTENT(IN) :: interpolTypeForTime
      !! interpol type of base in time
  REAL(DFP), INTENT(IN) :: tvec(:)

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

  CALL obj%InitiateElemSD(tvec=tvec)
END SUBROUTINE obj_InitiateElemSD2

!----------------------------------------------------------------------------
!                                            InitiateElemSD@ShapeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: Sets the local shape data for the mesh

SUBROUTINE obj_InitiateElemSD3(obj, &
  & orderSpace,  &
  & linSpaceElem, &
  & spaceElem, &
  & quadTypeForSpace, &
  & continuityTypeForSpace, &
  & interpolTypeForSpace, &
  & orderTime, &
  & linTimeElem, &
  & timeElem, &
  & quadTypeForTime, &
  & continuityTypeForTime, &
  & interpolTypeForTime)
  CLASS(ElementShapeFunctionData_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: orderSpace
      !! integrand order in space
  CLASS(ReferenceElement_), TARGET, INTENT(IN) :: linSpaceElem
      !! linear space element
  CLASS(ReferenceElement_), TARGET, INTENT(IN) :: spaceElem
      !! space element
  CHARACTER(*), INTENT(IN) :: quadTypeForSpace
      !! quadrature type of base in space
  CHARACTER(*), INTENT(IN) :: continuityTypeForSpace
      !! continuity type of base in space
  CHARACTER(*), INTENT(IN) :: interpolTypeForSpace
      !! interpolation type of base in space
  INTEGER(I4B), INTENT(IN) :: orderTime
      !! integrand order in time
  TYPE(ReferenceLine_), INTENT(IN) :: linTimeElem
      !! linear time element
  TYPE(ReferenceLine_), INTENT(IN) :: timeElem
      !! time element
  CHARACTER(*), INTENT(IN) :: quadTypeForTime
      !! quadrature type of base in time
  CHARACTER(*), INTENT(IN) :: continuityTypeForTime
      !! continuity type of base in time
  CHARACTER(*), INTENT(IN) :: interpolTypeForTime
      !! interpolation type of base in time

  CALL obj%InitiateElemSD( &
    & orderSpace=orderSpace, &
    & linSpaceElem=linSpaceElem, &
    & spaceElem=spaceElem, &
    & quadTypeForSpace=quadTypeForSpace, &
    & continuityTypeForSpace=continuityTypeForSpace, &
    & interpolTypeForSpace=interpolTypeForSpace)

  obj%quadTypeForTime = quadTypeForTime
  obj%continuityTypeForTime = continuityTypeForTime
  obj%interpolTypeForTime = interpolTypeForTime
  obj%orderTime = orderTime

  CALL Initiate(obj=obj%quadForTime, &
    & refelem=timeElem, &
    & order=orderTime, &
    & QuadratureType=quadTypeForTime)

  CALL Initiate(obj=obj%linTimeElemSD, &
    & quad=obj%quadForTime, &
    & refelem=linTimeElem, &
    & ContinuityType=continuityTypeForTime, &
    & InterpolType=interpolTypeForTime)

  CALL Initiate(obj=obj%timeElemSD, &
    & quad=obj%quadForTime, &
    & refelem=timeElem, &
    & ContinuityType=continuityTypeForTime, &
    & InterpolType=interpolTypeForTime)

END SUBROUTINE obj_InitiateElemSD3

!----------------------------------------------------------------------------
!                                            InitiateElemSD@ShapeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: Sets the local shape data for the mesh

SUBROUTINE obj_InitiateElemSD4(obj, tvec)
  CLASS(ElementShapeFunctionData_), INTENT(INOUT) :: obj
  REAL(DFP), INTENT(IN) :: tvec(:)

  INTEGER(I4B) :: ii
  CHARACTER(*), PARAMETER :: myName = "obj_InitiateElemSD4()"

  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[WIP ERROR] :: This routine is under development')

  ! CALL Set(obj=obj%timeElemSD, &
  !   & val=RESHAPE(tvec, [1, SIZE(tvec)]), &
  !   & N=obj%linTimeElemSD%N, &
  !   & dNdXi=obj%linTimeElemSD%dNdXi)
  !
  ! CALL Initiate(obj=obj%stelemsd, elemsd=obj%timeElemSD)
  !
  ! DO ii = 1, SIZE(obj%stelemsd)
  !   CALL Initiate(obj=obj%stelemsd(ii), &
  !     & quad=obj%quadForSpace, &
  !     & refelem=obj%spaceElemSD%refelem, &
  !     & ContinuityType=obj%continuityTypeForSpace%chars(), &
  !     & InterpolType=obj%interpolTypeForSpace%chars())
  ! END DO

END SUBROUTINE obj_InitiateElemSD4

!----------------------------------------------------------------------------
!                                            InitiateElemSD@ShapeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! summary: Sets the local shape data for the mesh

SUBROUTINE obj_InitiateFacetElemSD1(obj, &
  & orderSpace,  &
  & linSpaceElem, &
  & spaceElem, &
  & quadTypeForSpace, &
  & continuityTypeForSpace, &
  & interpolTypeForSpace)
  CLASS(ElementShapeFunctionData_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: orderSpace
      !! integrand order in space
  CLASS(ReferenceElement_), TARGET, INTENT(IN) :: linSpaceElem(:)
      !! linear (simplex) space element for each face
  CLASS(ReferenceElement_), TARGET, INTENT(IN) :: spaceElem(:)
      !! space element for each face
  CHARACTER(*), INTENT(IN) :: quadTypeForSpace
      !! quadrature for space
  CHARACTER(*), INTENT(IN) :: continuityTypeForSpace
      !! continuity for base in space
  CHARACTER(*), INTENT(IN) :: interpolTypeForSpace
      !! interpolation type for base in space

  CHARACTER(*), PARAMETER :: myName = "obj_initiateFacetElemSD1()"

  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[WIP ERROR] :: This routine is under development')

  ! INTEGER(I4B) :: nn, ii
  ! INTEGER(I4B), ALLOCATABLE :: faceNptrs(:)
  ! REAL(DFP), ALLOCATABLE :: xijCell(:, :), quadPointsCell(:, :), &
  !   & points(:, :)
  !
  ! IF (.NOT. ALLOCATED(obj%facetElements)) THEN
  !   CALL e%raiseError(modName//'::'//myName//' - '// &
  !     & 'In Mesh_ object facetElements are not allocated!')
  ! END IF
  !
  ! nn = SIZE(obj%facetElements)
  !
  ! IF (.NOT. ALLOCATED(obj%quadForFacet)) &
  !   & ALLOCATE (obj%quadForFacet(nn))
  !
  ! IF (.NOT. ALLOCATED(obj%quadForFacetCell)) &
  !   & ALLOCATE (obj%quadForFacetCell(nn))
  !
  ! IF (.NOT. ALLOCATED(obj%linFacetElemSD)) &
  !   & ALLOCATE (obj%linFacetElemSD(nn))
  !
  ! IF (.NOT. ALLOCATED(obj%linFacetCellElemSD)) &
  !   & ALLOCATE (obj%linFacetCellElemSD(nn))
  !
  ! IF (.NOT. ALLOCATED(obj%facetElemSD)) &
  !   & ALLOCATE (obj%facetElemSD(nn))
  !
  ! IF (.NOT. ALLOCATED(obj%facetCellElemSD)) &
  !   & ALLOCATE (obj%facetCellElemSD(nn))
  !
  ! obj%quadTypeForFacet = TRIM(quadTypeForSpace)
  ! obj%continuityTypeForFacet = TRIM(continuityTypeForSpace)
  ! obj%interpolTypeForFacet = TRIM(interpolTypeForSpace)
  ! obj%orderFacet = orderSpace
  !
  ! DO ii = 1, nn
  !
  !   CALL Initiate(obj=obj%quadForFacet(ii), &
  !     & refelem=spaceElem(ii), &
  !     & order=orderSpace, &
  !     & QuadratureType=quadTypeForSpace)
  !
  !   CALL Initiate(obj=obj%linFacetElemSD(ii), &
  !     & quad=obj%quadForFacet(ii), &
  !     & refelem=linSpaceElem(ii), &
  !     & ContinuityType=continuityTypeForSpace, &
  !     & InterpolType=interpolTypeForSpace)
  !
  !   CALL Initiate(obj=obj%facetElemSD(ii), &
  !     & quad=obj%quadForFacet(ii), &
  !     & refelem=spaceElem(ii), &
  !     & ContinuityType=continuityTypeForSpace, &
  !     & InterpolType=interpolTypeForSpace)
  !
  !   faceNptrs = getConnectivity(obj%facetElements(ii))
  !   xijCell = LocalNodeCoord(obj%refelem)
  !   CALL getInterpolation(obj=obj%facetElemSD(ii), &
  !     & interpol=quadPointsCell, val=xijCell(:, faceNptrs))
  !
  !   CALL Reallocate(points, obj%nsd + 1, SIZE(quadPointsCell, 2))
  !   points(1:obj%nsd, :) = quadPointsCell(1:obj%nsd, :)
  !   CALL Initiate(obj%quadForFacetCell(ii), points=points)
  !
  !   CALL Initiate(obj=obj%linFacetCellElemSD(ii), &
  !     & quad=obj%quadForFacetCell(ii), &
  !     & refelem=obj%linSpaceElemSD%refelem, &
  !     & ContinuityType=continuityTypeForSpace, &
  !     & InterpolType=interpolTypeForSpace)
  !
  !   CALL Initiate(obj=obj%facetCellElemSD(ii), &
  !     & quad=obj%quadForFacetCell(ii), &
  !     & refelem=obj%spaceElemSD%refelem, &
  !     & ContinuityType=continuityTypeForSpace, &
  !     & InterpolType=interpolTypeForSpace)
  !
  ! END DO

END SUBROUTINE obj_InitiateFacetElemSD1

!----------------------------------------------------------------------------
!                                            InitiateElemSD@ShapeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Sets the local shape data for the mesh

SUBROUTINE obj_InitiateFacetElemSD2(obj, &
  & orderSpace,  &
  & linSpaceElem, &
  & spaceElem, &
  & quadTypeForSpace, &
  & continuityTypeForSpace, &
  & interpolTypeForSpace, &
  & orderTime, &
  & linTimeElem, &
  & timeElem, &
  & quadTypeForTime, &
  & continuityTypeForTime, &
  & interpolTypeForTime, &
  & tvec)
  CLASS(ElementShapeFunctionData_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: orderSpace
      !! integrand order in space
  CLASS(ReferenceElement_), TARGET, INTENT(IN) :: linSpaceElem(:)
      !! linear space element for each face
  CLASS(ReferenceElement_), TARGET, INTENT(IN) :: spaceElem(:)
      !! space element for each face
  CHARACTER(*), INTENT(IN) :: quadTypeForSpace
      !! quadrature type for space
  CHARACTER(*), INTENT(IN) :: continuityTypeForSpace
      !! continuity type of base in space
  CHARACTER(*), INTENT(IN) :: interpolTypeForSpace
      !! interpol type of base in space
  INTEGER(I4B), INTENT(IN) :: orderTime
      !! integrand order in time
  TYPE(ReferenceLine_), INTENT(IN) :: linTimeElem
      !! linear time element
  TYPE(ReferenceLine_), INTENT(IN) :: timeElem
      !! time element
  CHARACTER(*), INTENT(IN) :: quadTypeForTime
      !! quadrature type of base in time
  CHARACTER(*), INTENT(IN) :: continuityTypeForTime
      !! continuity type of base in time
  CHARACTER(*), INTENT(IN) :: interpolTypeForTime
      !! interpol type of base in time
  REAL(DFP), INTENT(IN) :: tvec(:)

  CALL obj%InitiateFacetElemSD( &
    & orderSpace=orderSpace, &
    & linSpaceElem=linSpaceElem, &
    & spaceElem=spaceElem, &
    & quadTypeForSpace=quadTypeForSpace, &
    & continuityTypeForSpace=continuityTypeForSpace, &
    & interpolTypeForSpace=interpolTypeForSpace)

  CALL obj%InitiateFacetElemSD(tvec=tvec)

END SUBROUTINE obj_InitiateFacetElemSD2

!----------------------------------------------------------------------------
!                                            InitiateElemSD@ShapeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: Sets the local shape data for the mesh

SUBROUTINE obj_InitiateFacetElemSD3(obj, tvec)
  CLASS(ElementShapeFunctionData_), INTENT(INOUT) :: obj
  REAL(DFP), INTENT(IN) :: tvec(:)

  CHARACTER(*), PARAMETER :: myName = "obj_InitiateFacetElemSD3()"

  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[WIP ERROR] :: This routine is under development')

  ! INTEGER(I4B) :: ii, jj, nn, mm
  ! TYPE(STElemshapeData_), ALLOCATABLE :: stelemsd(:)
  !
  ! CALL Initiate(obj=stelemsd, elemsd=obj%timeElemSD)
  !
  ! mm = SIZE(stelemsd)
  ! nn = SIZE(obj%facetElements)
  !
  ! IF (.NOT. ALLOCATED(obj%facetSTelemsd)) &
  !   & ALLOCATE (obj%facetSTelemsd(mm, nn))
  !
  ! DO jj = 1, nn
  !
  !   DO ii = 1, mm
  !
  !     obj%facetSTelemsd(ii, jj) = stelemsd(ii)
  !
  !     CALL Initiate( &
  !       & obj=obj%facetSTelemsd(ii, jj), &
  !       & quad=obj%quadForFacet(jj), &
  !       & refelem=obj%facetElemSD(jj)%refelem, &
  !       & ContinuityType=obj%continuityTypeForFacet%chars(), &
  !       & InterpolType=obj%interpolTypeForFacet%chars())
  !   END DO
  !
  ! END DO
  !
  ! DEALLOCATE (stelemsd)

END SUBROUTINE obj_InitiateFacetElemSD3

END MODULE ElementShapeFunctionData_Class
