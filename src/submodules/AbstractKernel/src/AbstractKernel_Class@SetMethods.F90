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

SUBMODULE(AbstractKernel_Class) SetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               SetShowTime
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetShowTime
obj%showTime = showTime
END PROCEDURE obj_SetShowTime

!----------------------------------------------------------------------------
!                                                                    PreSet
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_PreSet
CHARACTER(*), PARAMETER :: myName = "obj_PreSet()"
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & 'This routine does noting.')
END PROCEDURE obj_PreSet

!----------------------------------------------------------------------------
!                                                                   PostSet
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_PostSet
CHARACTER(*), PARAMETER :: myName = "obj_PostSet()"
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & 'This routine does noting.')
END PROCEDURE obj_PostSet

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set
CHARACTER(*), PARAMETER :: myName = "obj_Set()"
TYPE(BoundingBox_) :: bbox
TYPE(CPUTime_) :: TypeCPUTime

IF (obj%showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

CALL obj%PreCheckError()

CALL obj%PreSet()

CALL obj%SetMeshData()

! Set Mesh Data required for Nitsche formulation from MovingMeshUtility
IF (obj%isNitsche) CALL obj%SetNitscheMeshData()

! Now we map materials (MeshSelection_) to the domain
CALL obj%SetMaterialToDomain()

! Call InitiateFields, which is defined by children of abstract kernel
CALL obj%InitiateFields()

! now we make elemToMatId, which contains fluid-material-id for
! each element. We can use these material-id to Get access the fluid material
CALL obj%SetElementToMatID()

! Set finite element in space as well as in time
! Set local space function data in space and time
CALL obj%SetFiniteElements()
! CALL obj%SetElemShapeData()

! Local element shape data for the domain of velocity field
CALL obj%SetFacetFiniteElements()
! CALL obj%SetFacetElemShapeData()

! Get length scale of the problem
bbox = obj%dom%GetBoundingBox()
obj%lengthScale = GetDiameter(bbox)

! Create MatIfaceConnectData
CALL obj%SetMatIFaceConnectData()

! Create SetConstantMatProp
CALL obj%SetMaterialProperties()

CALL obj%PostSet()

CALL obj%PostCheckError()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif

IF (obj%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL obj%showTimeFile%WRITE(val=TypeCPUTime%GetStringForKernelLog( &
  & currentTime=obj%currentTime, currentTimeStep=obj%currentTimeStep, &
  & methodName=myName))
END IF
END PROCEDURE obj_Set

!----------------------------------------------------------------------------
!                                                         SetCurrentTimeStep
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetCurrentTimeStep
obj%currentTimeStep = its
END PROCEDURE obj_SetCurrentTimeStep

!----------------------------------------------------------------------------
!                                                       SetIterationNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetIterationNumber
obj%iterData%iterationNumber = iter
END PROCEDURE obj_SetIterationNumber

!----------------------------------------------------------------------------
!                                                              SetMeshData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMeshData
CHARACTER(*), PARAMETER :: myName = "obj_SetMeshData()"
TYPE(CPUTime_) :: TypeCPUTime

IF (obj%showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

IF (ASSOCIATED(obj%dom)) THEN
  CALL obj%dom%InitiateNodeToElements()
  CALL obj%dom%InitiateNodeToNodes()
  CALL obj%dom%InitiateFacetElements()
ELSE
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[POINTER ERROR] :: AbstractKernel_::obj%dom is not associated.')
  RETURN
END IF

IF (ALLOCATED(obj%domains)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[WIP] :: AbstractKernel_::obj%domains  case todo.')
  RETURN
END IF
! TODO: Implement SetMeshData when isCommonDomain is false.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

IF (obj%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL obj%showTimeFile%WRITE(val=TypeCPUTime%GetStringForKernelLog( &
  & currentTime=obj%currentTime, currentTimeStep=obj%currentTimeStep, &
  & methodName=myName))
END IF
END PROCEDURE obj_SetMeshData

!----------------------------------------------------------------------------
!                                                   SetMatIFaceConnectData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMatIFaceConnectData
CHARACTER(*), PARAMETER :: myName = "obj_SetMatIFaceConnectData()"
INTEGER(I4B) :: ii, jj, tsize
CLASS(Mesh_), POINTER :: amesh
CLASS(Domain_), POINTER :: dom
TYPE(CPUTime_) :: TypeCPUTime

IF (obj%showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

amesh => NULL()
dom => NULL()
dom => obj%dom
IF (obj%ismaterialInterfaces) THEN
  tsize = SIZE(obj%materialInterfaces)
  DO ii = 1, tsize
    jj = obj%materialInterfaces(ii)
    amesh => dom%GetMeshPointer(dim=obj%nsd - 1, entityNum=jj)
    CALL obj%matIfaceConnectData(ii)%InitiateFacetToCellData( &
      & facetMesh=amesh, cellDomain=dom)
  END DO
END IF
NULLIFY (amesh, dom)

#ifdef DEBUG_VER
CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif

IF (obj%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL obj%showTimeFile%WRITE(val=TypeCPUTime%GetStringForKernelLog( &
  & currentTime=obj%currentTime, currentTimeStep=obj%currentTimeStep, &
  & methodName=myName))
END IF
END PROCEDURE obj_SetMatIFaceConnectData

!----------------------------------------------------------------------------
!                                                         SetFiniteElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFiniteElements
CHARACTER(*), PARAMETER :: myName = "obj_SetFiniteElements()"
INTEGER(I4B), ALLOCATABLE :: order(:), elemType(:)
INTEGER(I4B) :: tsize, ii, nsd
LOGICAL(LGT) :: problem
TYPE(CPUTime_) :: TypeCPUTime

IF (obj%showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

nsd = obj%nsd
tsize = obj%dom%GetTotalMesh(dim=nsd)
CALL Reallocate(elemType, tsize)
CALL Reallocate(order, tsize)
elemType = obj%dom%GetElemType(dim=nsd)
order = obj%dom%GetOrder(dim=nsd)

problem = (ALLOCATED(obj%cellFE) .OR. ALLOCATED(obj%geoCellFE))
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: AbstractKernel_::obj%cellFE or obj%geoCellFE '//  &
    & 'already allocated.')
  RETURN
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] Setting Cell Finite Element.')
#endif DEBUG_VER

ALLOCATE (obj%cellFE(tsize), obj%geoCellFE(tsize))

DO ii = 1, tsize
  ALLOCATE (FiniteElement_ :: obj%cellFE(ii)%ptr)
  ALLOCATE (FiniteElement_ :: obj%geoCellFE(ii)%ptr)

  CALL obj%cellFE(ii)%ptr%InitiateLagrangeFE( &
    & nsd=nsd,  &
    & elemType=elemType(ii),  &
    & order=order(ii),  &
    & baseContinuity=obj%baseContinuityForSpace%chars(),  &
    & baseInterpolation=obj%baseInterpolationForSpace%chars(),  &
    & ipType=obj%ipTypeForSpace,  &
    & basisType=obj%basisTypeForSpace,  &
    & alpha=obj%alphaForSpace,  &
    & beta=obj%betaForSpace,  &
    & lambda=obj%lambdaForSpace)

  CALL obj%geoCellFE(ii)%ptr%InitiateLagrangeFE( &
    & nsd=nsd,  &
    & elemType=elemType(ii),  &
    & order=order(ii),  &
    & baseContinuity=obj%baseContinuityForSpace%chars(),  &
    & baseInterpolation=obj%baseInterpolationForSpace%chars(),  &
    & ipType=obj%ipTypeForSpace,  &
    & basisType=obj%basisTypeForSpace,  &
    & alpha=obj%alphaForSpace,  &
    & beta=obj%betaForSpace,  &
    & lambda=obj%lambdaForSpace)

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] Setting Cell Finite Element.')
#endif DEBUG_VER

IF (nsd .GE. 2) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] Setting Facet Finite Element.')
#endif DEBUG_VER

  elemType = obj%dom%GetElemType(dim=nsd - 1)
  order = obj%dom%GetOrder(dim=nsd - 1)
  tsize = SIZE(elemType)

  problem = ALLOCATED(obj%facetFE) .OR. ALLOCATED(obj%geoFacetFE)
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: AbstractKernel_::obj%facetFE '//  &
    & 'or obj%geoFacetFE already allocated.')
    RETURN
  END IF

  ALLOCATE (obj%facetFE(tsize), obj%geoFacetFE(tsize))

  DO ii = 1, tsize

    ALLOCATE (FiniteElement_ :: obj%facetFE(ii)%ptr)
    ALLOCATE (FiniteElement_ :: obj%geoFacetFE(ii)%ptr)

    CALL obj%facetFE(ii)%ptr%InitiateLagrangeFE( &
      & nsd=nsd,  &
      & elemType=elemType(ii),  &
      & order=order(ii),  &
      & baseContinuity=obj%baseContinuityForSpace%chars(),  &
      & baseInterpolation=obj%baseInterpolationForSpace%chars(),  &
      & ipType=obj%ipTypeForSpace,  &
      & basisType=obj%basisTypeForSpace,  &
      & alpha=obj%alphaForSpace,  &
      & beta=obj%betaForSpace,  &
      & lambda=obj%lambdaForSpace)

    CALL obj%geoFacetFE(ii)%ptr%InitiateLagrangeFE( &
      & nsd=nsd,  &
      & elemType=elemType(ii),  &
      & order=order(ii),  &
      & baseContinuity=obj%baseContinuityForSpace%chars(),  &
      & baseInterpolation=obj%baseInterpolationForSpace%chars(),  &
      & ipType=obj%ipTypeForSpace,  &
      & basisType=obj%basisTypeForSpace,  &
      & alpha=obj%alphaForSpace,  &
      & beta=obj%betaForSpace,  &
      & lambda=obj%lambdaForSpace)

  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] Setting Facet Finite Element.')
#endif DEBUG_VER
END IF

IF (nsd .GE. 3) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] Setting edge Finite Element.')
#endif DEBUG_VER

  elemType = obj%dom%GetElemType(dim=nsd - 2)
  order = obj%dom%GetOrder(dim=nsd - 2)
  tsize = SIZE(elemType)

  IF (ALLOCATED(obj%edgeFE) .OR. ALLOCATED(obj%geoEdgeFE)) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: AbstractKernel_::obj%edgeFE '//  &
      & 'or obj%geoEdgeFE already allocated.')
    RETURN
  END IF

  ! CALL DEALLOCATE (obj%edgeFE)
  ALLOCATE (obj%edgeFE(tsize), obj%geoEdgeFE(tsize))

  DO ii = 1, tsize
    ALLOCATE (FiniteElement_ :: obj%edgeFE(ii)%ptr)
    ALLOCATE (FiniteElement_ :: obj%geoEdgeFE(ii)%ptr)

    CALL obj%edgeFE(ii)%ptr%InitiateLagrangeFE( &
      & nsd=nsd,  &
      & elemType=elemType(ii),  &
      & order=order(ii),  &
      & baseContinuity=obj%baseContinuityForSpace%chars(),  &
      & baseInterpolation=obj%baseInterpolationForSpace%chars(),  &
      & ipType=obj%ipTypeForSpace,  &
      & basisType=obj%basisTypeForSpace,  &
      & alpha=obj%alphaForSpace,  &
      & beta=obj%betaForSpace,  &
      & lambda=obj%lambdaForSpace)

    CALL obj%geoEdgeFE(ii)%ptr%InitiateLagrangeFE( &
      & nsd=nsd,  &
      & elemType=elemType(ii),  &
      & order=order(ii),  &
      & baseContinuity=obj%baseContinuityForSpace%chars(),  &
      & baseInterpolation=obj%baseInterpolationForSpace%chars(),  &
      & ipType=obj%ipTypeForSpace,  &
      & basisType=obj%basisTypeForSpace,  &
      & alpha=obj%alphaForSpace,  &
      & beta=obj%betaForSpace,  &
      & lambda=obj%lambdaForSpace)
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] Setting edge Finite Element.')
#endif DEBUG_VER
END IF

IF (obj%nnt .GT. 1_I4B) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] Setting time Finite Element.')
#endif DEBUG_VER

  CALL obj%timeFE%InitiateLagrangeFE( &
    & nsd=nsd,  &
    & elemType=Line2,  &
    & order=obj%nnt - 1_I4B,  &
    & baseContinuity=obj%baseContinuityForTime%chars(),  &
    & baseInterpolation=obj%baseInterpolationForTime%chars(),  &
    & ipType=obj%ipTypeForTime,  &
    & basisType=obj%basisTypeForTime,  &
    & alpha=obj%alphaForTime,  &
    & beta=obj%betaForTime,  &
    & lambda=obj%lambdaForTime)

  CALL obj%geoTimeFE%InitiateLagrangeFE( &
    & nsd=nsd,  &
    & elemType=Line2,  &
    & order=obj%nnt - 1_I4B,  &
    & baseContinuity=obj%baseContinuityForTime%chars(),  &
    & baseInterpolation=obj%baseInterpolationForTime%chars(),  &
    & ipType=obj%ipTypeForTime,  &
    & basisType=obj%basisTypeForTime,  &
    & alpha=obj%alphaForTime,  &
    & beta=obj%betaForTime,  &
    & lambda=obj%lambdaForTime)

  CALL obj%SetQuadPointsInTime()
  CALL obj%SetLocalElemShapeDataInTime()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] Setting time Finite Element.')
#endif DEBUG_VER
END IF

CALL obj%SetQuadPointsInSpace()
CALL obj%SetLocalElemShapeDataInSpace()

IF (ALLOCATED(elemType)) DEALLOCATE (elemType)
IF (ALLOCATED(order)) DEALLOCATE (order)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

IF (obj%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL obj%showTimeFile%WRITE(val=TypeCPUTime%GetStringForKernelLog( &
  & currentTime=obj%currentTime, currentTimeStep=obj%currentTimeStep, &
  & methodName=myName))
END IF
END PROCEDURE obj_SetFiniteElements

!----------------------------------------------------------------------------
!                                                     SetQuadPointsInSpace
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetQuadPointsInSpace
CHARACTER(*), PARAMETER :: myName = "obj_SetQuadPointsInSpace()"
INTEGER(I4B) :: ii, tCell, order
LOGICAL(LGT) :: isok
CLASS(FiniteElement_), POINTER :: fe
TYPE(CPUTime_) :: TypeCPUTime

IF (obj%showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

isok = ALLOCATED(obj%cellFE)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractKernel_::obj%cellFE not allocated')
  RETURN
END IF

tCell = SIZE(obj%cellFE)
fe => NULL()

isok = ALLOCATED(obj%quadratureForSpace)
IF (.NOT. isok) THEN
  ALLOCATE (obj%quadratureForSpace(tCell))
END IF

DO ii = 1, tCell
  fe => obj%cellFE(ii)%ptr
  CALL fe%GetParam(order=order)
  order = order * 2
  CALL fe%GetQuadraturePoints( &
    & quad=obj%quadratureForSpace(ii), &
    & quadratureType=[obj%quadTypeForSpace],  &
    & order=[order],  &
    & alpha=[obj%alphaForSpace],  &
    & beta=[obj%betaForSpace],  &
    & lambda=[obj%lambdaForSpace])
END DO

NULLIFY (fe)

!----------------------------------------------------------------------------
!                                                                   facetFE
!----------------------------------------------------------------------------

isok = ALLOCATED(obj%facetFE)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractKernel_::obj%facetFE not allocated')
  RETURN
END IF

tCell = SIZE(obj%facetFE)
fe => NULL()

isok = ALLOCATED(obj%quadratureForSpace_facet)
IF (.NOT. isok) THEN
  ALLOCATE (obj%quadratureForSpace_facet(tCell))
END IF

DO ii = 1, tCell
  fe => obj%facetFE(ii)%ptr
  CALL fe%GetParam(order=order)
  order = order * 2
  CALL fe%GetQuadraturePoints( &
    & quad=obj%quadratureForSpace_facet(ii), &
    & quadratureType=[obj%quadTypeForSpace],  &
    & order=[order],  &
    & alpha=[obj%alphaForSpace],  &
    & beta=[obj%betaForSpace],  &
    & lambda=[obj%lambdaForSpace])
END DO

NULLIFY (fe)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

IF (obj%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL obj%showTimeFile%WRITE(val=TypeCPUTime%GetStringForKernelLog( &
  & currentTime=obj%currentTime, currentTimeStep=obj%currentTimeStep, &
  & methodName=myName))
END IF
END PROCEDURE obj_SetQuadPointsInSpace

!----------------------------------------------------------------------------
!                                                     SetQuadPointsInTime
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetQuadPointsInTime
CHARACTER(*), PARAMETER :: myName = "obj_SetQuadPointsInTime()"
INTEGER(I4B) :: order
TYPE(CPUTime_) :: TypeCPUTime

IF (obj%showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

IF (obj%nnt .GT. 1) THEN
  CALL obj%timeFE%GetParam(order=order)
  order = order * 2
  CALL obj%timeFE%GetQuadraturePoints( &
    & quad=obj%quadratureForTime, &
    & quadratureType=[obj%quadTypeForTime],  &
    & order=[order],  &
    & alpha=[obj%alphaForTime],  &
    & beta=[obj%betaForTime],  &
    & lambda=[obj%lambdaForTime])
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif

IF (obj%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL obj%showTimeFile%WRITE(val=TypeCPUTime%GetStringForKernelLog( &
  & currentTime=obj%currentTime, currentTimeStep=obj%currentTimeStep, &
  & methodName=myName))
END IF
END PROCEDURE obj_SetQuadPointsInTime

!----------------------------------------------------------------------------
!                                               SetLocalElemShapeDataInSpace
!----------------------------------------------------------------------------

! This routine Sets the local shape data in space (geoSpaceElemSD and
! spaceElemSD) for the mesh.
! The quadrature points should be initiated before calling this routine.
MODULE PROCEDURE obj_SetLocalElemShapeDataInSpace
CHARACTER(*), PARAMETER :: myName = "obj_SetLocalElemShapeDataInSpace()"
INTEGER(I4B) :: ii, tCell
CLASS(FiniteElement_), POINTER :: fe
LOGICAL(LGT) :: isok, problem
TYPE(CPUTime_) :: TypeCPUTime

IF (obj%showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

isok = ALLOCATED(obj%cellFE)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: AbstractKernel_::obj%cellFE not allocated')
  RETURN
END IF

isok = ALLOCATED(obj%quadratureForSpace)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[CONFIG ERROR] :: AbstractKernel_::obj%quadratureForSpace not allocated')
  RETURN
END IF

tCell = SIZE(obj%cellFE)
fe => NULL()

problem = ALLOCATED(obj%spaceElemSD)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[CONFIG ERROR] :: AbstractKernel_::obj%spaceElemSD already allocated')
  RETURN
END IF

IF (ALLOCATED(obj%geoSpaceElemSD)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[CONFIG ERROR] :: AbstractKernel_::obj%geoSpaceElemSD already allocated')
  RETURN
END IF

ALLOCATE (obj%spaceElemSD(tCell), obj%geoSpaceElemSD(tCell))

DO ii = 1, tCell
  fe => obj%cellFE(ii)%ptr
  CALL fe%GetLocalElemShapeData( &
    & quad=obj%quadratureForSpace(ii), &
    & elemsd=obj%spaceElemSD(ii))

  fe => obj%geoCellFE(ii)%ptr
  CALL fe%GetLocalElemShapeData( &
    & quad=obj%quadratureForSpace(ii), &
    & elemsd=obj%geoSpaceElemSD(ii))
END DO

NULLIFY (fe)

!----------------------------------------------------------------------------
!                                                                   facetFE
!----------------------------------------------------------------------------

isok = ALLOCATED(obj%facetFE)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: AbstractKernel_::obj%facetFE not allocated')
  RETURN
END IF

isok = ALLOCATED(obj%quadratureForSpace_facet)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[CONFIG ERROR] :: AbstractKernel_::obj%quadratureForSpace_facet'// &
  & ' not allocated.')
  RETURN
END IF

tCell = SIZE(obj%facetFE)
fe => NULL()

problem = ALLOCATED(obj%spaceElemSD_facet)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[CONFIG ERROR] :: AbstractKernel_::obj%spaceElemSD_facet'//  &
  & ' already allocated')
  RETURN
END IF

IF (ALLOCATED(obj%geoSpaceElemSD_facet)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[CONFIG ERROR] :: AbstractKernel_::obj%geoSpaceElemSD_facet '//  &
  & ' already allocated')
  RETURN
END IF

ALLOCATE (obj%spaceElemSD_facet(tCell), obj%geoSpaceElemSD_facet(tCell))

DO ii = 1, tCell
  fe => obj%facetFE(ii)%ptr
  CALL fe%GetLocalElemShapeData( &
    & quad=obj%quadratureForSpace_facet(ii), &
    & elemsd=obj%spaceElemSD_facet(ii))

  fe => obj%geoFacetFE(ii)%ptr
  CALL fe%GetLocalElemShapeData( &
    & quad=obj%quadratureForSpace_facet(ii), &
    & elemsd=obj%geoSpaceElemSD_facet(ii))
END DO

NULLIFY (fe)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif

IF (obj%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL obj%showTimeFile%WRITE(val=TypeCPUTime%GetStringForKernelLog( &
  & currentTime=obj%currentTime, currentTimeStep=obj%currentTimeStep, &
  & methodName=myName))
END IF
END PROCEDURE obj_SetLocalElemShapeDataInSpace

!----------------------------------------------------------------------------
!                                               SetLocalElemShapeDataInTime
!----------------------------------------------------------------------------

! This routine Sets the local shape data in time (geoTimeElemSD and
! timeElemSD) for the mesh.
! The quadrature points should be initiated before calling this routine.
MODULE PROCEDURE obj_SetLocalElemShapeDataInTime
CHARACTER(*), PARAMETER :: myName = "obj_SetLocalElemShapeDataInTime()"
TYPE(CPUTime_) :: TypeCPUTime

IF (obj%showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

CALL obj%timeFE%GetLocalElemShapeData( &
  & quad=obj%quadratureForTime, &
  & elemsd=obj%timeElemSD)

CALL obj%geoTimeFE%GetLocalElemShapeData( &
  & quad=obj%quadratureForTime, &
  & elemsd=obj%geoTimeElemSD)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif

IF (obj%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL obj%showTimeFile%WRITE(val=TypeCPUTime%GetStringForKernelLog( &
  & currentTime=obj%currentTime, currentTimeStep=obj%currentTimeStep, &
  & methodName=myName))
END IF
END PROCEDURE obj_SetLocalElemShapeDataInTime

!----------------------------------------------------------------------------
!                                             SetGlobalElemShapeDataInSpace
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetGlobalElemShapeDataInSpace
CHARACTER(*), PARAMETER :: myName = " obj_SetGlobalElemShapeDataInSpace()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine has not been implemented yet.')
END PROCEDURE obj_SetGlobalElemShapeDataInSpace

!----------------------------------------------------------------------------
!                                              SetGlobalElemShapeDataInTime
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetGlobalElemShapeDataInTime
CHARACTER(*), PARAMETER :: myName = " obj_SetGlobalElemShapeDataInTime()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine has not been implemented yet.')
END PROCEDURE obj_SetGlobalElemShapeDataInTime

!----------------------------------------------------------------------------
!                                                    SetFacetFiniteElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFacetFiniteElements
CHARACTER(*), PARAMETER :: myName = "obj_SetFacetFiniteElements()"
CALL e%RaiseWarning(modName//'::'//myName//' - '// &
  & '[WIP WARNING] :: This routine has been implemented yet.')
END PROCEDURE obj_SetFacetFiniteElements

!----------------------------------------------------------------------------
!                                                         SetAlgoParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetAlgoParam
CHARACTER(*), PARAMETER :: myName = "obj_SetAlgoParam"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[IMPLEMENTATION ERROR] :: This routine should be implemented by '//&
  & 'child classes')
END PROCEDURE obj_SetAlgoParam

END SUBMODULE SetMethods
