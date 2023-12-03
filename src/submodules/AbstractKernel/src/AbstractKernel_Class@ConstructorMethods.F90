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

SUBMODULE(AbstractKernel_Class) ConstructorMethods
USE BaseMethod
USE FPL_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                   SetAbstractKernelParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetAbstractKernelParam
INTEGER(I4B) :: aint
CHARACTER(*), PARAMETER :: myName = "SetAbstractKernelParam()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

! char
CALL Set(obj=param, datatype="char", prefix=prefix, &
  & key="name", VALUE=name)
CALL Set(obj=param, datatype="char", prefix=prefix, &
  & key="engine", VALUE=engine)
CALL Set(param, datatype="char", prefix=prefix,  &
  & key="domainFile", VALUE=domainFile)
CALL Set(param, datatype="char", prefix=prefix,  &
  & key="domainFile", VALUE=domainFile)
CALL Set(param, datatype="char", prefix=prefix,  &
  & key="baseInterpolationForSpace",  &
  & VALUE=input(option=baseInterpolationForSpace,  &
  & default=DEFAULT_baseInterpolationForSpace))
CALL Set(param, datatype="char", prefix=prefix,  &
  & key="baseInterpolationForTime",  &
  & VALUE=input(option=baseInterpolationForTime,  &
  & default=DEFAULT_baseInterpolationForTime))
CALL Set(param, datatype="char", prefix=prefix,  &
  & key="baseContinuityForSpace",  &
  & VALUE=input(option=baseContinuityForSpace,  &
  & default=DEFAULT_baseContinuityForSpace))
CALL Set(param, datatype="char", prefix=prefix,  &
  & key="baseContinuityForTime",  &
  & VALUE=input(option=basecontinuityfortime,  &
  & default=default_basecontinuityfortime))
CALL Set(param, datatype="char", prefix=prefix,  &
  & key="quadratureTypeForSpace",  &
  & VALUE=input(option=quadratureTypeForSpace,  &
  & default=DEFAULT_quadratureTypeForSpace))
CALL Set(param, datatype="char", prefix=prefix,  &
  & key="quadratureTypeForTime",  &
  & VALUE=input(option=quadratureTypeForTime,  &
  & default=DEFAULT_quadratureTypeForTime))

!! int
CALL Set(param, TypeIntI4B, prefix, "coordinateSystem", coordinateSystem)
CALL Set(param, TypeIntI4B, prefix, "timeDependency",  &
  & INPUT(option=timeDependency, default=KERNEL_STEADY))
!! NOTE: KERNEL_STEADY is defined in AbstractKernelParam module
CALL Set(param, TypeIntI4B, prefix, "maxIter",  &
  & INPUT(option=maxIter, default=default_maxIter))
CALL Set(param, TypeIntI4B, prefix, "nsd",  &
  & INPUT(option=nsd, default=0_I4B))
CALL Set(param, TypeIntI4B, prefix, "nnt",  &
  & INPUT(option=nnt, default=1_I4B))
CALL Set(param, TypeIntI4B, prefix, "tdof",  &
  & INPUT(option=tdof, default=0_I4B))
CALL Set(param, TypeIntI4B, prefix, "currentTimeStep",  &
  & INPUT(option=currentTimeStep, default=1_I4B))
CALL Set(param, TypeIntI4B, prefix, "totalTimeStep",  &
  & INPUT(option=totalTimeStep, default=1_I4B))
CALL Set(param, TypeIntI4B, prefix, "postProcessOpt", postProcessOpt)
CALL Set(param, TypeIntI4B, prefix, "ipTypeForSpace", ipTypeForSpace)
CALL Set(param, TypeIntI4B, prefix, "ipTypeForTime", ipTypeForTime)
CALL Set(param, TypeIntI4B, prefix, "basisTypeForSpace", basisTypeForSpace)
CALL Set(param, TypeIntI4B, prefix, "basisTypeForTime", basisTypeForTime)

CALL Set(param, TypeIntI4B, prefix, "tDirichletBC",  &
 & INPUT(option=tDirichletBC, default=0_I4B))

CALL Set(param, TypeIntI4B, prefix, "tNeumannBC",  &
 & INPUT(option=tNeumannBC, default=0_I4B))

CALL Set(param, TypeIntI4B, prefix, "tWeakDirichletBC",  &
 & INPUT(option=tWeakDirichletBC, default=0_I4B))

CALL Set(param, .TRUE., prefix, "isSymNitsche",  &
  & INPUT(option=isSymNitsche, default=DEFAULT_isSymNitsche))
! INFO: DEFAULT_isSymNitsche is definedin AbstractElasticityParam module

! real
CALL Set(param, TypeDFP, prefix, "nitscheAlpha",  &
  & INPUT(option=nitscheAlpha, default=DEFAULT_nitscheAlpha))

aint = QuadraturePointNameToID(INPUT(option=quadratureTypeForSpace,  &
  & default=DEFAULT_quadratureTypeForSpace))
CALL Set(param, datatype=TypeIntI4B, prefix=prefix, key="quadTypeForSpace", &
  & VALUE=aint)

aint = QuadraturePointNameToID(INPUT(option=quadratureTypeForTime,  &
  & default=DEFAULT_quadratureTypeForTime))
CALL Set(param, datatype=TypeIntI4B, prefix=prefix, key="quadTypeForTime",  &
  & VALUE=aint)

!! bool
CALL Set(param, .TRUE., prefix, "isCommonDomain", isCommonDomain)

!! real
CALL Set(param, [TypeDFP], prefix, "gravity",  &
  & INPUT(default=[0.0_DFP, 0.0_DFP, 0.0_DFP], option=gravity))
CALL Set(param, TypeDFP, prefix, "dt",  &
  & INPUT(option=dt, default=0.0_DFP))
CALL Set(param, TypeDFP, prefix, "startTime",  &
  & INPUT(option=startTime, default=0.0_DFP))
CALL Set(param, TypeDFP, prefix, "endTime",  &
  & INPUT(option=endTime, default=0.0_DFP))
CALL Set(param, TypeDFP, prefix, "currentTime",  &
  & INPUT(option=currentTime, default=0.0_DFP))

CALL Set(param, TypeDFP, prefix, "alphaForSpace", alphaForSpace)
CALL Set(param, TypeDFP, prefix, "alphaForTime", alphaForTime)
CALL Set(param, TypeDFP, prefix, "betaForSpace", betaForSpace)
CALL Set(param, TypeDFP, prefix, "betaForTime", betaForTime)
CALL Set(param, TypeDFP, prefix, "lambdaForSpace", lambdaForSpace)
CALL Set(param, TypeDFP, prefix, "lambdaForTime", lambdaForTime)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif

END PROCEDURE SetAbstractKernelParam

!----------------------------------------------------------------------------
!                                                        CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "obj_CheckEssentialParam"
CHARACTER(:), ALLOCATABLE :: prefix0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

prefix0 = input(default=obj%GetPrefix(), option=prefix)

CALL CheckEssentialParam(obj=param,  &
  & keys=AbstractKernelEssentialParam,  &
  & prefix=prefix0,  &
  & myName=myName,  &
  & modName=modName)
!NOTE: CheckEssentialParam param is defined in easifemClasses FPL_Method

! linsol
IF (ASSOCIATED(obj%linsol)) THEN
  CALL obj%linsol%CheckEssentialParam(param=param)
END IF

! tanmat
IF (ASSOCIATED(obj%tanmat)) THEN
  CALL obj%tanmat%CheckEssentialParam(param=param)
END IF

prefix0 = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
INTEGER(I4B) :: ii
INTEGER(I4B) :: tDirichletBC
INTEGER(I4B) :: tWeakDirichletBC
INTEGER(I4B) :: tNeumannBC
LOGICAL(LGT) :: isSymNitsche
CHARACTER(:), ALLOCATABLE :: prefix

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

! check
IF (obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[CONFIG ERROR] :: The object is already initiated, deallocate first!')
  RETURN
END IF

obj%isInitiated = .TRUE.
prefix = obj%GetPrefix()
CALL obj%CheckEssentialParam(param, prefix)

CALL GetValue(param, prefix, "isCommonDomain", obj%isCommonDomain)
CALL GetValue(param, prefix, "name", obj%name)
CALL GetValue(param, prefix, "engine", obj%engine)
CALL GetValue(param, prefix, "coordinateSystem", obj%coordinateSystem)
CALL GetValue(param, prefix, "maxIter", obj%maxIter)
CALL GetValue(param, prefix, "timeDependency", obj%timeDependency)
CALL GetValue(param, prefix, "nsd", obj%nsd)
CALL GetValue(param, prefix, "nnt", obj%nnt)
CALL GetValue(param, prefix, "tdof", obj%tdof)
CALL GetValue(param, prefix, "dt", obj%dt)
CALL GetValue(param, prefix, "startTime", obj%startTime)
CALL GetValue(param, prefix, "endTime", obj%endTime)
CALL GetValue(param, prefix, "currentTime", obj%currentTime)
CALL GetValue(param, prefix, "currentTimeStep", obj%currentTimeStep)
CALL GetValue(param, prefix, "totalTimeStep", obj%totalTimeStep)
CALL GetValue(param, prefix, "postProcessOpt", obj%postProcessOpt)
CALL GetValue(param, prefix, "gravity", obj%gravity)
CALL GetValue(param, prefix, "baseContinuityForSpace",  &
  & obj%baseContinuityForSpace)
CALL GetValue(param, prefix, "baseContinuityForTime",  &
  & obj%baseContinuityForTime)
CALL GetValue(param, prefix, "baseInterpolationForSpace",  &
  & obj%baseInterpolationForSpace)
CALL GetValue(param, prefix, "baseInterpolationForTime",  &
  & obj%baseInterpolationForTime)
CALL GetValue(param, prefix, "quadratureTypeForSpace",  &
  & obj%quadratureTypeForSpace)
CALL GetValue(param, prefix, "quadratureTypeForTime",  &
  & obj%quadratureTypeForTime)
CALL GetValue(param, prefix, "quadTypeForSpace", obj%quadTypeForSpace)
CALL GetValue(param, prefix, "quadTypeForTime", obj%quadTypeForTime)
CALL GetValue(param, prefix, "domainFile", obj%domainFile)

obj%ipTypeForSpace = DEFAULT_ipTypeForSpace
obj%ipTypeForTime = DEFAULT_ipTypeForTime

obj%basisTypeForSpace = DEFAULT_basisTypeForSpace
obj%basisTypeForTime = DEFAULT_basisTypeForTime

obj%alphaForSpace = DEFAULT_alphaForSpace
obj%betaForSpace = DEFAULT_betaForSpace
obj%lambdaForSpace = DEFAULT_lambdaForSpace

obj%alphaForTime = DEFAULT_alphaForTime
obj%betaForTime = DEFAULT_betaForTime
obj%lambdaForTime = DEFAULT_lambdaForTime

CALL GetValue(param, prefix, "ipTypeForSpace", obj%ipTypeForSpace)
CALL GetValue(param, prefix, "ipTypeForTime", obj%ipTypeForTime)
CALL GetValue(param, prefix, "basisTypeForSpace", obj%basisTypeForSpace)
CALL GetValue(param, prefix, "basisTypeForTime", obj%basisTypeForTime)
CALL GetValue(param, prefix, "alphaForSpace", obj%alphaForSpace)
CALL GetValue(param, prefix, "betaForSpace", obj%betaForSpace)
CALL GetValue(param, prefix, "lambdaForSpace", obj%lambdaForSpace)
CALL GetValue(param, prefix, "alphaForTime", obj%alphaForTime)
CALL GetValue(param, prefix, "betaForTime", obj%betaForTime)
CALL GetValue(param, prefix, "lambdaForTime", obj%lambdaForTime)

! iterData
CALL Initiate( &
  & obj=obj%iterdata, &
  & maxIter=obj%maxIter, &
  & convergenceType=absoluteConvergence, &
  & convergenceIn=convergenceInSol, &
  & normType=NormL2, &
  & timeAtStart=obj%currentTime, &
  & timeAtEnd=obj%currentTime + obj%dt)

! linsol
obj%linsol => LinearSolverFactory(obj%engine%chars())
CALL obj%linsol%initiate(param=param)

IF (PRESENT(dom)) THEN
  obj%dom => dom
  obj%isCommonDomain = .TRUE.
  obj%nsd = dom%GetNSD()
END IF

IF (PRESENT(domains)) THEN
  ALLOCATE (obj%domains(SIZE(domains)))
  DO ii = 1, SIZE(domains)
    obj%domains(ii)%ptr => domains(ii)%ptr
  END DO
END IF

IF (.NOT. PRESENT(domains) .AND. .NOT. PRESENT(dom)) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[ARGUMENT ERROR] :: Either dom or domains should be present.')
  RETURN
END IF

tDirichletBC = 0
CALL GetValue(param, prefix, "tDirichletBC", tDirichletBC)
ALLOCATE (obj%dbc(tDirichletBC))

CALL GetValue(param, prefix, "tNeumannBC", tNeumannBC)
ALLOCATE (obj%nbc(tNeumannBC))

tWeakDirichletBC = 0
CALL GetValue(param, prefix, "tWeakDirichletBC", tWeakDirichletBC)
ALLOCATE (obj%wdbc(tWeakDirichletBC))
obj%isNitsche = .FALSE.
IF (tWeakDirichletBC .GT. 0) obj%isNitsche = .TRUE.

CALL GetValue(param, prefix, "isSymNitsche", isSymNitsche)
obj%NitscheType = Nitsche_SkewSym
IF (isSymNitsche) obj%NitscheType = Nitsche_Sym

CALL GetValue(param, prefix, "nitscheAlpha", obj%nitscheAlpha)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif

END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                       KernelDeallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
INTEGER(I4B) :: ii, jj
obj%IsInitiated = .FALSE.
obj%name = ""
obj%engine = ""
obj%coordinateSystem = 0
obj%domainFile = ""
obj%isCommonDomain = .TRUE.
obj%timeDependency = 0
obj%maxIter = 0
obj%nsd = 0
obj%nnt = 0
obj%tdof = 0
obj%normRHS = 0.0_DFP
obj%dt = 0.0
obj%startTime = 0.0
obj%endTime = 0.0
obj%currentTime = 0.0
obj%currentTimeStep = 0
obj%totalTimeStep = 0
IF (ALLOCATED(obj%elemToMatId)) DEALLOCATE (obj%elemToMatId)
IF (ALLOCATED(obj%dbcIndx)) DEALLOCATE (obj%dbcIndx)
obj%postProcessOpt = 0
CALL DEALLOCATE (obj%iterData)
obj%baseContinuityForSpace = ''
obj%baseInterpolationForSpace = ''
obj%quadratureTypeForSpace = ''
obj%baseContinuityForTime = ''
obj%quadTypeForSpace = 0_I4B
obj%baseInterpolationForTime = ''
obj%quadratureTypeForTime = ''
obj%quadTypeForTime = 0_I4B
obj%linsol => NULL()
obj%tanmat => NULL()
CALL DEALLOCATE (obj%refTimeElem)
CALL DEALLOCATE (obj%refLinTimeElem)
obj%dom => NULL()
IF (ALLOCATED(obj%domains)) THEN
  DO ii = 1, SIZE(obj%domains)
    obj%domains(ii)%ptr => NULL()
  END DO
  DEALLOCATE (obj%domains)
END IF

IF (ALLOCATED(obj%quadratureForSpace)) THEN
  DO ii = 1, SIZE(obj%quadratureForSpace)
    CALL DEALLOCATE (obj%quadratureForSpace(ii))
  END DO
  DEALLOCATE (obj%quadratureForSpace)
END IF

CALL DEALLOCATE (obj%quadratureForTime)

CALL FiniteElementDeallocate(obj%cellFE)
CALL FiniteElementDeallocate(obj%linCellFE)
CALL FiniteElementDeallocate(obj%facetFE)
CALL FiniteElementDeallocate(obj%linFacetFE)
CALL FiniteElementDeallocate(obj%edgeFE)
CALL FiniteElementDeallocate(obj%linEdgeFE)
CALL obj%timeFE%DEALLOCATE()
CALL obj%linTimeFE%DEALLOCATE()

obj%ipTypeForSpace = 0
obj%ipTypeForTime = 0
obj%basisTypeForSpace = 0
obj%basisTypeForTime = 0
obj%alphaForSpace = 0
obj%betaForSpace = 0
obj%lambdaForSpace = 0
obj%alphaForTime = 0
obj%betaForTime = 0
obj%lambdaForTime = 0

CALL DEALLOCATE (obj%linTimeElemSD)
CALL DEALLOCATE (obj%timeElemSD)
IF (ALLOCATED(obj%linSpaceElemSD)) THEN
  DO ii = 1, SIZE(obj%linSpaceElemSD)
    CALL DEALLOCATE (obj%linSpaceElemSD(ii))
  END DO
  DEALLOCATE (obj%linSpaceElemSD)
END IF

IF (ALLOCATED(obj%spaceElemSD)) THEN
  DO ii = 1, SIZE(obj%spaceElemSD)
    CALL DEALLOCATE (obj%spaceElemSD(ii))
  END DO
  DEALLOCATE (obj%spaceElemSD)
END IF

IF (ALLOCATED(obj%stelemsd)) THEN
  DO jj = 1, SIZE(obj%stelemsd, 2)
    DO ii = 1, SIZE(obj%stelemsd, 1)
      CALL DEALLOCATE (obj%stelemsd(ii, jj))
    END DO
  END DO
  DEALLOCATE (obj%stelemsd)
END IF

CALL DirichletBCDeallocate(obj%dbc)
CALL NeumannBCDeallocate(obj%nbc)
CALL NitscheBCDeallocate(obj%wdbc)

obj%isNitsche = .FALSE.
obj%nitscheAlpha = DEFAULT_nitscheAlpha
obj%nitscheType = Nitsche_Sym

CALL DomainConnectivityDeallocate(obj%nitscheFacetToCell)
IF (ALLOCATED(obj%nitscheLocalID)) DEALLOCATE (obj%nitscheLocalID)
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
