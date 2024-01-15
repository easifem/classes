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
INTEGER(I4B) :: ii
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
  & VALUE=Input(option=baseInterpolationForSpace,  &
  & default=DEFAULT_baseInterpolationForSpace))

CALL Set(param, datatype="char", prefix=prefix,  &
  & key="baseContinuityForSpace",  &
  & VALUE=Input(option=baseContinuityForSpace,  &
  & default=DEFAULT_baseContinuityForSpace))

CALL Set(param, datatype="char", prefix=prefix,  &
  & key="quadratureTypeForSpace",  &
  & VALUE=Input(option=quadratureTypeForSpace,  &
  & default=DEFAULT_quadratureTypeForSpace))

aint = QuadraturePointNameToID(Input(option=quadratureTypeForSpace,  &
  & default=DEFAULT_quadratureTypeForSpace))
CALL Set(param, datatype=TypeIntI4B, prefix=prefix, key="quadTypeForSpace", &
  & VALUE=aint)

CALL Set(param, TypeIntI4B, prefix, "ipTypeForSpace",   &
  & Input(option=ipTypeForSpace, default=DEFAULT_ipTypeForSpace))

CALL Set(param, TypeIntI4B, prefix, "basisTypeForSpace",  &
  & Input(option=basisTypeForSpace, default=DEFAULT_basisTypeForSpace))

CALL Set(param, TypeDFP, prefix, "alphaForSpace",  &
& Input(default=DEFAULT_alphaForSpace, option=alphaForSpace))

CALL Set(param, TypeDFP, prefix, "betaForSpace",  &
& Input(default=DEFAULT_betaForSpace, option=betaForSpace))

CALL Set(param, TypeDFP, prefix, "lambdaForSpace",  &
& Input(default=DEFAULT_lambdaForSpace, option=lambdaForSpace))

CALL Set(param, datatype="char", prefix=prefix,  &
  & key="baseInterpolationForTime",  &
  & VALUE=Input(option=baseInterpolationForTime,  &
  & default=DEFAULT_baseInterpolationForTime))

CALL Set(param, datatype="char", prefix=prefix,  &
  & key="baseContinuityForTime",  &
  & VALUE=Input(option=basecontinuityfortime,  &
  & default=default_basecontinuityfortime))

CALL Set(param, datatype="char", prefix=prefix,  &
  & key="quadratureTypeForTime",  &
  & VALUE=Input(option=quadratureTypeForTime,  &
  & default=DEFAULT_quadratureTypeForTime))

aint = QuadraturePointNameToID(Input(option=quadratureTypeForTime,  &
  & default=DEFAULT_quadratureTypeForTime))
CALL Set(param, datatype=TypeIntI4B, prefix=prefix, key="quadTypeForTime",  &
  & VALUE=aint)

CALL Set(param, TypeIntI4B, prefix, "ipTypeForTime",  &
  & Input(option=ipTypeForTime, default=DEFAULT_ipTypeForTime))

CALL Set(param, TypeIntI4B, prefix, "basisTypeForTime",  &
  & Input(option=basisTypeForTime, default=DEFAULT_basisTypeForTime))

CALL Set(param, TypeDFP, prefix, "alphaForTime",  &
& Input(default=DEFAULT_alphaForTime, option=alphaForTime))

CALL Set(param, TypeDFP, prefix, "betaForTime",  &
& Input(default=DEFAULT_betaForTime, option=betaForTime))

CALL Set(param, TypeDFP, prefix, "lambdaForTime",  &
& Input(default=DEFAULT_lambdaForTime, option=lambdaForTime))

CALL Set(param, datatype="char", prefix=prefix, key="tanmatProp",  &
  & VALUE=Input(option=tanmatProp, default=DEFAULT_TANMAT_PROP))

CALL Set(param, datatype="char", prefix=prefix, key="outputPath",  &
  & VALUE=Input(option=outputPath, default=DEFAULT_OUTPUT_PATH))

!! int
CALL Set(param, TypeIntI4B, prefix, "problemType", problemType)
CALL Set(param, TypeIntI4B, prefix, "coordinateSystem", coordinateSystem)
CALL Set(param, TypeIntI4B, prefix, "timeDependency",  &
  & Input(option=timeDependency, default=KERNEL_STEADY))

!! NOTE: KERNEL_STEADY is defined in AbstractKernelParam module
CALL Set(param, TypeIntI4B, prefix, "maxIter",  &
  & Input(option=maxIter, default=default_maxIter))
CALL Set(param, TypeIntI4B, prefix, "nsd",  &
  & Input(option=nsd, default=0_I4B))
CALL Set(param, TypeIntI4B, prefix, "nnt",  &
  & Input(option=nnt, default=1_I4B))
CALL Set(param, TypeIntI4B, prefix, "tdof",  &
  & Input(option=tdof, default=0_I4B))
CALL Set(param, TypeIntI4B, prefix, "currentTimeStep",  &
  & Input(option=currentTimeStep, default=1_I4B))
CALL Set(param, TypeIntI4B, prefix, "totalTimeStep",  &
  & Input(option=totalTimeStep, default=1_I4B))
CALL Set(param, TypeIntI4B, prefix, "postProcessOpt", postProcessOpt)

CALL Set(param, TypeIntI4B, prefix, "tDirichletBC",  &
  & Input(option=tDirichletBC, default=0_I4B))

CALL Set(param, TypeIntI4B, prefix, "tNeumannBC",  &
  & Input(option=tNeumannBC, default=0_I4B))

CALL Set(param, TypeIntI4B, prefix, "tPointSource",  &
  & Input(option=tPointSource, default=0_I4B))

CALL Set(param, TypeIntI4B, prefix, "tWeakDirichletBC",  &
  & Input(option=tWeakDirichletBC, default=0_I4B))

CALL Set(param, .TRUE., prefix, "isSymNitsche",  &
  & Input(option=isSymNitsche, default=DEFAULT_isSymNitsche))
! INFO: DEFAULT_isSymNitsche is definedin AbstractElasticityParam module

CALL Set(param, TypeIntI4B, prefix, "tOverlappedMaterials",  &
  & Input(option=tOverlappedMaterials, default=DEFAULT_tOverlappedMaterials))

! real
CALL Set(param, TypeDFP, prefix, "nitscheAlpha",  &
  & Input(option=nitscheAlpha, default=DEFAULT_nitscheAlpha))

CALL Set(param, TypeDFP, prefix, "rtoleranceForDisplacement",  &
  & VALUE=Input(default=DEFAULT_rtoleranceForDisplacement, &
  & option=rtoleranceForDisplacement))

CALL Set(param, TypeDFP, prefix, "atoleranceForDisplacement",  &
  & VALUE=Input(default=DEFAULT_atoleranceForDisplacement, &
  & option=atoleranceForDisplacement))

CALL Set(param, TypeDFP, prefix, "rtoleranceForVelocity",  &
  & VALUE=Input(default=DEFAULT_rtoleranceForVelocity, &
  & option=rtoleranceForVelocity))

CALL Set(param, TypeDFP, prefix, "atoleranceForVelocity",  &
  & VALUE=Input(default=DEFAULT_atoleranceForVelocity, &
  & option=atoleranceForVelocity))

CALL Set(param, TypeDFP, prefix, "rtoleranceForResidual",  &
  & VALUE=Input(default=DEFAULT_rtoleranceForResidual, &
  & option=rtoleranceForResidual))

CALL Set(param, TypeDFP, prefix, "atoleranceForResidual",  &
  & VALUE=Input(default=DEFAULT_atoleranceForResidual, &
  & option=atoleranceForResidual))
! INFO: All of the above floating point default values are defined
! AbstractElasticityParam module.

!! bool
CALL Set(param, .TRUE., prefix, "isCommonDomain",  &
  & Input(default=DEFAULT_isCommonDomain, option=isCommonDomain))

!! real
CALL Set(param, [TypeDFP], prefix, "gravity",  &
  & Input(default=[0.0_DFP, 0.0_DFP, 0.0_DFP], option=gravity))
CALL Set(param, TypeDFP, prefix, "dt",  &
  & Input(option=dt, default=0.0_DFP))
CALL Set(param, TypeDFP, prefix, "startTime",  &
  & Input(option=startTime, default=0.0_DFP))
CALL Set(param, TypeDFP, prefix, "endTime",  &
  & Input(option=endTime, default=0.0_DFP))
CALL Set(param, TypeDFP, prefix, "currentTime",  &
  & Input(option=currentTime, default=0.0_DFP))

CALL Set(param, TypeIntI4B, prefix, "algorithm",  &
 & Input(option=algorithm, default=DEFAULT_algorithm))
! INFO: DEFAULT_algorithm is defined in AbstractElasticityParam module
! INFO: Set method is defined in FPL_Method

ii = 0_I4B
IF (PRESENT(materialInterfaces)) THEN
  ii = SIZE(materialInterfaces)
  IF (ii .GT. 0_I4B) THEN
    CALL Set(  &
    & param,  &
    & [TypeIntI4B],  &
    & prefix,  &
    & "materialInterfaces",  &
    & materialInterfaces)
  END IF
END IF
CALL Set(param, TypeIntI4B, prefix, "tMaterialInterfaces", ii)

CALL Set(param, TypeIntI4B, prefix, "tSolidMaterials",  &
 & Input(option=tSolidMaterials, default=1_I4B))

!bool
CALL Set(param, .TRUE., prefix, "isConstantMatProp",  &
  & Input(option=isConstantMatProp, default=DEFAULT_isConstantMatProp))
! INFO: DEFAULT_isConstantMatProp is definedin AbstractElasticityParam module

! int
CALL Set(param, .TRUE., prefix, "isIsotropic",  &
  & Input(option=isIsotropic, default=DEFAULT_isIsotropic))
! INFO: DEFAULT_isIsotropic is definedin AbstractElasticityParam module

CALL Set(param, .TRUE., prefix, "isIncompressible",  &
  & Input(option=isIncompressible, default=DEFAULT_isIncompressible))
! INFO: DEFAULT_isIncompressible is definedin AbstractElasticityParam module

CALL Set(param, .TRUE., prefix, "showTime",  &
  & Input(option=showTime, default=.FALSE.))

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

prefix0 = Input(default=obj%GetPrefix(), option=prefix)

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

IF (ASSOCIATED(obj%displacement)) &
  & CALL obj%displacement%CheckEssentialParam(param=param)

IF (ASSOCIATED(obj%velocity)) &
  & CALL obj%velocity%CheckEssentialParam(param=param)

IF (ASSOCIATED(obj%acceleration)) &
  & CALL obj%acceleration%CheckEssentialParam(param=param)

IF (ASSOCIATED(obj%nodeCoord)) &
  & CALL obj%nodeCoord%CheckEssentialParam(param=param)

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
INTEGER(I4B) :: tPointSource
INTEGER(I4B) :: tNeumannBC
LOGICAL(LGT) :: isSymNitsche
CHARACTER(:), ALLOCATABLE :: prefix, temp_str
INTEGER(I4B) :: tMaterialInterfaces
TYPE(CPUTime_) :: TypeCPUTime

CALL TypeCPUTime%SetStartTime()

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

CALL GetValue(param, prefix, "problemType", obj%problemType)
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
CALL GetValue(param, prefix, "tanmatProp", obj%tanmatProp)
CALL GetValue(param, prefix, "outputPath", obj%outputPath)
CALL GetValue(param, prefix, "tOverlappedMaterials",  &
  & obj%tOverlappedMaterials)

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

CALL GetValue(param, prefix, "tPointSource", tPointSource)
ALLOCATE (obj%nbcPointSource(tPointSource))

tWeakDirichletBC = 0
CALL GetValue(param, prefix, "tWeakDirichletBC", tWeakDirichletBC)
ALLOCATE (obj%wdbc(tWeakDirichletBC))
obj%isNitsche = .FALSE.
IF (tWeakDirichletBC .GT. 0) obj%isNitsche = .TRUE.

CALL GetValue(param, prefix, "isSymNitsche", isSymNitsche)
obj%NitscheType = Nitsche_SkewSym
IF (isSymNitsche) obj%NitscheType = Nitsche_Sym

CALL GetValue(param, prefix, "nitscheAlpha", obj%nitscheAlpha)

CALL GetValue(obj=param, prefix=prefix, key="isConstantMatProp",  &
& VALUE=obj%isConstantMatProp)
CALL GetValue(param, prefix, "isIsotropic", obj%isIsotropic)
CALL GetValue(param, prefix, "isIncompressible", obj%isIncompressible)

! tSolidMaterials
obj%tSolidMaterials = 0
CALL GetValue(param, prefix, "tSolidMaterials", obj%tSolidMaterials)
ALLOCATE (obj%solidMaterial(obj%tSolidMaterials))
ALLOCATE (obj%solidMaterialToMesh(obj%tSolidMaterials))

! materialInterfaces
tMaterialInterfaces = 0
CALL GetValue(param, prefix, "tMaterialInterfaces", tMaterialInterfaces)
ALLOCATE (obj%materialInterfaces(tMaterialInterfaces))
ALLOCATE (obj%matIfaceConnectData(tMaterialInterfaces))
obj%isMaterialInterfaces = .FALSE.
IF (tMaterialInterfaces .GT. 0) THEN
  obj%isMaterialInterfaces = .TRUE.
  CALL GetValue(param, prefix, "materialInterfaces", obj%materialInterfaces)
END IF

CALL GetValue(param, prefix, "atoleranceForDisplacement",  &
& obj%atoleranceForDisplacement)
CALL GetValue(param, prefix, "rtoleranceForDisplacement",  &
& obj%rtoleranceForDisplacement)
CALL GetValue(param, prefix, "atoleranceForVelocity", &
& obj%atoleranceForVelocity)
CALL GetValue(param, prefix, "rtoleranceForVelocity",  &
& obj%rtoleranceForVelocity)
CALL GetValue(param, prefix, "atoleranceForResidual",  &
& obj%atoleranceForResidual)
CALL GetValue(param, prefix, "rtoleranceForResidual",  &
& obj%rtoleranceForResidual)

obj%tDOF = obj%nsd * obj%nnt

CALL GetValue(param, prefix, "showTime", obj%showTime)

IF (obj%showTime) THEN
  temp_str = obj%outputPath//obj%name//"_time_stat.csv"
  CALL obj%showTimeFile%Initiate(filename=temp_str,  &
    & status="REPLACE", action="WRITE", separator=",")
  CALL obj%showTimeFile%OPEN()
  temp_str = ""
  temp_str = "currentTimeStep,currentTime,method,cpu-time"
  CALL obj%showTimeFile%WRITE(val=temp_str)
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
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                       KernelDeallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
INTEGER(I4B) :: ii, jj

obj%tOverlappedMaterials = 0
obj%outputPath = ""
obj%tanmatProp = ""
obj%problemType = 0
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
CALL DEALLOCATE (obj%refGeoTimeElem)
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

IF (ALLOCATED(obj%quadratureForSpace_facet)) THEN
  DO ii = 1, SIZE(obj%quadratureForSpace_facet)
    CALL DEALLOCATE (obj%quadratureForSpace_facet(ii))
  END DO
  DEALLOCATE (obj%quadratureForSpace_facet)
END IF

CALL DEALLOCATE (obj%quadratureForTime)

CALL FiniteElementDeallocate(obj%cellFE)
CALL FiniteElementDeallocate(obj%geoCellFE)
CALL FiniteElementDeallocate(obj%facetFE)
CALL FiniteElementDeallocate(obj%geoFacetFE)
CALL FiniteElementDeallocate(obj%edgeFE)
CALL FiniteElementDeallocate(obj%geoEdgeFE)
CALL obj%timeFE%DEALLOCATE()
CALL obj%geoTimeFE%DEALLOCATE()

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

CALL DEALLOCATE (obj%geoTimeElemSD)
CALL DEALLOCATE (obj%timeElemSD)

IF (ALLOCATED(obj%geoSpaceElemSD)) THEN
  DO ii = 1, SIZE(obj%geoSpaceElemSD)
    CALL DEALLOCATE (obj%geoSpaceElemSD(ii))
  END DO
  DEALLOCATE (obj%geoSpaceElemSD)
END IF

IF (ALLOCATED(obj%spaceElemSD)) THEN
  DO ii = 1, SIZE(obj%spaceElemSD)
    CALL DEALLOCATE (obj%spaceElemSD(ii))
  END DO
  DEALLOCATE (obj%spaceElemSD)
END IF

IF (ALLOCATED(obj%geoSpaceElemSD_facet)) THEN
  DO ii = 1, SIZE(obj%geoSpaceElemSD_facet)
    CALL DEALLOCATE (obj%geoSpaceElemSD_facet(ii))
  END DO
  DEALLOCATE (obj%geoSpaceElemSD_facet)
END IF

IF (ALLOCATED(obj%spaceElemSD_facet)) THEN
  DO ii = 1, SIZE(obj%spaceElemSD_facet)
    CALL DEALLOCATE (obj%spaceElemSD_facet(ii))
  END DO
  DEALLOCATE (obj%spaceElemSD_facet)
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
CALL NeumannBCDeallocate(obj%nbcPointSource)
CALL NitscheBCDeallocate(obj%wdbc)

obj%isNitsche = .FALSE.
obj%nitscheAlpha = DEFAULT_nitscheAlpha
obj%nitscheType = Nitsche_Sym

CALL DomainConnectivityDeallocate(obj%nitscheFacetToCell)
IF (ALLOCATED(obj%nitscheLocalID)) DEALLOCATE (obj%nitscheLocalID)

obj%isConstantMatProp = DEFAULT_isConstantMatProp
obj%isIsotropic = DEFAULT_isIsotropic
obj%isIncompressible = DEFAULT_isIncompressible
obj%isMaterialInterfaces = .FALSE.
IF (ALLOCATED(obj%materialInterfaces)) DEALLOCATE (obj%materialInterfaces)
CALL DomainConnectivityDeallocate(obj%matIfaceConnectData)
obj%tSolidMaterials = 0
obj%SOLID_MATERIAL_ID = 0

CALL SolidMaterialDeallocate(obj%solidMaterial)
CALL MeshSelectionDeallocate(obj%solidMaterialToMesh)

obj%incrementScale = 1.0_DFP
obj%rtoleranceForDisplacement = DEFAULT_rtoleranceForDisplacement
obj%atoleranceForDisplacement = DEFAULT_atoleranceForDisplacement
obj%rtoleranceForVelocity = DEFAULT_rtoleranceForVelocity
obj%atoleranceForVelocity = DEFAULT_atoleranceForVelocity
obj%rtoleranceForResidual = DEFAULT_rtoleranceForResidual
obj%atoleranceForResidual = DEFAULT_atoleranceForResidual
obj%displacementError0 = 0.0_DFP
obj%displacementError = 0.0_DFP
obj%velocityError0 = 0.0_DFP
obj%velocityError = 0.0_DFP

! matrixField
CALL MatrixFieldDeallocate(obj%matrixFields)

! VectorField
CALL VectorFieldDeallocate(obj%vectorFields)
CALL STVectorFieldDeallocate(obj%stVectorFields)

! ScalarField
CALL ScalarFieldDeallocate(obj%scalarFields)

! STScalarField
CALL STScalarFieldDeallocate(obj%stScalarFields)

! displacement
IF (ASSOCIATED(obj%displacement)) THEN
  CALL obj%displacement%DEALLOCATE()
  obj%displacement => NULL()
END IF

! velocity
IF (ASSOCIATED(obj%velocity)) THEN
  CALL obj%velocity%DEALLOCATE()
  obj%velocity => NULL()
END IF

! acceleration
IF (ASSOCIATED(obj%acceleration)) THEN
  CALL obj%acceleration%DEALLOCATE()
  obj%acceleration => NULL()
END IF

! nodeCoord
IF (ASSOCIATED(obj%nodeCoord)) THEN
  CALL obj%nodeCoord%DEALLOCATE()
  obj%nodeCoord => NULL()
END IF

! stiffnessMat
IF (ASSOCIATED(obj%stiffnessMat)) THEN
  CALL obj%stiffnessMat%DEALLOCATE()
  obj%stiffnessMat => NULL()
END IF

! massMat
IF (ASSOCIATED(obj%massMat)) THEN
  CALL obj%massMat%DEALLOCATE()
  obj%massMat => NULL()
END IF

! dampingMat
IF (ASSOCIATED(obj%dampingMat)) THEN
  CALL obj%dampingMat%DEALLOCATE()
  obj%dampingMat => NULL()
END IF

CALL VectorMeshFieldDeallocate(obj%solidMechData)
CALL AbstractMeshFieldDeallocate(obj%massDensity)
CALL AbstractMeshFieldDeallocate(obj%shearModulus)
CALL AbstractMeshFieldDeallocate(obj%youngsModulus)
CALL AbstractMeshFieldDeallocate(obj%dampCoeff_alpha)
CALL AbstractMeshFieldDeallocate(obj%dampCoeff_beta)
CALL AbstractMeshFieldDeallocate(obj%Cijkl)
CALL AbstractMeshFieldDeallocate(obj%stress)
CALL AbstractMeshFieldDeallocate(obj%strain)

NULLIFY (obj%bodySourceFunc)

obj%showTime = .FALSE.
CALL obj%showTimeFile%DEALLOCATE()

END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                           PreCheckError
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_PreCheckError
CHARACTER(*), PARAMETER :: myName = "obj_PreCheckError()"
LOGICAL(LGT) :: problem

! Check
problem = obj%tOverlappedMaterials .LE. 0_I4B
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[CONFIG ERROR] :: in tOverlappedMaterials.')
  RETURN
END IF

! Check
problem = (.NOT. ASSOCIATED(obj%dom)) .AND. (.NOT. ALLOCATED(obj%domains))
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[WRONG CONFIG] Domain_::dom is not associated')
  RETURN
END IF

! Check
problem = obj%problemType .EQ. 0_I4B
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[WRONG CONFIG] AbstractKernel_::obj%problemType is not set.')
  RETURN
END IF

IF (obj%showTime) THEN
  problem = .NOT. obj%showTimeFile%isOpen()
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: you have set showTime=true, '//  &
      & 'but showTimeFile is not opened')
    RETURN
  END IF
END IF
END PROCEDURE obj_PreCheckError

!----------------------------------------------------------------------------
!                                                           PostCheckError
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_PostCheckError
CHARACTER(*), PARAMETER :: myName = "obj_PostCheckError()"
LOGICAL(LGT) :: isok

isok = ASSOCIATED(obj%tanmat)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractElasticity_::obj%linsol is not'//  &
    & " ASSOCIATED.")
  RETURN
END IF

isok = ASSOCIATED(obj%linsol)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractElasticity_::obj%linsol is not'//  &
    & " associated.")
  RETURN
END IF
END PROCEDURE obj_PostCheckError

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
