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

SUBMODULE(AbstractKernel_Class) ImportTomlMethods
USE BaseMethod
USE FPL_Method
USE TomlUtility
USE tomlf, ONLY:  &
  & toml_serialize,  &
  & toml_get => get_value, &
  & toml_stat
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                         ImportParamFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportParamFromToml
CHARACTER(*), PARAMETER :: myName = "obj_ImportParamFromToml"
TYPE(toml_table), POINTER :: linsolve_toml
CLASS(AbstractLinSolver_), POINTER :: temp_linsolve_ptr
TYPE(String) :: name, engine, coordinateSystem, domainFile,  &
& timeDependency, baseInterpolationForSpace,  &
& baseContinuityForSpace,  &
& quadratureTypeForSpace, ipTypeForSpace, basisTypeForSpace, &
& baseInterpolationForTime, baseContinuityForTime, quadratureTypeForTime,  &
& ipTypeForTime, basisTypeForTime

INTEGER(I4B) :: algorithm, tMaterials, tDirichletBC, tWeakDirichletBC,  &
  & tNeumannBC, tMaterialInterfaces, origin, stat, maxIter, nsd, nnt, tdof, &
  & currentTimeStep, totalTimeStep, postProcessOpt, ii

INTEGER(I4B), ALLOCATABLE :: materialInterfaces(:)

LOGICAL(LGT) :: isConstantMatProp, isIsotropic, isIncompressible,  &
  & isSymNitsche
REAL(DFP) :: nitscheAlpha, rtoleranceForDisplacement,  &
  & atoleranceForDisplacement, rtoleranceForVelocity,  &
  & atoleranceForVelocity, rtoleranceForResidual, atoleranceForResidual,  &
  & dt, startTime, endTime, currentTime, alphaForSpace,  &
  & betaForSpace, lambdaForSpace, alphaForTime, betaForTime, lambdaForTime,  &
  & gravity(3)

REAL(DFP), ALLOCATABLE :: dummy_rvec(:)

LOGICAL(LGT) :: isCommonDomain

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

CALL toml_get(table, "baseContinuityForSpace", baseContinuityForSpace%raw,  &
  & DEFAULT_baseContinuityForSpace, origin=origin, stat=stat)

CALL toml_get(table, "baseContinuityForTime", baseContinuityForTime%raw,  &
  & DEFAULT_baseContinuityForTime, origin=origin, stat=stat)

CALL toml_get(table, "baseInterpolationForSpace",   &
  & baseInterpolationForSpace%raw, DEFAULT_baseInterpolationForSpace,  &
  & origin=origin, stat=stat)

CALL toml_get(table, "baseInterpolationForTime",   &
  & baseInterpolationForTime%raw, DEFAULT_baseInterpolationForTime,  &
  & origin=origin, stat=stat)

CALL toml_get(table, "domainFile", domainFile%raw, origin=origin, stat=stat)
IF (stat .NE. toml_stat%success) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: domainFile is missing. It should be string.')
  RETURN
END IF

CALL toml_get(table, "engine", engine%raw, default_engine, origin=origin, &
  & stat=stat)

CALL toml_get(table, "name", name%raw, obj%GetPrefix(), origin=origin, &
  & stat=stat)

CALL toml_get(table, "quadratureTypeForSpace",   &
  & quadratureTypeForSpace%raw, DEFAULT_quadratureTypeForSpace, &
  & origin=origin, stat=stat)

CALL toml_get(table, "quadratureTypeForTime", quadratureTypeForTime%raw,  &
  & DEFAULT_quadratureTypeForTime, origin=origin, stat=stat)

CALL toml_get(table, "coordinateSystem", coordinateSystem%raw, &
  & DEFAULT_CoordinateSystem_Char, origin=origin, stat=stat)

CALL toml_get(table, "currentTimeStep", currentTimeStep,  &
  & 1_I4B, origin=origin, stat=stat)

CALL toml_get(table, "maxIter", maxIter, DEFAULT_maxIter,  &
  & origin=origin, stat=stat)

CALL toml_get(table, "nnt", nnt, DEFAULT_NNT, origin=origin,  &
  & stat=stat)

CALL toml_get(table, "nsd", nsd, DEFAULT_NSD, origin=origin, stat=stat)

CALL toml_get(table, "postProcessOpt", postProcessOpt,  &
  & DEFAULT_postProcessOpt, origin=origin, stat=stat)

CALL toml_get(table, "tdof", tdof, DEFAULT_tdof, origin=origin, stat=stat)

CALL toml_get(table, "timeDependency", timeDependency%raw,  &
  & DEFAULT_TimeDependency_char, origin=origin, stat=stat)

CALL toml_get(table, "totalTimeStep", totalTimeStep,  &
  & DEFAULT_TotalTimeStep, origin=origin, stat=stat)

CALL toml_get(table, "isCommonDomain", isCommonDomain,  &
  & DEFAULT_isCommonDomain, origin=origin, stat=stat)

CALL toml_get(table, "currentTime", currentTime,  &
  & DEFAULT_currentTime, origin=origin, stat=stat)

CALL toml_get(table, "dt", dt, DEFAULT_dt, origin=origin, stat=stat)

CALL toml_get(table, "endTime", endTime,  &
  & DEFAULT_endTime, origin=origin, stat=stat)

CALL GetValue(table, "gravity", dummy_rvec,  &
  & origin=origin, stat=stat)

IF (.NOT. (ALLOCATED(dummy_rvec))) THEN
  gravity = DEFAULT_gravity
ELSE
  ii = MIN(SIZE(dummy_rvec), 3)
  gravity = 0.0_DFP
  gravity(1:ii) = dummy_rvec(1:ii)
  DEALLOCATE (dummy_rvec)
END IF

CALL toml_get(table, "startTime", startTime, DEFAULT_startTime,  &
  & origin=origin, stat=stat)

CALL toml_get(table, "ipTypeForSpace", ipTypeForSpace%raw,  &
  & DEFAULT_ipTypeForSpace_char, origin=origin, stat=stat)

CALL toml_get(table, "ipTypeForTime", ipTypeForTime%raw,  &
  & DEFAULT_ipTypeForTime_char, origin=origin, stat=stat)

CALL toml_get(table, "basisTypeForSpace", basisTypeForSpace%raw,  &
  & DEFAULT_basisTypeForSpace_char, origin=origin, stat=stat)

CALL toml_get(table, "basisTypeForTime", basisTypeForTime%raw,  &
  & DEFAULT_basisTypeForTime_char, origin=origin, stat=stat)

CALL toml_get(table, "alphaForSpace", alphaForSpace,  &
  & DEFAULT_alphaForSpace, origin=origin, stat=stat)

CALL toml_get(table, "alphaForTime", alphaForTime,  &
  & DEFAULT_alphaForTime, origin=origin, stat=stat)

CALL toml_get(table, "betaForSpace", betaForSpace,  &
  & DEFAULT_betaForSpace, origin=origin, stat=stat)

CALL toml_get(table, "betaForTime", betaForTime,  &
  & DEFAULT_betaForTime, origin=origin, stat=stat)

CALL toml_get(table, "lambdaForSpace", lambdaForSpace,  &
  & DEFAULT_lambdaForSpace, origin=origin, stat=stat)

CALL toml_get(table, "lambdaForTime", lambdaForTime,  &
  & DEFAULT_lambdaForTime, origin=origin, stat=stat)

CALL toml_get(table, "algorithm", algorithm, DEFAULT_algorithm,  &
  & origin=origin, stat=stat)

tMaterialInterfaces = 0
CALL GetValue(table=table, key="materialInterfaces",  &
  & VALUE=materialInterfaces, origin=origin, stat=stat)
IF (ALLOCATED(materialInterfaces)) THEN
  tMaterialInterfaces = SIZE(materialInterfaces)
ELSE
  tMaterialInterfaces = 0
  CALL reallocate(materialInterfaces, tMaterialInterfaces)
END IF

CALL toml_get(table, "tMaterials", tMaterials, 1_I4B, origin=origin,  &
& stat=stat)

CALL toml_get(table, "tDirichletBC", tDirichletBC, 0_I4B,  &
  & origin=origin, stat=stat)

CALL toml_get(table, "tWeakDirichletBC", tWeakDirichletBC, 0_I4B,  &
  & origin=origin, stat=stat)

CALL toml_get(table, "tNeumannBC", tNeumannBC, 0_I4B,  &
  & origin=origin, stat=stat)

CALL toml_get(table, "isConstantMatProp", isConstantMatProp, &
  & DEFAULT_isConstantMatProp, origin=origin, stat=stat)

CALL toml_get(table, "isIsotropic", isIsotropic, DEFAULT_isIsotropic, &
  & origin=origin, stat=stat)

CALL toml_get(table, "isIncompressible", isIncompressible, &
  & DEFAULT_isIncompressible, origin=origin, stat=stat)

CALL toml_get(table, "isSymNitsche", isSymNitsche,  &
  & DEFAULT_isSymNitsche, origin=origin, stat=stat)

CALL toml_get(table, "nitscheAlpha", nitscheAlpha,  &
  & DEFAULT_nitscheAlpha, origin=origin, stat=stat)

CALL toml_get(table, "rtoleranceForResidual", rtoleranceForResidual,  &
  & DEFAULT_rtoleranceForResidual, origin=origin, stat=stat)

CALL toml_get(table, "rtoleranceForDisplacement",  &
  & rtoleranceForDisplacement, DEFAULT_rtoleranceForDisplacement,  &
  & origin=origin, stat=stat)

CALL toml_get(table, "rtoleranceForVelocity", rtoleranceForVelocity,  &
  & DEFAULT_rtoleranceForVelocity, origin=origin, stat=stat)

CALL toml_get(table, "atoleranceForResidual", atoleranceForResidual,  &
  & DEFAULT_atoleranceForResidual, origin=origin, stat=stat)

CALL toml_get(table, "atoleranceForDisplacement", atoleranceForDisplacement, &
  & DEFAULT_atoleranceForDisplacement, origin=origin, stat=stat)

CALL toml_get(table, "atoleranceForVelocity", atoleranceForVelocity,  &
  & DEFAULT_atoleranceForVelocity, origin=origin, stat=stat)

! CALL Display(toml_serialize(table))

CALL SetAbstractKernelParam( &
  & param=param,  &
  & prefix=obj%GetPrefix(),  &
  & baseContinuityForSpace=baseContinuityForSpace%chars(),  &
  & baseContinuityForTime=baseContinuityForTime%chars(),  &
  & baseInterpolationForSpace=baseInterpolationForSpace%chars(),  &
  & baseInterpolationForTime=baseInterpolationForTime%chars(),  &
  & domainFile=domainFile%chars(),  &
  & engine=engine%chars(),  &
  & name=name%chars(),  &
  & quadratureTypeForSpace=quadratureTypeForSpace%chars(),  &
  & quadratureTypeForTime=quadratureTypeForTime%chars(),  &
  & coordinateSystem=typeCoordinateSystem%ToNumber(coordinateSystem//""),  &
  & currentTimeStep=currentTimeStep,  &
  & maxIter=maxIter,  &
  & nnt=nnt,  &
  & postProcessOpt=postProcessOpt,  &
  & tdof=tdof,  &
  & timeDependency=typeTimeDependency%ToNumber(timeDependency%chars()),  &
  & totalTimeStep=totalTimeStep,  &
  & isCommonDomain=isCommonDomain,  &
  & currentTime=currentTime,  &
  & dt=dt,  &
  & endTime=endTime,  &
  & gravity=gravity,  &
  & startTime=startTime,  &
  & ipTypeForSpace=BaseInterpolation_ToInteger(ipTypeForSpace%chars()),  &
  & ipTypeForTime=BaseInterpolation_ToInteger(ipTypeForTime%chars()),  &
  & basisTypeForSpace=BasisType_ToInteger(basisTypeForSpace%chars()),  &
  & basisTypeForTime=BasisType_ToInteger(basisTypeForTime%chars()),  &
  & alphaForSpace=alphaForSpace,  &
  & alphaForTime=alphaForTime,  &
  & betaForSpace=betaForSpace,  &
  & betaForTime=betaForTime,  &
  & lambdaForSpace=lambdaForSpace,  &
  & lambdaForTime=lambdaForTime, &
  & algorithm=algorithm,  &
  & isConstantMatProp=isConstantMatProp,  &
  & isIsotropic=isIsotropic,  &
  & isIncompressible=isIncompressible,  &
  & materialInterfaces=materialInterfaces,  &
  & tMaterials=tMaterials,  &
  & tDirichletBC=tDirichletBC,  &
  & tWeakDirichletBC=tWeakDirichletBC,  &
  & isSymNitsche=isSymNitsche,  &
  & nitscheAlpha=nitscheAlpha,  &
  & tNeumannBC=tNeumannBC,  &
  & rtoleranceForDisplacement=rtoleranceForDisplacement,  &
  & rtoleranceForVelocity=rtoleranceForVelocity,  &
  & rtoleranceForResidual=rtoleranceForResidual,  &
  & atoleranceForDisplacement=rtoleranceForDisplacement,  &
  & atoleranceForVelocity=rtoleranceForVelocity,  &
  & atoleranceForResidual=atoleranceForResidual)

! linesolve
linsolve_toml => NULL()
CALL toml_get(table, toml_linsolver_name, linsolve_toml, origin=origin,  &
  & requested=.FALSE., stat=stat)
IF (.NOT. ASSOCIATED(linsolve_toml)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: following error occured while reading '//  &
    & 'the toml file :: cannot find kernel.linSolver table')
  RETURN
END IF

temp_linsolve_ptr => LinearSolverFactory(engine%chars())
CALL temp_linsolve_ptr%ImportParamFromToml(param=param, table=linsolve_toml)
CALL temp_linsolve_ptr%DEALLOCATE()
NULLIFY (temp_linsolve_ptr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif
END PROCEDURE obj_ImportParamFromToml

!----------------------------------------------------------------------------
!                                                       ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
TYPE(ParameterList_) :: param
! TYPE(DomainPointer_), ALLOCATABLE :: domains(:)
TYPE(String) :: astr
TYPE(HDF5File_) :: domainFile

CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

CALL param%Initiate()
CALL obj%ImportParamFromToml(param=param, table=table)

!---------------------- make domain ----------------------------------
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] Making domain')
#endif

CALL GetValue(obj=param, prefix=obj%GetPrefix(), key="domainFile",  &
  & VALUE=astr)

CALL domainFile%Initiate(filename=astr%chars(), mode="READ")
astr = ""
CALL domainFile%OPEN()
ALLOCATE (Domain_ :: obj%dom)
CALL obj%dom%Initiate(domainFile, '')
CALL domainFile%DEALLOCATE()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] Making domain')
#endif

CALL obj%Initiate(param=param, dom=obj%dom)
CALL param%DEALLOCATE()
!---------------------- make domain ----------------------------------

CALL DirichletBCImportFromToml(table=table, dom=obj%dom,  &
  & tomlName=TOML_DIRICHLET_BC_NAME, obj=obj%dbc)

CALL NeumannBCImportFromToml(table=table, dom=obj%dom,  &
  & tomlName=TOML_NEUMANN_BC_NAME, obj=obj%nbc)

CALL NitscheBCImportFromToml(table=table, dom=obj%dom,  &
  & tomlName=TOML_NITSCHE_BC_NAME, obj=obj%wdbc)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                         ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml()"
TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

CALL GetValue(table=table, afile=afile, filename=filename)

node => NULL()
CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE.,  &
  & stat=stat)

IF (.NOT. ASSOCIATED(node)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: following error occured while reading '//  &
    & 'the toml file :: cannot find ['//tomlName//"] table in config.")
  RETURN
END IF

CALL obj%ImportFromToml(table=node)

#ifdef DEBUG_VER
IF (PRESENT(printToml)) THEN
  CALL Display(toml_serialize(node), "toml config = "//CHAR_LF,  &
    & unitNo=stdout)
END IF
#endif

NULLIFY (node)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif
END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!                                                            ExportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ExportToToml
CHARACTER(*), PARAMETER :: myName = "obj_ExportToToml"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine should be implemented by child class.')
END PROCEDURE obj_ExportToToml

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ImportTomlMethods
