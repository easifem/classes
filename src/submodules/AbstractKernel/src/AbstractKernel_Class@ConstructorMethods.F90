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
! USE BaseMethod

USE InputUtility, ONLY: Input

USE FPL_Method, ONLY: Set

USE AbstractKernelParam, ONLY: TypeKernelBasisOpt, &
                               TypeKernelTimeOpt

USE QuadraturePoint_Method, ONLY: QuadraturePointNameToID

USE Display_Method, ONLY: ToString

USE CPUTime_Class, ONLY: CPUTime_

USE LinSolverFactory, ONLY: LinearSolverFactory

USE KernelComponentsMethods, ONLY: KernelOptInitiate, &
                                   KernelBCInitiate, &
                                   KernelMaterialsInitiate, &
                                   KernelOptCheckEssentialParam, &
                                   KernelBasisCheckEssentialParam, &
                                   KernelBCCheckEssentialParam, &
                                   KernelMaterialsCheckEssentialParam, &
                                   KernelOptDeallocate, &
                                   KernelFieldsDeallocate, &
                                   KernelMeshFieldsDeallocate, &
                                   KernelMaterialsDeallocate, &
                                   KernelElemshapeDataDeallocate, &
                                   KernelBCDeallocate

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                   SetAbstractKernelParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetAbstractKernelParam
CHARACTER(*), PARAMETER :: myName = "SetAbstractKernelParam()"

INTEGER(I4B), PARAMETER :: DEFAULT_tSolidMaterials = 0_I4B, &
                           DEFAULT_tOverlappedMaterials = 1_I4B

REAL(DFP), PARAMETER :: DEFAULT_nitscheAlpha = 100.0_DFP

LOGICAL(LGT), PARAMETER :: DEFAULT_isSymNitsche = .FALSE., &
                           DEFAULT_isConstantMatProp = .FALSE., &
                           DEFAULT_isIsotropic = .TRUE., &
                           DEFAULT_isIncompressible = .FALSE.

INTEGER(I4B) :: aint, ii
LOGICAL(LGT) :: abool
REAL(DFP) :: areal, areal3(3)
CHARACTER(:), ALLOCATABLE :: astr
CHARACTER(*), PARAMETER :: default_tanmatProp = "UNSYM", &
                           default_tanmatName = "MATRIX", &
                           default_outputPath = "./results/"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

! char
CALL Set(obj=param, datatype="char", prefix=prefix, &
         key="name", VALUE=name)
CALL Set(obj=param, datatype="char", prefix=prefix, &
         key="engine", VALUE=engine)
CALL Set(param, datatype="char", prefix=prefix, &
         key="domainFile", VALUE=domainFile)
CALL Set(param, datatype="char", prefix=prefix, &
         key="domainFile", VALUE=domainFile)

CALL SetBasisOpt(param=param, myname=myname, prefix=prefix, suffix="Space", &
                 baseInterpolation=baseInterpolationForSpace, &
                 baseContinuity=baseContinuityForSpace, &
                 quadratureType=quadratureTypeForSpace, &
                 ipType=ipTypeForSpace, basisType=basisTypeForSpace, &
                alpha=alphaForSpace, beta=betaForSpace, lambda=lambdaForSpace)

CALL SetBasisOpt(param=param, myname=myname, prefix=prefix, suffix="Time", &
                 baseInterpolation=baseInterpolationForTime, &
                 baseContinuity=baseContinuityForTime, &
                 quadratureType=quadratureTypeForTime, &
                 ipType=ipTypeForTime, basisType=basisTypeForTime, &
                 alpha=alphaForTime, beta=betaForTime, lambda=lambdaForTime)

astr = Input(option=tanmatProp, default=default_tanmatProp)
CALL Set(param, datatype="char", prefix=prefix, key="tanmatProp", VALUE=astr)

astr = Input(option=tanmatName, default=default_tanmatName)
CALL Set(param, datatype="char", prefix=prefix, key="tanmatName", VALUE=astr)

astr = Input(option=outputPath, default=default_outputPath)
CALL Set(param, datatype="char", prefix=prefix, key="outputPath", VALUE=astr)

!! int
CALL Set(param, datatype=problemType, prefix=prefix, key="problemType", &
         VALUE=problemType)

CALL Set(param, datatype=coordinateSystem, prefix=prefix, &
         key="coordinateSystem", VALUE=coordinateSystem)

aint = Input(option=timeDependency, default=TypeKernelTimeOpt%default)
CALL Set(param, datatype=aint, prefix=prefix, key="timeDependency", &
         VALUE=aint)

aint = Input(option=maxIter, default=TypeKernelOpt%maxIter)
CALL Set(param, datatype=aint, prefix=prefix, key="maxIter", VALUE=aint)

aint = Input(option=nsd, default=TypeKernelOpt%nsd)
CALL Set(param, datatype=aint, prefix=prefix, key="nsd", VALUE=aint)

aint = Input(option=nnt, default=TypeKernelOpt%nnt)
CALL Set(param, datatype=aint, prefix=prefix, key="nnt", VALUE=aint)

aint = Input(option=tdof, default=TypeKernelOpt%tdof)
CALL Set(param, aint, prefix, "tdof", aint)

aint = Input(option=currentTimeStep, default=TypeKernelOpt%currentTimeStep)
CALL Set(param, aint, prefix, "currentTimeStep", aint)

aint = Input(option=totalTimeStep, default=TypeKernelOpt%totalTimeStep)
CALL Set(param, aint, prefix, "totalTimeStep", aint)

aint = Input(option=postProcessOpt, default=TypeKernelOpt%postProcessOpt)
CALL Set(param, aint, prefix, "postProcessOpt", aint)

aint = Input(option=tDirichletBC, default=0_I4B)
CALL Set(param, aint, prefix, "tDirichletBC", aint)

aint = Input(option=tNeumannBC, default=0_I4B)
CALL Set(param, aint, prefix, "tNeumannBC", aint)

aint = Input(option=tPointSource, default=0_I4B)
CALL Set(param, aint, prefix, "tPointSource", aint)

aint = Input(option=tWeakDirichletBC, default=0_I4B)
CALL Set(param, aint, prefix, "tWeakDirichletBC", aint)

abool = Input(option=isSymNitsche, default=DEFAULT_isSymNitsche)
CALL Set(param, .TRUE., prefix, "isSymNitsche", abool)

aint = Input(option=tOverlappedMaterials, &
             default=DEFAULT_tOverlappedMaterials)
CALL Set(param, aint, prefix, "tOverlappedMaterials", aint)

! real
areal = Input(option=nitscheAlpha, default=DEFAULT_nitscheAlpha)
CALL Set(param, areal, prefix, "nitscheAlpha", areal)

areal = Input(default=TypeKernelOpt%dispError%rtol, &
              option=rtoleranceForDisplacement)
CALL Set(param, areal, prefix, "rtoleranceForDisplacement", areal)

areal = Input(default=TypeKernelOpt%dispError%rtol, &
              option=atoleranceForDisplacement)
CALL Set(param, areal, prefix, "atoleranceForDisplacement", areal)

areal = Input(default=TypeKernelOpt%velError%rtol, &
              option=rtoleranceForVelocity)
CALL Set(param, areal, prefix, "rtoleranceForVelocity", areal)

areal = Input(default=TypeKernelOpt%velError%rtol, &
              option=atoleranceForVelocity)
CALL Set(param, areal, prefix, "atoleranceForVelocity", areal)

areal = Input(default=TypeKernelOpt%resError%rtol, &
              option=rtoleranceForResidual)
CALL Set(param, areal, prefix, "rtoleranceForResidual", areal)

areal = Input(default=TypeKernelOpt%resError%rtol, &
              option=atoleranceForResidual)
CALL Set(param, areal, prefix, "atoleranceForResidual", areal)

!! bool
abool = Input(default=TypeKernelOpt%isCommonDomain, option=isCommonDomain)
CALL Set(param, abool, prefix, "isCommonDomain", abool)

!! real
IF (PRESENT(gravity)) THEN
  areal3 = gravity
ELSE
  areal3 = TypeKernelOpt%gravity
END IF

CALL Set(param, areal3, prefix, "gravity", areal3)

areal = Input(option=dt, default=TypeKernelOpt%dt)
CALL Set(param, areal, prefix, "dt", areal)

areal = Input(option=startTime, default=TypeKernelOpt%startTime)
CALL Set(param, areal, prefix, "startTime", areal)

areal = Input(option=endTime, default=TypeKernelOpt%endTime)
CALL Set(param, areal, prefix, "endTime", areal)

areal = Input(option=currentTime, default=TypeKernelOpt%currentTime)
CALL Set(param, areal, prefix, "currentTime", areal)

aint = Input(option=algorithm, default=TypeKernelOpt%algorithm)
CALL Set(param, aint, prefix, "algorithm", aint)

ii = 0_I4B
IF (PRESENT(materialInterfaces)) THEN
  ii = SIZE(materialInterfaces)

  IF (ii .GT. 0_I4B) THEN
    CALL Set(param, datatype=materialInterfaces, prefix=prefix, &
             key="materialInterfaces", VALUE=materialInterfaces)
  END IF

END IF
CALL Set(param, datatype=ii, prefix=prefix, key="tMaterialInterfaces", &
         VALUE=ii)

aint = Input(option=tSolidMaterials, default=DEFAULT_tSolidMaterials)
CALL Set(param, datatype=aint, prefix=prefix, key="tSolidMaterials", &
         VALUE=aint)

!bool
abool = Input(option=isConstantMatProp, &
              default=DEFAULT_isConstantMatProp)
CALL Set(param, abool, prefix, "isConstantMatProp", abool)

abool = Input(option=isIsotropic, default=DEFAULT_isIsotropic)
CALL Set(param, abool, prefix, "isIsotropic", abool)

abool = Input(option=isIncompressible, default=DEFAULT_isIncompressible)
CALL Set(param, abool, prefix, "isIncompressible", abool)

abool = Input(option=showTime, default=.FALSE.)
CALL Set(param, abool, prefix, "showTime", abool)

abool = Input(option=unifyVTK, default=.FALSE.)
CALL Set(param, abool, prefix, "unifyVTK", abool)

abool = Input(option=createPVD, default=.FALSE.)
CALL Set(param, abool, prefix, "createPVD", abool)

aint = Input(option=vtkOutputFreq, default=1)
CALL Set(param, aint, prefix, "vtkOutputFreq", aint)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif

END PROCEDURE SetAbstractKernelParam

!----------------------------------------------------------------------------
!                                                                SetBasisOpt
!----------------------------------------------------------------------------

SUBROUTINE SetBasisOpt(param, myname, prefix, suffix, baseInterpolation, &
       baseContinuity, quadratureType, ipType, basisType, alpha, beta, lambda)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  CHARACTER(*), INTENT(IN) :: myname
  CHARACTER(*), INTENT(IN) :: prefix
  CHARACTER(*), INTENT(IN) :: suffix
  CHARACTER(*), OPTIONAL, INTENT(IN) :: baseInterpolation
  CHARACTER(*), OPTIONAL, INTENT(IN) :: baseContinuity
  CHARACTER(*), OPTIONAL, INTENT(IN) :: quadratureType
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
  REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
  REAL(DFP), OPTIONAL, INTENT(IN) :: beta
  REAL(DFP), OPTIONAL, INTENT(IN) :: lambda

  !! internal variables
  CHARACTER(:), ALLOCATABLE :: astr
  INTEGER(I4B) :: aint
  REAL(DFP) :: areal
  LOGICAL(LGT) :: isok

  astr = Input(option=baseInterpolation, &
               default=TypeKernelBasisOpt%baseInterpolation)
  CALL Set(param, datatype="char", prefix=prefix, &
           key="baseInterpolationFor"//suffix, VALUE=astr)

  astr = Input(option=baseContinuity, &
               default=TypeKernelBasisOpt%baseContinuity)
  CALL Set(param, datatype="char", prefix=prefix, &
           key="baseContinuityFor"//suffix, VALUE=astr)

  astr = Input(option=quadratureType, &
               default=TypeKernelBasisOpt%quadratureType_char)
  CALL Set(param, datatype="char", prefix=prefix, &
           key="quadratureTypeFor"//suffix, VALUE=astr)

  aint = QuadraturePointNameToID(astr)

  isok = aint .GE. 0
  CALL AssertError1(isok, myname, &
                    "Error in converting quadrature point to integer")

  CALL Set(param, datatype=aint, prefix=prefix, key="quadTypeFor"//suffix, &
           VALUE=aint)

  aint = Input(option=ipType, default=TypeKernelBasisOpt%ipType)
  CALL Set(param, datatype=aint, prefix=prefix, key="ipTypeFor"//suffix, &
           VALUE=aint)

  aint = Input(option=basisType, default=TypeKernelBasisOpt%basisType)
  CALL Set(param, datatype=aint, prefix=prefix, key="basisTypeFor"//suffix, &
           VALUE=aint)

  areal = Input(option=alpha, default=TypeKernelBasisOpt%alpha)
  CALL Set(param, datatype=areal, prefix=prefix, key="alphaFor"//suffix, &
           VALUE=areal)

  areal = Input(option=beta, default=TypeKernelBasisOpt%beta)
  CALL Set(param, datatype=areal, prefix=prefix, key="betaFor"//suffix, &
           VALUE=areal)

  areal = Input(option=lambda, default=TypeKernelBasisOpt%lambda)
  CALL Set(param, datatype=areal, prefix=prefix, key="lambdaFor"//suffix, &
           VALUE=areal)
END SUBROUTINE SetBasisOpt

!----------------------------------------------------------------------------
!                                                        CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "obj_CheckEssentialParam()"
CHARACTER(:), ALLOCATABLE :: prefix0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(prefix)) THEN
  prefix0 = prefix
ELSE
  prefix0 = obj%GetPrefix()
END IF

CALL KernelOptCheckEssentialParam(param=param, prefix=prefix0, &
                                  myname=myname)

CALL KernelBasisCheckEssentialParam(param=param, prefix=prefix0, &
                                    myname=myname)

CALL KernelBCCheckEssentialParam(param=param, prefix=prefix0, &
                                 myname=myname)

CALL KernelMaterialsCheckEssentialParam(param=param, prefix=prefix0, &
                                        myname=myname)

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
                        '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
LOGICAL(LGT) :: isok

INTEGER(I4B) :: ii
CHARACTER(:), ALLOCATABLE :: astr, prefix
TYPE(CPUTime_) :: TypeCPUTime

CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif DEBUG_VER

prefix = obj%GetPrefix()

CALL KernelOptInitiate(obj=obj%opt, param=param, prefix=prefix, &
                       myname=myname)

CALL obj%CheckEssentialParam(param=param, prefix=prefix)

CALL KernelBCInitiate(obj=obj%bc, param=param, prefix=prefix, myname=myname)

CALL KernelMaterialsInitiate(obj=obj%materials, param=param, prefix=prefix, &
                             myname=myname)

IF (obj%opt%showTime) THEN
  astr = obj%opt%outputPath//obj%opt%name//"_time_stat.csv"
  CALL obj%showTimeFile%Initiate(filename=astr, status="REPLACE", &
                                 action="WRITE", separator=",")
  CALL obj%showTimeFile%OPEN()
  astr = ""
  astr = "currentTimeStep,currentTime,method,cpu-time"
!$ astr = astr//",wtime"
  CALL obj%showTimeFile%WRITE(val=astr)
END IF

IF (obj%opt%createPVD) THEN
  astr = obj%opt%outputPath//prefix//"_results.pvd"
  CALL obj%pvdFile%InitiatePVDFile(filename=astr)
END IF

! linsol
obj%linsol => LinearSolverFactory(obj%opt%engine%chars())
CALL obj%linsol%initiate(param=param)

IF (PRESENT(dom)) THEN
  obj%dom => dom
  obj%opt%isCommonDomain = .TRUE.
  obj%opt%nsd = dom%GetNSD()
END IF

IF (PRESENT(domains)) THEN
  ALLOCATE (obj%domains(SIZE(domains)))
  DO ii = 1, SIZE(domains)
    obj%domains(ii)%ptr => domains(ii)%ptr
  END DO
END IF

isok = PRESENT(domains) .OR. PRESENT(dom)
CALL AssertError1(isok, myname, &
                  "Either dom or domains should be present.")

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif

IF (obj%opt%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL obj%showTimeFile%WRITE(val=TypeCPUTime%GetStringForKernelLog( &
                              currentTime=obj%opt%currentTime, &
                              currentTimeStep=obj%opt%currentTimeStep, &
                              methodName=myName))
END IF

END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                       KernelDeallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
INTEGER(I4B) :: ii, jj, tsize
LOGICAL(LGT) :: abool

obj%dom => NULL()

abool = ALLOCATED(obj%domains)
IF (abool) THEN
  tsize = SIZE(obj%domains)
  DO ii = 1, tsize
    obj%domains(ii)%ptr => NULL()
  END DO
  DEALLOCATE (obj%domains)
END IF

CALL KernelOptDeallocate(obj%opt)
CALL KernelFieldsDeallocate(obj%fields)
CALL KernelMeshFieldsDeallocate(obj%meshFields)
CALL KernelMaterialsDeallocate(obj%materials)
CALL KernelElemshapeDataDeallocate(obj%elemsd)
CALL KernelBCDeallocate(obj%bc)

abool = ASSOCIATED(obj%fedof)
IF (abool) THEN
  CALL obj%fedof%DEALLOCATE()
END IF
obj%fedof => NULL()

abool = ASSOCIATED(obj%geofedof)
IF (abool) THEN
  CALL obj%geofedof%DEALLOCATE()
END IF
obj%geofedof => NULL()

abool = ASSOCIATED(obj%timeFE)
IF (abool) THEN
  CALL obj%timeFE%DEALLOCATE()
END IF
obj%timeFE => NULL()

abool = ASSOCIATED(obj%geoTimeFE)
IF (abool) THEN
  CALL obj%geoTimeFE%DEALLOCATE()
END IF
obj%geoTimeFE => NULL()

CALL obj%pvdFile%DEALLOCATE()

abool = ASSOCIATED(obj%linsol)
IF (abool) THEN
  CALL obj%linsol%DEALLOCATE()
END IF
obj%linsol => NULL()

abool = ASSOCIATED(obj%tanmat)
IF (abool) THEN
  CALL obj%tanmat%DEALLOCATE()
END IF
obj%tanmat => NULL()

CALL obj%showTimeFile%DEALLOCATE()

IF (ALLOCATED(obj%timeVec)) DEALLOCATE (obj%timeVec)

IF (ALLOCATED(obj%dbcIndx)) DEALLOCATE (obj%dbcIndx)

END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                           PreCheckError
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_PreCheckError
CHARACTER(*), PARAMETER :: myName = "obj_PreCheckError()"
LOGICAL(LGT) :: problem

! Check
problem = obj%materials%tOverlappedMaterials .LE. 0_I4B
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: tOverlappedMaterials is negative.')
  RETURN
END IF

! Check
problem = (.NOT. ASSOCIATED(obj%dom)) .AND. (.NOT. ALLOCATED(obj%domains))
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
      '[INTERNAL ERROR] :: dom is not associated or domains is not allocated')
  RETURN
END IF

! Check
problem = (.NOT. ASSOCIATED(obj%fedof)) .AND. (.NOT. ALLOCATED(obj%fedofs))
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
     '[INTERNAL ERROR] :: fedof is not associated or fedofs is not allocated')
  RETURN
END IF

! Check
problem = obj%opt%problemType .EQ. 0_I4B
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: problemType is not set.')
  RETURN
END IF

IF (obj%opt%showTime) THEN
  problem = .NOT. obj%showTimeFile%isOpen()
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTERNAL ERROR] :: you have set showTime=true, '// &
                      'but showTimeFile is not opened')
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
     '[INTERNAL ERROR] :: AbstractElasticity_::obj%linsol is not ASSOCIATED.')
  RETURN
END IF

isok = ASSOCIATED(obj%linsol)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
     '[INTERNAL ERROR] :: AbstractElasticity_::obj%linsol is not ASSOCIATED.')
  RETURN
END IF
END PROCEDURE obj_PostCheckError

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
