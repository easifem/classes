! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

MODULE KernelComponentsMethods
USE KernelComponents, ONLY: KernelOpt_, &
                            KernelFields_, &
                            KernelMeshFields_, &
                            KernelMaterials_, &
                            KernelElemshapeData_, &
                            KernelBC_

USE AbstractKernelParam, ONLY: KernelBasisOpt_, &
                               KernelErrorOpt_, &
                               TypeKernelProblemOpt, &
                               TypeKernelCoordOpt, &
                               TypeKernelTimeOpt, &
                               TypeKernelNitscheOpt, &
                               TypeKernelBasisOpt

USE FPL, ONLY: ParameterList_

USE FPL_Method, ONLY: CheckEssentialParam, GetValue

USE ExceptionHandler_Class, ONLY: e

USE CPUTime_Class, ONLY: CPUTime_

USE GlobalData, ONLY: DFP, I4B, LGT, NormL2, AbsoluteConvergence, &
                      ConvergenceInSol

USE String_Class, ONLY: String

USE Display_Method, ONLY: ToString

USE StringUtility, ONLY: UpperCase

USE IterationData_Method, ONLY: IterationData_Initiate => Initiate

USE AbstractMeshField_Class, ONLY: AbstractMeshFieldDeallocate

USE VectorMeshField_Class, ONLY: VectorMeshFieldDeallocate

USE MatrixField_Class, ONLY: MatrixFieldDeallocate
USE VectorField_Class, ONLY: VectorFieldDeallocate
USE ScalarField_Class, ONLY: ScalarFieldDeallocate
USE STVectorField_Class, ONLY: STVectorFieldDeallocate
USE STScalarField_Class, ONLY: STScalarFieldDeallocate

USE QuadraturePoint_Method, ONLY: QuadraturePointDeallocate => DEALLOCATE

USE IterationData_Method, ONLY: IterationDataDeallocate => DEALLOCATE

USE DomainConnectivity_Class, ONLY: DomainConnectivityDeallocate

IMPLICIT NONE
PRIVATE

PUBLIC :: KernelOptInitiate
PUBLIC :: KernelBCInitiate
PUBLIC :: KernelMaterialsInitiate
PUBLIC :: KernelOptCheckEssentialParam
PUBLIC :: KernelBasisCheckEssentialParam
PUBLIC :: KernelBCCheckEssentialParam
PUBLIC :: KernelMaterialsCheckEssentialParam

PUBLIC :: KernelOptDeallocate
PUBLIC :: KernelFieldsDeallocate
PUBLIC :: KernelMeshFieldsDeallocate
PUBLIC :: KernelMaterialsDeallocate
PUBLIC :: KernelElemshapeDataDeallocate
PUBLIC :: KernelBCDeallocate

CHARACTER(*), PARAMETER :: modName = "KernelComponentsMethods"

CONTAINS

!----------------------------------------------------------------------------
!                                                   KernelBasisOptInitiate
!----------------------------------------------------------------------------

SUBROUTINE KernelBasisOptInitiate(obj, param, prefix, suffix)
  TYPE(KernelBasisOpt_), INTENT(INOUT) :: obj
  TYPE(ParameterList_), INTENT(IN) :: param
  CHARACTER(*), INTENT(IN) :: prefix
  CHARACTER(*), INTENT(IN) :: suffix

  TYPE(String) :: astr

  CALL GetValue(param, prefix, "baseContinuityFor"//suffix, astr)
  obj%baseContinuity = UpperCase(astr%slice(1, 2))

  CALL GetValue(param, prefix, "baseInterpolationFor"//suffix, astr)
  obj%baseInterpolation = UpperCase(astr%slice(1, 4))

  CALL GetValue(param, prefix, "basisTypeFor"//suffix, obj%basisType)

  CALL GetValue(param, prefix, "quadTypeFor"//suffix, &
                obj%quadratureType)

  CALL GetValue(param, prefix, "ipTypeFor"//suffix, obj%ipType)
  CALL GetValue(param, prefix, "alphaFor"//suffix, obj%alpha)
  CALL GetValue(param, prefix, "betaFor"//suffix, obj%beta)
  CALL GetValue(param, prefix, "lambdaFor"//suffix, obj%lambda)

END SUBROUTINE KernelBasisOptInitiate

!----------------------------------------------------------------------------
!                                                           KernelBCInitiate
!----------------------------------------------------------------------------

SUBROUTINE KernelBCInitiate(obj, param, prefix, myname)
  TYPE(KernelBC_), INTENT(INOUT) :: obj
  TYPE(ParameterList_), INTENT(IN) :: param
  CHARACTER(*), INTENT(IN) :: prefix
  CHARACTER(*), INTENT(IN) :: myname

  LOGICAL(LGT) :: isok, abool
  INTEGER(I4B) :: tsize

  obj%tdbc = 0
  CALL GetValue(param, prefix, "tDirichletBC", obj%tdbc)

  abool = ALLOCATED(obj%dbc)
  IF (abool) THEN

    tsize = SIZE(obj%dbc)
    isok = obj%tdbc .EQ. tsize
    CALL AssertError1(isok, myname, &
                      "obj%dbc is allocated but the size is not enough")

  END IF

  IF (.NOT. abool) THEN
    ALLOCATE (obj%dbc(obj%tdbc))
  END IF

  obj%tnbc = 0
  CALL GetValue(param, prefix, "tNeumannBC", obj%tnbc)

  abool = ALLOCATED(obj%nbc)
  IF (abool) THEN

    tsize = SIZE(obj%nbc)
    isok = obj%tnbc .EQ. tsize
    CALL AssertError1(isok, myname, &
                      "obj%nbc is allocated but the size is not enough")

  END IF

  IF (.NOT. abool) THEN
    ALLOCATE (obj%nbc(obj%tnbc))
  END IF

  obj%tnbcPointSource = 0
  CALL GetValue(param, prefix, "tPointSource", obj%tnbcPointSource)

  abool = ALLOCATED(obj%nbcPointSource)
  IF (abool) THEN

    tsize = SIZE(obj%nbcPointSource)
    isok = obj%tnbcPointSource .EQ. tsize
    CALL AssertError1(isok, myname, &
                 "obj%nbcPointSource is allocated but the size is not enough")

  END IF

  IF (.NOT. abool) THEN
    ALLOCATE (obj%nbcPointSource(obj%tnbcPointSource))
  END IF

  obj%twdbc = 0
  CALL GetValue(param, prefix, "tWeakDirichletBC", obj%twdbc)

  abool = ALLOCATED(obj%wdbc)
  IF (abool) THEN

    tsize = SIZE(obj%wdbc)
    isok = obj%twdbc .EQ. tsize
    CALL AssertError1(isok, myname, &
                      "obj%wdbc is allocated but the size is not enough")

  END IF

  IF (.NOT. abool) THEN
    ALLOCATE (obj%wdbc(obj%twdbc))
  END IF

  obj%isNitsche = .FALSE.
  CALL GetValue(param, prefix, "isSymNitsche", abool)
  obj%NitscheType = TypeKernelNitscheOpt%SkewSym
  IF (abool) obj%NitscheType = TypeKernelNitscheOpt%Sym
  CALL GetValue(param, prefix, "nitscheAlpha", obj%nitscheAlpha)

  IF (obj%twdbc .GT. 0) obj%isNitsche = .TRUE.
END SUBROUTINE KernelBCInitiate

!----------------------------------------------------------------------------
!                                                 KernelMaterialsInitiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-09-24
! summary:  This subroutine initiates the material properties for the kernel

SUBROUTINE KernelMaterialsInitiate(obj, param, prefix, myname)
  TYPE(KernelMaterials_), INTENT(INOUT) :: obj
  TYPE(ParameterList_), INTENT(IN) :: param
  CHARACTER(*), INTENT(IN) :: prefix
  CHARACTER(*), INTENT(IN) :: myname

  INTEGER(I4B) :: tsize
  LOGICAL(LGT) :: abool, isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(obj=param, prefix=prefix, key="isConstantMatProp", &
                VALUE=obj%isConstantMatProp)

  CALL GetValue(param, prefix, "isIsotropic", obj%isIsotropic)

  CALL GetValue(param, prefix, "isIncompressible", obj%isIncompressible)

! tSolidMaterials
  obj%tSolidMaterials = 0
  CALL GetValue(param, prefix, "tSolidMaterials", obj%tSolidMaterials)

  abool = ALLOCATED(obj%solidMaterial)
  IF (abool) THEN
    tsize = SIZE(obj%solidMaterial)
    isok = obj%tSolidMaterials .EQ. tsize
    CALL AssertError1(isok, myname, &
                  "obj%solidMaterial is allocated but the size is not enough")
  END IF

  IF (.NOT. abool) THEN
    ALLOCATE (obj%solidMaterial(obj%tSolidMaterials))
  END IF

  abool = ALLOCATED(obj%solidMaterialToMesh)
  IF (abool) THEN
    tsize = SIZE(obj%solidMaterialToMesh)
    isok = obj%tSolidMaterials .EQ. tsize
    CALL AssertError1(isok, myname, &
            "obj%solidMaterialToMesh is allocated but the size is not enough")
  END IF

  IF (.NOT. abool) THEN
    ALLOCATE (obj%solidMaterialToMesh(obj%tSolidMaterials))
  END IF

  ! materialInterfaces
  obj%tMaterialInterfaces = 0
  CALL GetValue(param, prefix, "tMaterialInterfaces", obj%tMaterialInterfaces)

  abool = ALLOCATED(obj%materialInterfaces)
  IF (abool) THEN
    tsize = SIZE(obj%materialInterfaces)
    isok = obj%tMaterialInterfaces .EQ. SIZE(obj%materialInterfaces)
    CALL AssertError1(isok, myname, &
             "obj%materialInterfaces is allocated but the size is not enough")
  END IF

  IF (.NOT. abool) THEN
    ALLOCATE (obj%materialInterfaces(obj%tMaterialInterfaces))
  END IF

  abool = ALLOCATED(obj%matIfaceConnectData)
  IF (abool) THEN
  END IF

  IF (.NOT. abool) THEN
    ALLOCATE (obj%matIfaceConnectData(obj%tMaterialInterfaces))
  END IF

  obj%isMaterialInterface = .FALSE.
  IF (obj%tMaterialInterfaces .GT. 0) THEN
    obj%isMaterialInterface = .TRUE.
    CALL GetValue(param, prefix, "materialInterfaces", obj%materialInterfaces)
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE KernelMaterialsInitiate

!----------------------------------------------------------------------------
!                                                        KernelOptInitiate
!----------------------------------------------------------------------------

SUBROUTINE KernelOptInitiate(obj, param, prefix, myname)
  TYPE(KernelOpt_), INTENT(INOUT) :: obj
  TYPE(ParameterList_), INTENT(IN) :: param
  CHARACTER(*), INTENT(IN) :: prefix
  CHARACTER(*), INTENT(IN) :: myname

  !! internal variables
  REAL(DFP) :: areal

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif DEBUG_VER

  ! check
  IF (obj%isInitiated) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
       '[CONFIG ERROR] :: The object is already initiated, deallocate first!')
    RETURN
  END IF

  obj%isInitiated = .TRUE.
  ! CALL CheckEssentialParam(param, prefix)

  CALL GetValue(param, prefix, "isCommonDomain", obj%isCommonDomain)

  CALL GetValue(param, prefix, "problemType", obj%problemType)

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

  CALL KernelBasisOptInitiate(param=param, prefix=prefix, &
                              obj=obj%basisForSpace, suffix="Space")

  CALL KernelBasisOptInitiate(param=param, prefix=prefix, &
                              obj=obj%basisForTime, suffix="Time")

  CALL GetValue(param, prefix, "domainFile", obj%domainFile)
  CALL GetValue(param, prefix, "tanmatProp", obj%tanmatProp)
  CALL GetValue(param, prefix, "tanmatName", obj%tanmatName)
  CALL GetValue(param, prefix, "outputPath", obj%outputPath)

  ! iterData
  ! TODO: Initiate IterationData in a better way
  areal = obj%currentTime + obj%dt
  CALL IterationData_Initiate(obj=obj%iterdata, &
                              maxIter=obj%maxIter, &
                              convergenceType=AbsoluteConvergence, &
                              convergenceIn=ConvergenceInSol, &
                              normType=NormL2, &
                              timeAtStart=obj%currentTime, &
                              timeAtEnd=areal)

  CALL GetValue(param, prefix, "atoleranceForDisplacement", &
                obj%dispError%atol)

  CALL GetValue(param, prefix, "rtoleranceForDisplacement", &
                obj%dispError%rtol)

  CALL GetValue(param, prefix, "atoleranceForVelocity", &
                obj%velError%atol)

  CALL GetValue(param, prefix, "rtoleranceForVelocity", &
                obj%velError%rtol)

  CALL GetValue(param, prefix, "atoleranceForResidual", &
                obj%resError%atol)

  CALL GetValue(param, prefix, "rtoleranceForResidual", &
                obj%resError%rtol)

  obj%tDOF = obj%nsd * obj%nnt

  CALL GetValue(param, prefix, "showTime", obj%showTime)

  CALL GetValue(param, prefix, "unifyVTK", obj%unifyVTK)

  CALL GetValue(param, prefix, "createPVD", obj%createPVD)

  CALL GetValue(param, prefix, "vtkOutputFreq", obj%vtkOutputFreq)

  IF (obj%vtkOutputFreq .LT. 0) THEN
    obj%vtkOutputFreq = 1
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END]')
#endif

END SUBROUTINE KernelOptInitiate

!----------------------------------------------------------------------------
!                                               KernelOptCheckEssentialParam
!----------------------------------------------------------------------------

SUBROUTINE KernelOptCheckEssentialParam(param, prefix, myname)
  TYPE(ParameterList_), INTENT(IN) :: param
  CHARACTER(*), INTENT(IN) :: prefix
  CHARACTER(*), INTENT(IN) :: myname

  !! internal variables
  CHARACTER(:), ALLOCATABLE :: astr

  astr = "isCommonDomain/problemType/name/engine/coordinateSystem/maxIter/&
&timeDependency/nsd/nnt/tdof/dt/startTime/endTime/currentTime/&
&currentTimeStep/totalTimeStep/postProcessOpt/gravity/domainFile/&
&tanmatProp/tanmatName/outputPath/atoleranceForDisplacement/&
&rtoleranceForDisplacement/atoleranceForVelocity/&
&rtoleranceForVelocity/atoleranceForResidual/&
&rtoleranceForResidual/showTime/unifyVTK/createPVD/vtkOutputFreq"

  CALL CheckEssentialParam(obj=param, prefix=prefix, &
                           keys=astr, myname=myname, modname=modname)

  astr = ""

END SUBROUTINE KernelOptCheckEssentialParam

!----------------------------------------------------------------------------
!                                           KernelBasisCheckEssentialParam
!----------------------------------------------------------------------------

SUBROUTINE KernelBasisCheckEssentialParam(param, prefix, myname)
  TYPE(ParameterList_), INTENT(IN) :: param
  CHARACTER(*), INTENT(IN) :: prefix
  CHARACTER(*), INTENT(IN) :: myname

  !! internal variables
  CHARACTER(:), ALLOCATABLE :: astr

  astr = "baseContinuityForSpace/baseInterpolationForSpace/basisTypeForSpace/&
&quadTypeForSpace/ipTypeForSpace/alphaForSpace/betaForSpace/&
&lambdaForSpace/baseContinuityForTime/baseInterpolationForTime/&
&quadTypeForTime/ipTypeForTime/alphaForTime/betaForTime/lambdaForTime"

  CALL CheckEssentialParam(obj=param, prefix=prefix, &
                           keys=astr, myname=myname, modname=modname)

  astr = ""

END SUBROUTINE KernelBasisCheckEssentialParam

!----------------------------------------------------------------------------
!                                                KernelBCCheckEssentialParam
!----------------------------------------------------------------------------

SUBROUTINE KernelBCCheckEssentialParam(param, prefix, myname)
  TYPE(ParameterList_), INTENT(IN) :: param
  CHARACTER(*), INTENT(IN) :: prefix
  CHARACTER(*), INTENT(IN) :: myname

  !! internal variables
  CHARACTER(:), ALLOCATABLE :: astr

  astr = "tDirichletBC/tNeumannBC/tPointSource/tWeakDirichletBC/&
&isSymNitsche/nitscheAlpha"

  CALL CheckEssentialParam(obj=param, prefix=prefix, &
                           keys=astr, myname=myname, modname=modname)

  astr = ""

END SUBROUTINE KernelBCCheckEssentialParam

!----------------------------------------------------------------------------
!                                        KernelMaterialsCheckEssentialParam
!----------------------------------------------------------------------------

SUBROUTINE KernelMaterialsCheckEssentialParam(param, prefix, myname)
  TYPE(ParameterList_), INTENT(IN) :: param
  CHARACTER(*), INTENT(IN) :: prefix
  CHARACTER(*), INTENT(IN) :: myname

  !! internal variables
  CHARACTER(:), ALLOCATABLE :: astr

  astr = "isConstantMatProp/isIsotropic/isIncompressible/tSolidMaterials/&
&tMaterialInterfaces/materialInterfaces"

  CALL CheckEssentialParam(obj=param, prefix=prefix, &
                           keys=astr, myname=myname, modname=modname)

  astr = ""

END SUBROUTINE KernelMaterialsCheckEssentialParam

!----------------------------------------------------------------------------
!                                                   KernelErrorOptDeallocate
!----------------------------------------------------------------------------

SUBROUTINE KernelErrorOptDeallocate(obj)
  TYPE(KernelErrorOpt_), INTENT(INOUT) :: obj
  obj%atol = 1.0E-6
  obj%rtol = 1.0E-6
  obj%error0 = 0.0_DFP
  obj%error = 0.0_DFP
  obj%maxIter = 100
END SUBROUTINE KernelErrorOptDeallocate

!----------------------------------------------------------------------------
!                                                   KernelBasisOptDeallocate
!----------------------------------------------------------------------------

SUBROUTINE KernelBasisOptDeallocate(obj)
  TYPE(KernelBasisOpt_), INTENT(INOUT) :: obj
  obj%baseContinuity = TypeKernelBasisOpt%baseContinuity
  obj%baseInterpolation = TypeKernelBasisOpt%baseInterpolation
  obj%quadratureType = TypeKernelBasisOpt%quadratureType
  obj%basisType = TypeKernelBasisOpt%basisType
  obj%ipType = TypeKernelBasisOpt%ipType
  obj%alpha = TypeKernelBasisOpt%alpha
  obj%beta = TypeKernelBasisOpt%beta
  obj%lambda = TypeKernelBasisOpt%lambda
  obj%quadratureType_char = TypeKernelBasisOpt%quadratureType_char
  CALL QuadraturePointDeallocate(obj%qp)
END SUBROUTINE KernelBasisOptDeallocate

!----------------------------------------------------------------------------
!                                                      KernelOptDeallocate
!----------------------------------------------------------------------------

SUBROUTINE KernelOptDeallocate(obj)
  TYPE(KernelOpt_), INTENT(inout) :: obj

  obj%isInitiated = .FALSE.
  obj%isCommonDomain = .FALSE.
  ! obj%showTime = .FALSE.
  obj%unifyVTK = .FALSE.
  obj%createPVD = .FALSE.
  obj%problemType = TypeKernelProblemOpt%scalar
  obj%algorithm = 1
  obj%vtkOutputFreq = 0
  obj%coordinateSystem = TypeKernelCoordOpt%default
  obj%maxIter = 100
  obj%timeDependency = TypeKernelTimeOpt%default
  obj%nsd = 0
  obj%nnt = 0
  obj%tdof = 0
  obj%postProcessOpt = 1
  obj%currentTimeStep = 1
  obj%totalTimeStep = 0
  obj%normRHS = 0.0_DFP
  obj%dt = 0.0_DFP
  obj%startTime = 0.0_DFP
  obj%endTime = 0.0_DFP
  obj%currentTime = 0.0_DFP
  obj%lengthScale = 1.0_DFP
  obj%gravity = 0.0_DFP
  obj%incrementScale = 1.0_DFP

  CALL KernelErrorOptDeallocate(obj%dispError)
  CALL KernelErrorOptDeallocate(obj%velError)
  CALL KernelErrorOptDeallocate(obj%solError)
  CALL KernelErrorOptDeallocate(obj%resError)

  obj%name = ""
  obj%engine = "NATIVE_SERIAL"
  obj%tanmatProp = "UNSYM"
  obj%tanmatName = "MATRIX"
  obj%outputPath = "./results/"
  obj%domainFile = ""

  CALL KernelBasisOptDeallocate(obj%basisForSpace)
  CALL KernelBasisOptDeallocate(obj%basisForTime)
  CALL KernelBasisOptDeallocate(obj%basisForPressure)
  CALL KernelBasisOptDeallocate(obj%basisForVelocity)

  CALL IterationDataDeallocate(obj%iterData)

END SUBROUTINE KernelOptDeallocate

!----------------------------------------------------------------------------
!                                                      KernelFieldsDeallocate
!----------------------------------------------------------------------------

SUBROUTINE KernelFieldsDeallocate(obj)
  TYPE(KernelFields_), INTENT(inout) :: obj

  obj%tMatrixFields = 0
  obj%tVectorFields = 0
  obj%tScalarFields = 0

  obj%tSTMatrixFields = 0
  obj%tSTVectorFields = 0
  obj%tSTScalarFields = 0

  CALL MatrixFieldDeallocate(obj%matrixFields)
  CALL VectorFieldDeallocate(obj%vectorFields)
  CALL ScalarFieldDeallocate(obj%scalarFields)
  CALL STVectorFieldDeallocate(obj%stVectorFields)
  CALL STScalarFieldDeallocate(obj%stScalarFields)

  obj%stiffnessMat => NULL()
  obj%diffusionMat => NULL()
  obj%massMat => NULL()
  obj%dampingMat => NULL()
  obj%displacement => NULL()
  obj%velocity => NULL()
  obj%acceleration => NULL()
  obj%nodeCoord => NULL()
  obj%pressure => NULL()
  obj%p_velocity => NULL()
  obj%p_acceleration => NULL()
END SUBROUTINE KernelFieldsDeallocate

!----------------------------------------------------------------------------
!                                                  KernelMeshFieldsDeallocate
!----------------------------------------------------------------------------

SUBROUTINE KernelMeshFieldsDeallocate(obj)
  LOGICAL(LGT) :: abool

  TYPE(KernelMeshFields_), INTENT(INOUT) :: obj

  CALL AbstractMeshFieldDeallocate(obj%scalarFields)
  CALL AbstractMeshFieldDeallocate(obj%vectorFields)
  CALL AbstractMeshFieldDeallocate(obj%tensorFields)

  abool = ASSOCIATED(obj%massDensity)
  IF (abool) THEN
    CALL obj%massDensity%DEALLOCATE()
  END IF
  obj%massDensity => NULL()

  abool = ASSOCIATED(obj%shearModulus)
  IF (abool) THEN
    CALL obj%shearModulus%DEALLOCATE()
  END IF
  obj%shearModulus => NULL()

  abool = ASSOCIATED(obj%youngsModulus)
  IF (abool) THEN
    CALL obj%youngsModulus%DEALLOCATE()
  END IF
  obj%youngsModulus => NULL()

  abool = ASSOCIATED(obj%Cijkl)
  IF (abool) THEN
    CALL obj%Cijkl%DEALLOCATE()
  END IF
  obj%Cijkl => NULL()

  abool = ASSOCIATED(obj%dampCoeff_alpha)
  IF (abool) THEN
    CALL obj%dampCoeff_alpha%DEALLOCATE()
  END IF
  obj%dampCoeff_alpha => NULL()

  abool = ASSOCIATED(obj%dampCoeff_beta)
  IF (abool) THEN
    CALL obj%dampCoeff_beta%DEALLOCATE()
  END IF
  obj%dampCoeff_beta => NULL()

  abool = ASSOCIATED(obj%stress)
  IF (abool) THEN
    CALL obj%stress%DEALLOCATE()
  END IF
  obj%stress => NULL()

  abool = ASSOCIATED(obj%strain)
  IF (abool) THEN
    CALL obj%strain%DEALLOCATE()
  END IF
  obj%strain => NULL()

  abool = ASSOCIATED(obj%solidMechData)
  IF (abool) THEN
    CALL obj%solidMechData%DEALLOCATE()
  END IF
  obj%solidMechData => NULL()

  abool = ASSOCIATED(obj%phase_velocity)
  IF (abool) THEN
    CALL obj%phase_velocity%DEALLOCATE()
  END IF
  obj%phase_velocity => NULL()

  abool = ASSOCIATED(obj%scalarCoeff)
  IF (abool) THEN
    CALL obj%scalarCoeff%DEALLOCATE()
  END IF
  obj%scalarCoeff => NULL()

END SUBROUTINE KernelMeshFieldsDeallocate

!----------------------------------------------------------------------------
!                                                  KernelMaterialsDeallocate
!----------------------------------------------------------------------------

SUBROUTINE KernelMaterialsDeallocate(obj)
  TYPE(KernelMaterials_), INTENT(INOUT) :: obj

  obj%isConstantMatProp = .FALSE.
  obj%isIsotropic = .FALSE.
  obj%isIncompressible = .FALSE.
  obj%isMaterialInterface = .FALSE.
  obj%tOverlappedMaterials = 1
  obj%tSolidMaterials = 0
  obj%SOLID_MATERIAL_ID = 0
  obj%tMaterialInterfaces = 0
  CALL SolidMaterialDeallocate(obj%solidMaterial)
  CALL MeshSelectionDeallocate(obj%solidMaterialToMesh)
  CALL DomainConnectivityDeallocate(obj%matIfaceConnectData)

  IF (ALLOCATED(obj%materialInterfaces)) THEN
    DEALLOCATE (obj%materialInterfaces)
  END IF
END SUBROUTINE KernelMaterialsDeallocate

!----------------------------------------------------------------------------
!                                              KernelElemshapeDataDeallocate
!----------------------------------------------------------------------------

SUBROUTINE KernelElemshapeDataDeallocate(obj)
  TYPE(KernelElemshapeData_), INTENT(INOUT) :: obj

  ! internal variables
  INTEGER(I4B) :: ii, tsize
  LOGICAL(LGT) :: abool

  CALL ElemshapeDataDeallocate(obj%geoTimeElemSD)
  CALL ElemshapeDataDeallocate(obj%timeElemSD)
  CALL ElemshapeDataDeallocate(obj%geoSpaceElemSD)
  CALL ElemshapeDataDeallocate(obj%spaceElemSD)
  CALL ElemshapeDataDeallocate(obj%geoSpaceElemSD_facet)
  CALL ElemshapeDataDeallocate(obj%spaceElemSD_facet)

  abool = ALLOCATED(obj%stelemsd)
  IF (abool) THEN
    tsize = SIZE(obj%stelemsd)
    DO ii = 1, tsize
      CALL STElemshapeDataDeallocate(obj%stelemsd(ii))
    END DO
    DEALLOCATE (obj%stelemsd)
  END IF
END SUBROUTINE KernelElemshapeDataDeallocate

!----------------------------------------------------------------------------
!                                                      KernelBCDataDeallocate
!----------------------------------------------------------------------------

SUBROUTINE KernelBCDeallocate(obj)
  TYPE(KernelBC_), INTENT(INOUT) :: obj

  ! Internal variables

  obj%tdbc = 0
  obj%tnbc = 0
  obj%tnbcPointSource = 0
  obj%twdbc = 0
  obj%tnitscheLocalID = 0
  obj%isNitsche = .FALSE.
  obj%isSymNitsche = .FALSE.
  obj%nitscheAlpha = 100.0_DFP
  obj%nitscheType = TypeKernelNitscheOpt%Sym

  IF (ALLOCATED(obj%nitscheLocalID)) DEALLOCATE (obj%nitscheLocalID)

  CALL DirichletBCDeallocate(obj%dbc)
  CALL NeumannBCDeallocate(obj%nbc)
  CALL NeumannBCDeallocate(obj%nbcPointSource)
  CALL NitscheBCDeallocate(obj%wdbc)

  CALL DomainConnectivityDeallocate(obj%nitscheFacetToCell)

END SUBROUTINE KernelBCDeallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END MODULE KernelComponentsMethods
