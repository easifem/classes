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

SUBMODULE(AbstractKernel_Class) MaterialMethods

USE CPUTime_Class, ONLY: CPUTime_

USE SolidMaterial_Class, ONLY: AddSolidMaterial, &
                               GetSolidMaterialPointer

USE Display_Method, ONLY: ToString

USE FieldFactory, ONLY: ScalarMeshFieldFactory, &
                        TensorMeshFieldFactory

USE KernelScalarProperty_Method, ONLY: KernelInitiateScalarProperty, &
                                       KernelSetScalarProperty

USE KernelTensorProperty_Method, ONLY: KernelInitiateTensorProperty, &
                                       KernelSetTensorProperty

USE MeshSelection_Class, ONLY: MeshSelectionSet

USE AbstractMesh_Class, ONLY: AbstractMesh_

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                  InitiateConstantMatProps
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateMaterialProperties
CHARACTER(*), PARAMETER :: myName = "obj_InitiateMaterialProperties"
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  'child classes')
END PROCEDURE obj_InitiateMaterialProperties

!----------------------------------------------------------------------------
!                                                         AddSolidMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddSolidMaterial
CHARACTER(*), PARAMETER :: myName = "obj_AddSolidMaterial()"
LOGICAL(LGT) :: isok

TYPE(CPUTime_) :: TypeCPUTime

IF (obj%opt%showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif DEBUG_VER

#ifdef DEBUG_VER

isok = ALLOCATED(obj%materials%solidMaterialToMesh)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
           '[INTERNAL ERROR] :: AbstractKernel_::obj%solidMaterialToMesh '// &
                    'is not allocated!')
  RETURN
END IF

isok = ALLOCATED(obj%materials%solidMaterial)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                 '[INTERNAL ERROR] :: AbstractKernel_::obj%solidMaterial '// &
                    'is not allocated!')
  RETURN
END IF

#endif

CALL AddSolidMaterial(obj=obj%materials%solidMaterial, &
                      tMaterials=obj%materials%tSolidMaterials, &
                      materialNo=materialNo, param=param, &
                      materialName=materialName, region=region, &
                      solidMaterialToMesh=obj%materials%solidMaterialToMesh)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

IF (obj%opt%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL obj%showTimeFile%WRITE(val=TypeCPUTime%GetStringForKernelLog( &
                              currentTime=obj%opt%currentTime, &
                              currentTimeStep=obj%opt%currentTimeStep, &
                              methodName=myName))
END IF

END PROCEDURE obj_AddSolidMaterial

!----------------------------------------------------------------------------
!                                                   GetSolidMaterialPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSolidMaterialPointer
CHARACTER(*), PARAMETER :: myName = "obj_GetSolidMaterialPointer()"
LOGICAL(LGT) :: isok
TYPE(CPUTime_) :: TypeCPUTime

IF (obj%opt%showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans => NULL()

isok = ALLOCATED(obj%materials%solidMaterialToMesh)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
           '[INTERNAL ERROR] :: AbstractKernel_::obj%solidMaterialToMesh '// &
                    'is not allocated!')
  RETURN
END IF

ans => GetSolidMaterialPointer(obj=obj%materials%solidMaterial, &
                               materialNo=materialNo)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

IF (obj%opt%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL obj%showTimeFile%WRITE(val=TypeCPUTime%GetStringForKernelLog( &
                              currentTime=obj%opt%currentTime, &
                              currentTimeStep=obj%opt%currentTimeStep, &
                              methodName=myName))
END IF
END PROCEDURE obj_GetSolidMaterialPointer

!----------------------------------------------------------------------------
!                                                       InitiateMassDensity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateMassDensity
CHARACTER(*), PARAMETER :: myName = "obj_InitiateMassDensity()"
LOGICAL(LGT) :: isok
TYPE(CPUTime_) :: TypeCPUTime
CHARACTER(:), ALLOCATABLE :: astr

IF (obj%opt%showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%raiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

isok = ASSOCIATED(obj%dom)
CALL AssertError1(isok, myname, &
                  'AbstractKernel_::obj%dom is not ASSOCIATED.')

obj%meshfields%massDensity => NULL()

IF (obj%opt%nnt .EQ. 1) THEN
  astr = "SCALAR"
ELSE
  astr = "STSCALAR"
  isok = obj%meshfields%massDensityMaxNNT .GT. 0
  CALL AssertError1(isok, myname, &
"AbstractKernel_::obj%meshfields%massDensityMaxNNT is not greater than zero.")
END IF

obj%meshfields%massDensity => ScalarMeshFieldFactory(name=astr, &
                                                engine=obj%opt%engine%chars())

isok = obj%meshfields%massDensityMaxNNS .GT. 0
CALL AssertError1(isok, myname, &
"AbstractKernel_::obj%meshfields%massDensityMaxNNS is not greater than zero.")

CALL KernelInitiateScalarProperty(prop=obj%meshfields%massDensity, &
                                  dom=obj%dom, &
                                  maxNNS=obj%meshfields%massDensityMaxNNS, &
                                  maxNNT=obj%meshfields%massDensityMaxNNT, &
                                  propname="massDensity", &
                                  engine=obj%opt%engine%chars(), &
                              fieldType=obj%meshfields%massDensityFieldType, &
                                  varType=obj%meshfields%massDensityVarType, &
                                  defineOn=obj%meshfields%massDensityDefineOn)

#ifdef DEBUG_VER
CALL e%raiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif

IF (obj%opt%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL obj%showTimeFile%WRITE(val=TypeCPUTime%GetStringForKernelLog( &
                              currentTime=obj%opt%currentTime, &
                              currentTimeStep=obj%opt%currentTimeStep, &
                              methodName=myName))
END IF
END PROCEDURE obj_InitiateMassDensity

!----------------------------------------------------------------------------
!                                               InitiateElasticityProperties
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateElasticityProperties
CHARACTER(*), PARAMETER :: myName = "obj_InitiateElasticityProperties()"
LOGICAL(LGT) :: isok
TYPE(CPUTime_) :: TypeCPUTime
CHARACTER(:), ALLOCATABLE :: astr

IF (obj%opt%showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%raiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

isok = ALLOCATED(obj%materials%solidMaterial)
CALL AssertError1(isok, myname, &
                  'AbstractKernel_::obj%solidMaterial is not allocated.')

isok = ASSOCIATED(obj%dom)
CALL AssertError1(isok, myname, &
                  'AbstractKernel_::obj%dom is not ASSOCIATED.')

obj%meshfields%youngsModulus => NULL()

IF (obj%opt%nnt .EQ. 1) THEN
  astr = "SCALAR"

ELSE
  astr = "STSCALAR"

  isok = obj%meshfields%youngsModulusMaxNNT .GT. 0
  CALL AssertError1(isok, myname, &
"AbstractKernel_::obj%meshfields%youngsModulusMaxNNT is not greater than zero.")

  isok = obj%meshfields%shearModulusMaxNNT .GT. 0
  CALL AssertError1(isok, myname, &
"AbstractKernel_::obj%meshfields%shearModulusMaxNNT is not greater than zero.")

  isok = obj%meshfields%cijklMaxNNT .GT. 0
  CALL AssertError1(isok, myname, &
      "AbstractKernel_::obj%meshfields%cijklMaxNNT is not greater than zero.")

END IF

obj%meshfields%youngsModulus => ScalarMeshFieldFactory(name=astr, &
                                                engine=obj%opt%engine%chars())

obj%meshfields%shearModulus => ScalarMeshFieldFactory(name=astr, &
                                                engine=obj%opt%engine%chars())

obj%meshfields%cijkl => TensorMeshFieldFactory(name=astr, &
                                               engine=obj%opt%engine%chars())

isok = obj%meshfields%youngsModulusMaxNNS .GT. 0
CALL AssertError1(isok, myname, &
"AbstractKernel_::obj%meshfields%youngsModulusMaxNNS is not greater than zero.")

isok = obj%meshfields%shearModulusMaxNNS .GT. 0
CALL AssertError1(isok, myname, &
"AbstractKernel_::obj%meshfields%shearModulusMaxNNS is not greater than zero.")

isok = obj%meshfields%cijklMaxNNS .GT. 0
CALL AssertError1(isok, myname, &
"AbstractKernel_::obj%meshfields%shearModulusMaxNNS is not greater than zero.")

isok = obj%meshfields%cijklDim1 .GT. 0
CALL AssertError1(isok, myname, &
        "AbstractKernel_::obj%meshfields%cijklDim1 is not greater than zero.")

isok = obj%meshfields%cijklDim2 .GT. 0
CALL AssertError1(isok, myname, &
        "AbstractKernel_::obj%meshfields%cijklDim2 is not greater than zero.")

CALL KernelInitiateScalarProperty(prop=obj%meshfields%youngsModulus, &
                     dom=obj%dom, maxNNS=obj%meshfields%youngsModulusMaxNNS, &
        maxNNT=obj%meshfields%youngsModulusMaxNNT, propname="youngsModulus", &
                                  engine=obj%opt%engine%chars(), &
                            fieldType=obj%meshfields%youngsModulusFieldType, &
                                varType=obj%meshfields%youngsModulusVarType, &
                                defineOn=obj%meshfields%youngsModulusDefineOn)

CALL KernelInitiateScalarProperty(prop=obj%meshfields%shearModulus, &
                      dom=obj%dom, maxNNS=obj%meshfields%shearModulusMaxNNS, &
                                  maxNNT=obj%meshfields%shearModulusMaxNNT, &
                                  propname="shearModulus", &
                                  engine=obj%opt%engine%chars(), &
                             fieldType=obj%meshfields%shearModulusFieldType, &
                                 varType=obj%meshfields%shearModulusVarType, &
                                 defineOn=obj%meshfields%shearModulusDefineOn)

CALL KernelInitiateTensorProperty(prop=obj%meshfields%cijkl, &
                             dom=obj%dom, maxNNS=obj%meshfields%cijklMaxNNS, &
                                  maxNNT=obj%meshfields%cijklMaxNNT, &
                                  propname="cijkl", &
                                  engine=obj%opt%engine%chars(), &
                                  fieldType=obj%meshfields%cijklFieldType, &
                                  varType=obj%meshfields%cijklVarType, &
                                  defineOn=obj%meshfields%cijklDefineOn, &
                                  dim1=obj%meshfields%cijklDim1, &
                                  dim2=obj%meshfields%cijklDim2)

! CALL KernelInitiateConstantElasticityProperties( &
!   youngsModulus=obj%meshfields%youngsModulus, &
!   shearModulus=obj%meshfields%shearModulus, &
!   cijkl=obj%meshfields%cijkl, &
!   dom=obj%dom, nnt=obj%opt%nnt, engine=obj%opt%engine%chars())

#ifdef DEBUG_VER
CALL e%raiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif

IF (obj%opt%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL obj%showTimeFile%WRITE(val=TypeCPUTime%GetStringForKernelLog( &
                              currentTime=obj%opt%currentTime, &
                              currentTimeStep=obj%opt%currentTimeStep, &
                              methodName=myName))
END IF

END PROCEDURE obj_InitiateElasticityProperties

!----------------------------------------------------------------------------
!                                                 InitiateDampingProperties
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateDampingProperties
CHARACTER(*), PARAMETER :: myName = "obj_InitiateDampingProperties()"
LOGICAL(LGT) :: isok
TYPE(CPUTime_) :: TypeCPUTime
CHARACTER(:), ALLOCATABLE :: astr

IF (obj%opt%showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%raiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

isok = ASSOCIATED(obj%dom)
CALL AssertError1(isok, myname, &
                  'AbstractKernel_::obj%dom is not ASSOCIATED.')

obj%meshfields%dampCoeff_alpha => NULL()
obj%meshfields%dampCoeff_beta => NULL()

IF (obj%opt%nnt .EQ. 1) THEN
  astr = "SCALAR"
ELSE
  astr = "STSCALAR"
  isok = obj%meshfields%dampCoeffMaxNNT .GT. 0
  CALL AssertError1(isok, myname, &
  "AbstractKernel_::obj%meshfields%dampCoeffMaxNNT is not greater than zero.")
END IF

obj%meshfields%dampCoeff_alpha => ScalarMeshFieldFactory(name=astr, &
                                                engine=obj%opt%engine%chars())

obj%meshfields%dampCoeff_beta => ScalarMeshFieldFactory(name=astr, &
                                                engine=obj%opt%engine%chars())

isok = obj%meshfields%dampCoeffMaxNNS .GT. 0
CALL AssertError1(isok, myname, &
  "AbstractKernel_::obj%meshfields%dampCoeffMaxNNS is not greater than zero.")

CALL KernelInitiateScalarProperty(prop=obj%meshfields%dampCoeff_alpha, &
                                  dom=obj%dom, &
                                  maxNNS=obj%meshfields%dampCoeffMaxNNS, &
                                  maxNNT=obj%meshfields%dampCoeffMaxNNT, &
                                  propname="rayleigh_alpha", &
                                  engine=obj%opt%engine%chars(), &
                                fieldType=obj%meshfields%dampCoeffFieldType, &
                                  varType=obj%meshfields%dampCoeffVarType, &
                                  defineOn=obj%meshfields%dampCoeffDefineOn)

CALL KernelInitiateScalarProperty(prop=obj%meshfields%dampCoeff_beta, &
                                  dom=obj%dom, &
                                  maxNNS=obj%meshfields%dampCoeffMaxNNS, &
                                  maxNNT=obj%meshfields%dampCoeffMaxNNT, &
                                  propname="rayleigh_beta", &
                                  engine=obj%opt%engine%chars(), &
                                fieldType=obj%meshfields%dampCoeffFieldType, &
                                  varType=obj%meshfields%dampCoeffVarType, &
                                  defineOn=obj%meshfields%dampCoeffDefineOn)

#ifdef DEBUG_VER
CALL e%raiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif

IF (obj%opt%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL obj%showTimeFile%WRITE(val=TypeCPUTime%GetStringForKernelLog( &
                              currentTime=obj%opt%currentTime, &
                              currentTimeStep=obj%opt%currentTimeStep, &
                              methodName=myName))
END IF
END PROCEDURE obj_InitiateDampingProperties

!----------------------------------------------------------------------------
!                                                  InitiateScalarCoefficient
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateScalarCoefficient
CHARACTER(*), PARAMETER :: myName = "obj_InitiateScalarCoefficient()"
LOGICAL(LGT) :: isok
CHARACTER(:), ALLOCATABLE :: varname0, astr
TYPE(CPUTime_) :: TypeCPUTime

IF (obj%opt%showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%raiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

IF (PRESENT(varname)) THEN
  varname0 = varname
ELSE
  varname0 = "scalarCoeff"
END IF

isok = ASSOCIATED(obj%dom)
CALL AssertError1(isok, myname, &
                  'AbstractKernel_::obj%dom is not ASSOCIATED.')

obj%meshfields%scalarCoeff => NULL()

IF (obj%opt%nnt .EQ. 1) THEN
  astr = "SCALAR"
ELSE
  astr = "STSCALAR"
  isok = obj%meshfields%scalarCoeffMaxNNT .GT. 0
  CALL AssertError1(isok, myname, &
"AbstractKernel_::obj%meshfields%scalarCoeffMaxNNT is not greater than zero.")
END IF

obj%meshfields%scalarCoeff => ScalarMeshFieldFactory(name=astr, &
                                                engine=obj%opt%engine%chars())

isok = obj%meshfields%scalarCoeffMaxNNS .GT. 0
CALL AssertError1(isok, myname, &
"AbstractKernel_::obj%meshfields%scalarCoeffMaxNNS is not greater than zero.")

CALL KernelInitiateScalarProperty(prop=obj%meshfields%scalarCoeff, &
                                  dom=obj%dom, &
                                  maxNNS=obj%meshfields%scalarCoeffMaxNNS, &
                                  maxNNT=obj%meshfields%scalarCoeffMaxNNT, &
                                  propname="scalarCoeff", &
                                  engine=obj%opt%engine%chars(), &
                              fieldType=obj%meshfields%scalarCoeffFieldType, &
                                  varType=obj%meshfields%scalarCoeffVarType, &
                                  defineOn=obj%meshfields%scalarCoeffDefineOn)

#ifdef DEBUG_VER
CALL e%raiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif

IF (obj%opt%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL obj%showTimeFile%WRITE(val=TypeCPUTime%GetStringForKernelLog( &
                              currentTime=obj%opt%currentTime, &
                              currentTimeStep=obj%opt%currentTimeStep, &
                              methodName=myName))
END IF
END PROCEDURE obj_InitiateScalarCoefficient

!----------------------------------------------------------------------------
!                                                             SetMassDensity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMassDensity
CHARACTER(*), PARAMETER :: myName = "obj_SetMassDensity()"
TYPE(CPUTime_) :: TypeCPUTime

IF (obj%opt%showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%raiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL KernelSetScalarProperty(prop=obj%meshfields%massDensity, &
      materials=obj%materials%solidMaterial, dom=obj%dom, times=obj%timeVec, &
               propname="massDensity", medium=obj%materials%SOLID_MATERIAL_ID)

#ifdef DEBUG_VER
CALL e%raiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif

IF (obj%opt%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL obj%showTimeFile%WRITE(val=TypeCPUTime%GetStringForKernelLog( &
                              currentTime=obj%opt%currentTime, &
                              currentTimeStep=obj%opt%currentTimeStep, &
                              methodName=myName))
END IF
END PROCEDURE obj_SetMassDensity

!----------------------------------------------------------------------------
!                                                    SetElasticityProperties
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetElasticityProperties
CHARACTER(*), PARAMETER :: myName = "obj_SetElasticityProperties()"
TYPE(CPUTime_) :: TypeCPUTime

IF (obj%opt%showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL KernelSetScalarProperty(prop=obj%meshfields%youngsModulus, &
      materials=obj%materials%solidMaterial, dom=obj%dom, times=obj%timeVec, &
             propname="youngsModulus", medium=obj%materials%SOLID_MATERIAL_ID)

CALL KernelSetScalarProperty(prop=obj%meshfields%shearModulus, &
      materials=obj%materials%solidMaterial, dom=obj%dom, times=obj%timeVec, &
              propname="shearModulus", medium=obj%materials%SOLID_MATERIAL_ID)

CALL KernelSetTensorProperty(prop=obj%meshfields%cijkl, &
      materials=obj%materials%solidMaterial, dom=obj%dom, times=obj%timeVec, &
                     propname="cijkl", medium=obj%materials%SOLID_MATERIAL_ID)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

IF (obj%opt%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL obj%showTimeFile%WRITE(val=TypeCPUTime%GetStringForKernelLog( &
   currentTime=obj%opt%currentTime, currentTimeStep=obj%opt%currentTimeStep, &
                              methodName=myName))
END IF
END PROCEDURE obj_SetElasticityProperties

!----------------------------------------------------------------------------
!                                                       SetDampingProperties
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetDampingProperties
CHARACTER(*), PARAMETER :: myName = "obj_SetDampingProperties()"
TYPE(CPUTime_) :: TypeCPUTime

IF (obj%opt%showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL KernelSetScalarProperty(prop=obj%meshfields%dampCoeff_alpha, &
      materials=obj%materials%solidMaterial, dom=obj%dom, times=obj%timeVec, &
            propname="rayleigh_alpha", medium=obj%materials%SOLID_MATERIAL_ID)

CALL KernelSetScalarProperty(prop=obj%meshfields%dampCoeff_beta, &
      materials=obj%materials%solidMaterial, dom=obj%dom, times=obj%timeVec, &
             propname="rayleigh_beta", medium=obj%materials%SOLID_MATERIAL_ID)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

IF (obj%opt%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL obj%showTimeFile%WRITE(val=TypeCPUTime%GetStringForKernelLog( &
                              currentTime=obj%opt%currentTime, &
                              currentTimeStep=obj%opt%currentTimeStep, &
                              methodName=myName))
END IF
END PROCEDURE obj_SetDampingProperties

!----------------------------------------------------------------------------
!                                                        SetScalarCoefficient
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetScalarCoefficient
CHARACTER(*), PARAMETER :: myName = "obj_SetScalarCoefficient()"
TYPE(CPUTime_) :: TypeCPUTime
CHARACTER(:), ALLOCATABLE :: varname0

IF (obj%opt%showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%raiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

IF (PRESENT(varname)) THEN
  varname0 = varname
ELSE
  varname0 = "scalarCoeff"
END IF

CALL KernelSetScalarProperty(prop=obj%meshfields%scalarCoeff, &
      materials=obj%materials%solidMaterial, dom=obj%dom, times=obj%timeVec, &
                    propname=varname0, medium=obj%materials%SOLID_MATERIAL_ID)

#ifdef DEBUG_VER
CALL e%raiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif

IF (obj%opt%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL obj%showTimeFile%WRITE(val=TypeCPUTime%GetStringForKernelLog( &
                              currentTime=obj%opt%currentTime, &
                              currentTimeStep=obj%opt%currentTimeStep, &
                              methodName=myName))
END IF
END PROCEDURE obj_SetScalarCoefficient

!----------------------------------------------------------------------------
!                                                       SetConstantMatProps
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMaterialProperties
CHARACTER(*), PARAMETER :: myName = "obj_SetConstantMatProp"
TYPE(CPUTime_) :: TypeCPUTime

IF (obj%opt%showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL e%RaiseError(modName//'::'//myName//' - '// &
     '[INTERNAL ERROR] :: This routine is should be implemented by subclass.')

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

IF (obj%opt%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL obj%showTimeFile%WRITE(val=TypeCPUTime%GetStringForKernelLog( &
                              currentTime=obj%opt%currentTime, &
                              currentTimeStep=obj%opt%currentTimeStep, &
                              methodName=myName))
END IF
END PROCEDURE obj_SetMaterialProperties

!----------------------------------------------------------------------------
!                                                       SetMaterialToDomain
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMaterialToDomain
CHARACTER(*), PARAMETER :: myName = "obj_SetMaterialToDomain()"
INTEGER(I4B) :: ii, kk, jj, nsd
LOGICAL(LGT) :: isok
TYPE(CPUTime_) :: TypeCPUTime
CLASS(AbstractMesh_), POINTER :: mesh

IF (obj%opt%showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER

isok = ALLOCATED(obj%materials%solidMaterialToMesh)
CALL AssertError1(isok, myname, &
                 'AbstractKernel_::obj%solidMaterialToMesh is not allocated.')

isok = ASSOCIATED(obj%dom)
CALL AssertError1(isok, myname, &
                  'AbstractKernel_::obj%dom is not ASSOCIATED.')

#endif

CALL MeshSelectionSet(obj%materials%solidMaterialToMesh)

#ifdef DEBUG_VER

isok = obj%materials%tOverlappedMaterials .GT. 0
CALL AssertError1(isok, myname, &
"AbstractKernel_::obj%materials%tOverlappedMaterials is not greater than zero.")

#endif

nsd = obj%dom%GetNSD()
mesh => obj%dom%GetMeshPointer(dim=nsd)

#ifdef DEBUG_VER
isok = ASSOCIATED(mesh)
CALL AssertError1(isok, myname, 'mesh is not ASSOCIATED.')
#endif

CALL mesh%SetTotalMaterial(n=obj%materials%tOverlappedMaterials)
! Set total number of materials to the mesh

obj%materials%SOLID_MATERIAL_ID = mesh%GetTotalMaterial(globalElement=1_I4B, &
                                                        islocal=.TRUE.)

#ifdef DEBUG_VER

isok = obj%materials%SOLID_MATERIAL_ID .GT. 0
CALL AssertError1(isok, myname, &
 "AbstractKernel_::obj%materials%SOLID_MATERIAL_ID is not greater than zero.")

#endif

!! Set material in the mesh
DO ii = 1, obj%materials%tSolidMaterials
  CALL obj%materials%SolidMaterialToMesh(ii)%SetMaterialToMesh(dom=obj%dom, &
                 dim=nsd, medium=obj%materials%SOLID_MATERIAL_ID, material=ii)
END DO

NULLIFY (mesh)

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

END PROCEDURE obj_SetMaterialToDomain

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE MaterialMethods
