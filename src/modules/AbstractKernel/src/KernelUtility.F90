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

MODULE KernelUtility
USE GlobalData
USE Field
USE BaseMethod
USE BaseType
USE AbstractLinSolver_Class
USE FieldFactory
USE SolidMaterial_Class
USE Domain_Class
USE Mesh_Class
USE FiniteElement_Class
USE BaseMethod, ONLY: MassMatrix
USE ExceptionHandler_Class, ONLY: e
USE AbstractKernelParam, ONLY: kernelProblemType
USE UserFunction_Class
IMPLICIT NONE
PRIVATE

CHARACTER(*), PARAMETER :: modName = "KernelUtility"

PUBLIC :: KernelInitiateScalarProperty
PUBLIC :: KernelInitiateTensorProperty
PUBLIC :: KernelInitiateConstantElasticityProperties
PUBLIC :: KernelSetScalarProperty
PUBLIC :: KernelSetTensorProperty
PUBLIC :: KernelInitiateSpaceMatrix
PUBLIC :: KernelInitiateTangentMatrix
PUBLIC :: KernelAssembleMassMatrix
PUBLIC :: KernelAssembleStiffnessMatrix
PUBLIC :: KernelAssembleBodyForce

INTERFACE KernelInitiateScalarProperty
  MODULE PROCEDURE KernelInitiateScalarProperty1
END INTERFACE KernelInitiateScalarProperty

INTERFACE KernelInitiateTensorProperty
  MODULE PROCEDURE KernelInitiateTensorProperty1
END INTERFACE KernelInitiateTensorProperty

INTERFACE KernelSetScalarProperty
  MODULE PROCEDURE KernelSetScalarProperty1
END INTERFACE KernelSetScalarProperty

INTERFACE KernelSetTensorProperty
  MODULE PROCEDURE KernelSetTensorProperty1
END INTERFACE KernelSetTensorProperty

INTERFACE KernelAssembleStiffnessMatrix
  MODULE PROCEDURE KernelAssembleIsoStiffMat
  MODULE PROCEDURE KernelAssembleCijklStiffMat
END INTERFACE KernelAssembleStiffnessMatrix

INTERFACE KernelAssembleBodyForce
  MODULE PROCEDURE KernelAssembleBodyForce1
  MODULE PROCEDURE KernelAssembleBodyForce2
END INTERFACE KernelAssembleBodyForce

CONTAINS

!----------------------------------------------------------------------------
!                                                 InitiateScalarProperty
!----------------------------------------------------------------------------

SUBROUTINE KernelInitiateScalarProperty1(vars, materials, dom, nnt,  &
  & varname, matid, engine)
  TYPE(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: vars(:)
  TYPE(SolidMaterialPointer_), INTENT(INOUT) :: materials(:)
  TYPE(Domain_), INTENT(INOUT) :: dom
  INTEGER(I4B), INTENT(IN) :: nnt
  CHARACTER(*), INTENT(IN) :: varname
  INTEGER(I4B), INTENT(IN) :: matid
  CHARACTER(*), INTENT(IN) :: engine

  ! Define internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelInitiateScalarProperty1()"
  LOGICAL(LGT) :: isok, problem
  CLASS(Mesh_), POINTER :: amesh
  CLASS(SolidMaterial_), POINTER :: material
  CLASS(AbstractScalarMeshField_), POINTER :: var
  INTEGER(I4B) :: nsd, tmesh, ii, id
  CHARACTER(:), ALLOCATABLE :: name

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  amesh => NULL()
  material => NULL()
  var => NULL()

  nsd = dom%GetNSD()
  tmesh = dom%GetTotalMesh(dim=nsd)

  name = "STScalar"
  IF (nnt .EQ. 1) name = "Scalar"

  DO ii = 1, tmesh
    amesh => dom%GetMeshPointer(dim=nsd, entityNum=ii)
    isok = ASSOCIATED(amesh)

    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: mesh('//tostring(ii)//') not ASSOCIATED.')
      RETURN
    END IF

    problem = amesh%isEmpty()
    IF (problem) THEN
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        & 'mesh('//tostring(ii)//') is EMPTY.')
      CYCLE
    END IF

    id = amesh%GetMaterial(matid)
    IF (id .EQ. 0_I4B) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: For mesh('//tostring(ii)//') found  id = 0.')
      RETURN
    END IF

    material => materials(id)%ptr

    isok = ASSOCIATED(material)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: AbstractKernel_::solidMaterial('//  &
        & tostring(ii)//') is not ASSOCIATED.')
      RETURN
    END IF

    isok = material%IsMaterialPresent(name=varname)
    IF (.NOT. isok) THEN
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        & 'material name '//varname//" NOT FOUND.")
      ! vars(ii)%ptr => NULL()
    END IF

    IF (isok) THEN
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        & 'material name '//varname//" FOUND.")
      vars(ii)%ptr => ScalarMeshFieldFactory(engine=engine, name=name)
      var => vars(ii)%ptr
      CALL var%Initiate(material=material, mesh=amesh, name=varname,  &
        & engine=engine)
      var => NULL()
    END IF

  END DO

  NULLIFY (amesh, material, var)
  name = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER

END SUBROUTINE KernelInitiateScalarProperty1

!----------------------------------------------------------------------------
!                                                         SetScalarProperty
!----------------------------------------------------------------------------

SUBROUTINE KernelSetScalarProperty1(vars, materials, dom, varname,  &
  & matid, timeVec)
  TYPE(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: vars(:)
  TYPE(SolidMaterialPointer_), INTENT(INOUT) :: materials(:)
  TYPE(Domain_), INTENT(INOUT) :: dom
  CHARACTER(*), INTENT(IN) :: varname
  INTEGER(I4B), INTENT(IN) :: matid
  REAL(DFP), INTENT(IN) :: timeVec(:)

  ! Define internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelSetScalarProperty1()"
  LOGICAL(LGT) :: isok, problem
  CLASS(Mesh_), POINTER :: amesh
  CLASS(SolidMaterial_), POINTER :: material
  CLASS(AbstractScalarMeshField_), POINTER :: var
  INTEGER(I4B) :: nsd, tmesh, ii, id
  CHARACTER(:), ALLOCATABLE :: name

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  amesh => NULL()
  material => NULL()
  var => NULL()

  nsd = dom%GetNSD()
  tmesh = dom%GetTotalMesh(dim=nsd)

  DO ii = 1, tmesh
    amesh => dom%GetMeshPointer(dim=nsd, entityNum=ii)
    problem = amesh%isEmpty()
    IF (problem) CYCLE

    id = amesh%GetMaterial(matid)
    IF (id .EQ. 0_I4B) THEN
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        & '[SKIPPING] :: for mesh = '//tostring(ii)//' found  id = 0.')
      CYCLE
    END IF

    material => materials(id)%ptr

    isok = ASSOCIATED(material)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[SKIPPING] :: AbstractKernel_::solidMaterial('//  &
        & tostring(ii)//') is not ASSOCIATED.')
      CYCLE
    END IF

    isok = material%IsMaterialPresent(name=varname)
    IF (isok) THEN
      var => vars(ii)%ptr
      CALL var%Set(material=material, dom=dom, name=varname, timeVec=timeVec)
      var => NULL()
    ELSE
      vars(ii)%ptr => NULL()
    END IF

  END DO

  NULLIFY (amesh, material, var)
  name = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER
END SUBROUTINE KernelSetScalarProperty1

!----------------------------------------------------------------------------
!                                                 InitiateTensorProperty
!----------------------------------------------------------------------------

SUBROUTINE KernelInitiateTensorProperty1(vars, materials, dom, nnt, varname,  &
  & matid, engine)
  TYPE(AbstractTensorMeshFieldPointer_), INTENT(INOUT) :: vars(:)
  TYPE(SolidMaterialPointer_), INTENT(INOUT) :: materials(:)
  TYPE(Domain_), INTENT(INOUT) :: dom
  INTEGER(I4B), INTENT(IN) :: nnt
  CHARACTER(*), INTENT(IN) :: varname
  INTEGER(I4B), INTENT(IN) :: matid
  CHARACTER(*), INTENT(IN) :: engine

  ! Define internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelInitiateTensorProperty1()"
  LOGICAL(LGT) :: isok
  CLASS(Mesh_), POINTER :: amesh
  CLASS(SolidMaterial_), POINTER :: material
  CLASS(AbstractTensorMeshField_), POINTER :: var
  INTEGER(I4B) :: nsd, tmesh, ii, id
  CHARACTER(:), ALLOCATABLE :: name

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  amesh => NULL()
  material => NULL()
  var => NULL()

  nsd = dom%GetNSD()
  tmesh = dom%GetTotalMesh(dim=nsd)

  name = "STScalar"
  IF (nnt .EQ. 1) name = "Scalar"

  DO ii = 1, tmesh
    amesh => dom%GetMeshPointer(dim=nsd, entityNum=ii)
    id = amesh%GetMaterial(matid)

    IF (id .EQ. 0_I4B) THEN
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        & '[SKIPPING] :: for mesh = '//tostring(ii)//' found  id = 0.')
      CYCLE
    END IF

    material => materials(id)%ptr

    isok = ASSOCIATED(material)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[SKIPPING] :: AbstractKernel_::solidMaterial('//  &
        & tostring(ii)//') is not ASSOCIATED.')
      CYCLE
    END IF

    isok = material%IsMaterialPresent(name=varname)
    IF (isok) THEN
      vars(ii)%ptr => TensorMeshFieldFactory(engine=engine, name=name)

      var => vars(ii)%ptr
      CALL var%Initiate(material=material, mesh=amesh, name=varname,  &
        & engine=engine)
      var => NULL()
    ELSE
      vars(ii)%ptr => NULL()
    END IF
  END DO

  NULLIFY (amesh, material, var)
  name = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER

END SUBROUTINE KernelInitiateTensorProperty1

!----------------------------------------------------------------------------
!                                                         SetScalarProperty
!----------------------------------------------------------------------------

SUBROUTINE KernelSetTensorProperty1(vars, materials, dom, varname,  &
  & matid, timeVec)
  TYPE(AbstractTensorMeshFieldPointer_), INTENT(INOUT) :: vars(:)
  TYPE(SolidMaterialPointer_), INTENT(INOUT) :: materials(:)
  TYPE(Domain_), INTENT(INOUT) :: dom
  CHARACTER(*), INTENT(IN) :: varname
  INTEGER(I4B), INTENT(IN) :: matid
  REAL(DFP), INTENT(IN) :: timeVec(:)

  ! Define internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelSetTensorProperty1()"
  LOGICAL(LGT) :: isok
  CLASS(Mesh_), POINTER :: amesh
  CLASS(SolidMaterial_), POINTER :: material
  CLASS(AbstractTensorMeshField_), POINTER :: var
  INTEGER(I4B) :: nsd, tmesh, ii, id
  CHARACTER(:), ALLOCATABLE :: name

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  amesh => NULL()
  material => NULL()
  var => NULL()

  nsd = dom%GetNSD()
  tmesh = dom%GetTotalMesh(dim=nsd)

  DO ii = 1, tmesh
    amesh => dom%GetMeshPointer(dim=nsd, entityNum=ii)
    id = amesh%GetMaterial(matid)

    IF (id .EQ. 0_I4B) THEN
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        & '[SKIPPING] :: for mesh = '//tostring(ii)//' found  id = 0.')
      CYCLE
    END IF

    material => materials(id)%ptr

    isok = ASSOCIATED(material)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[SKIPPING] :: AbstractKernel_::solidMaterial('//  &
        & tostring(ii)//') is not ASSOCIATED.')
      CYCLE
    END IF

    isok = material%IsMaterialPresent(name=varname)
    IF (isok) THEN
      var => vars(ii)%ptr
      CALL var%Set(material=material, dom=dom, name=varname, timeVec=timeVec)
      var => NULL()
    ELSE
      vars(ii)%ptr => NULL()
    END IF
  END DO

  NULLIFY (amesh, material, var)
  name = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER
END SUBROUTINE KernelSetTensorProperty1

!----------------------------------------------------------------------------
!                                       InitiateConstantElasticityProperties
!----------------------------------------------------------------------------

SUBROUTINE KernelInitiateConstantElasticityProperties(youngsModulus,  &
  & shearModulus, cijkl, dom, nnt, engine)
  TYPE(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: youngsModulus(:)
  TYPE(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: shearModulus(:)
  TYPE(AbstractTensorMeshFieldPointer_), INTENT(INOUT) :: cijkl(:)
  TYPE(Domain_), INTENT(INOUT) :: dom
  INTEGER(I4B), INTENT(IN) :: nnt
  CHARACTER(*), INTENT(IN) :: engine

  ! Internal variables
  CHARACTER(*), PARAMETER :: myName =  &
    & "KernelInitiateConstantElasticityProperties()"
  INTEGER(I4B) :: ii, tmesh, nsd
  TYPE(ParameterList_) :: param1, param2, param3
  CLASS(Mesh_), POINTER :: amesh
  CHARACTER(:), ALLOCATABLE :: name
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START]')
#endif

  amesh => NULL()
  CALL param1%Initiate()
  CALL param2%Initiate()
  CALL param3%Initiate()
  nsd = dom%GetNSD()
  tmesh = dom%GetTotalMesh(dim=nsd)

  IF (nnt .EQ. 1) THEN
    name = ""
    CALL SetScalarMeshFieldParam(param=param1, name="youngsModulus", &
      & fieldType=FIELD_TYPE_CONSTANT, varType=Constant,  &
      & engine=engine, defineOn=Nodal, nns=1)

    CALL SetScalarMeshFieldParam(param=param2, name="shearModulus", &
      & fieldType=FIELD_TYPE_CONSTANT, varType=Constant,  &
      & engine=engine, defineOn=Nodal, nns=1)

    CALL SetTensorMeshFieldParam(param=param3, name="cijkl", &
      & fieldType=FIELD_TYPE_CONSTANT, varType=Constant, &
      & engine=engine, defineOn=Nodal, &
      & nns=1, dim1=6, dim2=6)

  ELSE
    name = "ST"

    CALL SetSTScalarMeshFieldParam(param=param1, name="youngsModulus", &
      & fieldType=FIELD_TYPE_CONSTANT, varType=Constant, &
      & engine=engine, defineOn=Nodal, nns=1, nnt=1)

    CALL SetSTScalarMeshFieldParam( &
      & param=param2, name="shearModulus", fieldType=FIELD_TYPE_CONSTANT, &
      & varType=Constant, engine=engine, defineOn=Nodal, &
      & nns=1, nnt=1)

    CALL SetSTTensorMeshFieldParam(param=param3, name="cijkl", &
      & fieldType=FIELD_TYPE_CONSTANT, varType=Constant, &
      & engine=engine, defineOn=Nodal, nns=1, dim1=6, dim2=6,  &
      & nnt=1)
  END IF

  DO ii = 1, tmesh
    amesh => dom%GetMeshPointer(dim=nsd, entityNum=ii)

    isok = ASSOCIATED(youngsModulus(ii)%ptr)
    IF (isok) THEN
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        & 'youngsModulus ('//tostring(ii)//') already ASSOCIATED.')
    END IF

    IF (.NOT. isok) THEN
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        & 'Making youngsModulus ('//tostring(ii)//').')
      youngsModulus(ii)%ptr => ScalarMeshFieldFactory(engine=engine, &
        & name=name//"Scalar")
      CALL youngsModulus(ii)%ptr%Initiate(param=param1, mesh=amesh)
    END IF

    isok = ASSOCIATED(shearModulus(ii)%ptr)
    IF (isok) THEN
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        & 'shearModulus ('//tostring(ii)//') already ASSOCIATED.')
    END IF

    IF (.NOT. isok) THEN
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        & 'Making shearModulus ('//tostring(ii)//').')
      shearModulus(ii)%ptr => ScalarMeshFieldFactory(engine=engine,  &
        & name=name//"Scalar")
      CALL shearModulus(ii)%ptr%Initiate(param=param2, mesh=amesh)
    END IF

    isok = ASSOCIATED(Cijkl(ii)%ptr)
    IF (isok) THEN
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        & 'Cijkl ('//tostring(ii)//') already ASSOCIATED.')
    END IF

    IF (.NOT. isok) THEN
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        & 'Making Cijkl ('//tostring(ii)//').')
      Cijkl(ii)%ptr => TensorMeshFieldFactory(engine=engine, &
        & name=name//"Tensor")
      CALL Cijkl(ii)%ptr%Initiate(param=param3, mesh=amesh)
    END IF
  END DO

  CALL param1%DEALLOCATE()
  CALL param2%DEALLOCATE()
  CALL param3%DEALLOCATE()
  NULLIFY (amesh)

#ifdef DEBUG_VER
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & '[END]')
#endif
END SUBROUTINE KernelInitiateConstantElasticityProperties

!----------------------------------------------------------------------------
!                                                        InitiateMassMatrix
!----------------------------------------------------------------------------

SUBROUTINE KernelInitiateSpaceMatrix(mat, dom, nsd, engine,  &
  & problemType, name, matrixProp)
  CLASS(MatrixField_), INTENT(INOUT) :: mat
  TYPE(Domain_), INTENT(INOUT) :: dom
  INTEGER(I4B), INTENT(IN) :: nsd
  CHARACTER(*), INTENT(IN) :: engine
  INTEGER(I4B), INTENT(IN) :: problemType
  CHARACTER(*), INTENT(IN) :: name
  CHARACTER(*), INTENT(IN) :: matrixProp

  ! Internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelInitiateSpaceMatrix()"
  TYPE(ParameterList_) :: param
  LOGICAL(LGT) :: problem
  INTEGER(I4B) :: spaceCompo

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  problem = nsd .EQ. 0_I4B
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: obj%nsd=0 found')
    RETURN
  END IF

  CALL param%Initiate()

  IF (problemType .EQ. kernelProblemType%scalar) THEN
    spaceCompo = 1
  ELSE IF (problemType .EQ. kernelProblemType%vector) THEN
    spaceCompo = nsd
  ELSE
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: Unknown problemType = '// &
      & tostring(problemType))
    RETURN
  END IF

  CALL SetMatrixFieldParam( &
    & param=param, &
    & name=name, &
    & matrixProp=matrixProp, &
    & spaceCompo=spaceCompo, &
    & timeCompo=1, &
    & fieldType=FIELD_TYPE_NORMAL, &
    & engine=engine)

  CALL mat%Initiate(param=param, dom=dom)
  CALL param%DEALLOCATE()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER
END SUBROUTINE KernelInitiateSpaceMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE KernelInitiateTangentMatrix(mat, linsol, dom, nsd, nnt, engine,  &
  & name, matrixProp)
  CLASS(AbstractMatrixField_), INTENT(INOUT) :: mat
  CLASS(AbstractLinSolver_), INTENT(INOUT) :: linsol
  TYPE(Domain_), INTENT(INOUT) :: dom
  INTEGER(I4B), INTENT(IN) :: nsd
  INTEGER(I4B), INTENT(IN) :: nnt
  CHARACTER(*), INTENT(IN) :: engine
  CHARACTER(*), INTENT(IN) :: name
  CHARACTER(*), INTENT(IN) :: matrixProp
! Internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelInitiateTangentMatrix()"
  TYPE(ParameterList_) :: param
  LOGICAL(LGT) :: problem

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  problem = nsd .EQ. 0_I4B .OR. nnt .EQ. 0_I4B

  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: nsd=0 or nnt=0 found')
    RETURN
  END IF

  CALL param%Initiate()

  CALL SetMatrixFieldParam( &
    & param=param, &
    & name=name, &
    & matrixProp=matrixProp, &
    & spaceCompo=nsd, &
    & timeCompo=nnt, &
    & fieldType=FIELD_TYPE_NORMAL, &
    & engine=engine)

  CALL mat%Initiate(param=param, dom=dom)
  CALL linsol%Set(mat)

  CALL param%DEALLOCATE()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER

END SUBROUTINE KernelInitiateTangentMatrix

!----------------------------------------------------------------------------
!                                                   KernelAssembleMassMatrix
!----------------------------------------------------------------------------

SUBROUTINE KernelAssembleMassMatrix(mat, massDensity, dom, cellFE,  &
  & linCellFE, spaceElemSD, linSpaceElemSD, problemType, reset)
  CLASS(MatrixField_), INTENT(INOUT) :: mat
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: massDensity(:)
  CLASS(Domain_), INTENT(INOUT) :: dom
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: cellFE(:)
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: linCellFE(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: spaceElemSD(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: linSpaceElemSD(:)
  INTEGER(I4B), INTENT(IN) :: problemType
  LOGICAL(LGT), INTENT(IN) :: reset

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelAssembleMassMatrix()"
  INTEGER(I4B) :: id, tmesh, nsd, telems, nns, tdof, iel
  TYPE(ElemShapeData_) :: elemsd, linElemSD
  TYPE(FEVariable_) :: fevar
  CLASS(Mesh_), POINTER :: meshptr
  CLASS(FiniteElement_), POINTER :: spaceFE, linSpaceFE
  CLASS(AbstractScalarMeshField_), POINTER :: rhoField
  CLASS(ReferenceElement_), POINTER :: refelem
  REAL(DFP), ALLOCATABLE :: Mmat(:, :), xij(:, :)
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  IF (reset) CALL mat%Set(VALUE=0.0_DFP)

  nsd = dom%GetNSD()
  tmesh = dom%GetTotalMesh(dim=nsd)

  tdof = 1
  IF (problemType .EQ. KernelProblemType%vector) tdof = nsd

  NULLIFY (meshptr, spaceFE, rhoField, refelem, linSpaceFE)

  DO id = 1, tmesh
    meshptr => dom%GetMeshPointer(dim=nsd, entityNum=id)
    telems = meshptr%GetTotalElements()
    IF (telems .EQ. 0_I4B) CYCLE

    spaceFE => cellFE(id)%ptr
    linSpaceFE => linCellFE(id)%ptr

    elemsd = spaceElemSD(id)
    linElemSD = linSpaceElemSD(id)

    refelem => meshptr%GetRefElemPointer()
    nns = (.NNE.refelem)

    CALL Reallocate(xij, nsd, nns)
    CALL Reallocate(Mmat, tdof * nns, tdof * nns)

    rhoField => massDensity(id)%ptr

    DO iel = meshptr%minElemNum, meshptr%maxElemNum

      IF (.NOT. meshptr%IsElementPresent(iel)) CYCLE

      CALL rhoField%Get(fevar=fevar, globalElement=iel)

      nptrs = meshptr%GetConnectivity(iel)

      CALL dom%GetNodeCoord(globalNode=nptrs, nodeCoord=xij)

      CALL spaceFE%GetGlobalElemShapeData(elemsd=elemsd, xij=xij,  &
        & geoElemSD=linElemSD)

      Mmat = MassMatrix(test=elemsd, trial=elemsd, opt=tdof,  &
        & rho=fevar, rhorank=TypeFEVariableScalar)

      CALL mat%Set(globalNode=nptrs, VALUE=Mmat,  &
        & storageFMT=DOF_FMT, scale=1.0_DFP, addContribution=.TRUE.)

    END DO
  END DO

  IF (ALLOCATED(Mmat)) DEALLOCATE (Mmat)
  IF (ALLOCATED(xij)) DEALLOCATE (xij)
  IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
  CALL DEALLOCATE (elemsd)
  CALL DEALLOCATE (linElemSD)
  CALL DEALLOCATE (fevar)

  NULLIFY (meshptr, spaceFE, rhoField, refelem, linSpaceFE)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER

END SUBROUTINE KernelAssembleMassMatrix

!----------------------------------------------------------------------------
!                                   KernelAssembleIsotropicStiffnessMatrix
!----------------------------------------------------------------------------

SUBROUTINE KernelAssembleIsoStiffMat(mat, youngsModulus, shearModulus,  &
  & dom, cellFE, linCellFE, spaceElemSD, linSpaceElemSD, reset)
  CLASS(MatrixField_), INTENT(INOUT) :: mat
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: youngsModulus(:)
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: shearModulus(:)
  CLASS(Domain_), INTENT(INOUT) :: dom
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: cellFE(:)
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: linCellFE(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: spaceElemSD(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: linSpaceElemSD(:)
  LOGICAL(LGT), INTENT(IN) :: reset

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelAssembleIsoStiffMat()"
  INTEGER(I4B) :: id, tmesh, nsd, nns, tdof, iel
  TYPE(ElemShapeData_) :: elemsd, linElemSD
  TYPE(FEVariable_) :: muVar, youngsVar
  CLASS(Mesh_), POINTER :: meshptr
  CLASS(FiniteElement_), POINTER :: spaceFE, linSpaceFE
  CLASS(AbstractScalarMeshField_), POINTER :: youngsModulusField,  &
    & shearModulusField
  CLASS(ReferenceElement_), POINTER :: refelem
  REAL(DFP), ALLOCATABLE :: Mmat(:, :), xij(:, :)
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)
  LOGICAL(LGT) :: isok, problem

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  IF (reset) CALL mat%Set(VALUE=0.0_DFP)
  nsd = dom%GetNSD()
  tmesh = dom%GetTotalMesh(dim=nsd)
  tdof = nsd

  NULLIFY (meshptr, spaceFE, linSpaceFE, youngsModulusField,  &
    & shearModulusField, refelem)

  DO id = 1, tmesh
    meshptr => dom%GetMeshPointer(dim=nsd, entityNum=id)

    isok = ASSOCIATED(meshptr)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: Null mesh found at id = '//tostring(id))
      RETURN
    END IF

    problem = meshptr%isEmpty()
    IF (problem) CYCLE

    spaceFE => cellFE(id)%ptr
    linSpaceFE => linCellFE(id)%ptr

    elemsd = spaceElemSD(id)
    linElemSD = linSpaceElemSD(id)

    refelem => meshptr%GetRefElemPointer()
    nns = (.NNE.refelem)

    CALL Reallocate(xij, nsd, nns)
    CALL Reallocate(Mmat, tdof * nns, tdof * nns)

    youngsModulusField => youngsModulus(id)%ptr
    shearModulusField => shearModulus(id)%ptr

    isok = ASSOCIATED(youngsModulusField)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: youngsModulus('//tostring(id)//') is NULL.')
      RETURN
    END IF

    isok = ASSOCIATED(shearModulusField)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: shearModulus('//tostring(id)//') is NULL.')
      RETURN
    END IF

    DO iel = meshptr%minElemNum, meshptr%maxElemNum

      isok = meshptr%isElementPresent(iel)
      IF (.NOT. isok) CYCLE

      CALL youngsModulusField%Get(fevar=youngsVar, globalElement=iel)

      CALL shearModulusField%Get(fevar=muVar, globalElement=iel)

      nptrs = meshptr%GetConnectivity(iel)

      CALL dom%GetNodeCoord(globalNode=nptrs, nodeCoord=xij)

      CALL spaceFE%GetGlobalElemShapeData(elemsd=elemsd, xij=xij,  &
        & geoElemSD=linElemSD)

      Mmat = StiffnessMatrix(test=elemsd, trial=elemsd, &
        & lambda=youngsVar, mu=muVar, isLambdaYoungsModulus=.TRUE.)

      CALL mat%Set(globalNode=nptrs, VALUE=Mmat,  &
        & storageFMT=DOF_FMT, scale=1.0_DFP, addContribution=.TRUE.)

    END DO
  END DO

  NULLIFY (meshptr, spaceFE, youngsModulusField, shearModulusField, refelem, linSpaceFE)

  IF (ALLOCATED(Mmat)) DEALLOCATE (Mmat)
  IF (ALLOCATED(xij)) DEALLOCATE (xij)
  IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
  CALL DEALLOCATE (elemsd)
  CALL DEALLOCATE (linElemSD)
  CALL DEALLOCATE (youngsVar)
  CALL DEALLOCATE (muVar)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER
END SUBROUTINE KernelAssembleIsoStiffMat

!----------------------------------------------------------------------------
!                                             KernelAssembleStiffnessMatrix
!----------------------------------------------------------------------------
SUBROUTINE KernelAssembleCijklStiffMat(mat, Cijkl, dom, cellFE,  &
  & linCellFE, spaceElemSD, linSpaceElemSD, reset)
  CLASS(MatrixField_), INTENT(INOUT) :: mat
  CLASS(AbstractTensorMeshFieldPointer_), INTENT(INOUT) :: Cijkl(:)
  CLASS(Domain_), INTENT(INOUT) :: dom
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: cellFE(:)
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: linCellFE(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: spaceElemSD(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: linSpaceElemSD(:)
  LOGICAL(LGT), INTENT(IN) :: reset

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelAssembleCijklStiffMat()"
  INTEGER(I4B) :: id, tmesh, nsd, telems, nns, tdof, iel
  TYPE(ElemShapeData_) :: elemsd, linElemSD
  TYPE(FEVariable_) :: CijklVar
  CLASS(Mesh_), POINTER :: meshptr
  CLASS(FiniteElement_), POINTER :: spaceFE, linSpaceFE
  CLASS(AbstractTensorMeshField_), POINTER :: CijklField
  CLASS(ReferenceElement_), POINTER :: refelem
  REAL(DFP), ALLOCATABLE :: Mmat(:, :), xij(:, :)
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  IF (reset) CALL mat%Set(VALUE=0.0_DFP)

  nsd = dom%GetNSD()
  tmesh = dom%GetTotalMesh(dim=nsd)
  tdof = nsd

  NULLIFY (meshptr, spaceFE, CijklField, refelem, linSpaceFE)

  DO id = 1, tmesh
    meshptr => dom%GetMeshPointer(dim=nsd, entityNum=id)
    telems = meshptr%GetTotalElements()
    IF (telems .EQ. 0_I4B) CYCLE

    spaceFE => cellFE(id)%ptr
    linSpaceFE => linCellFE(id)%ptr

    elemsd = spaceElemSD(id)
    linElemSD = linSpaceElemSD(id)

    refelem => meshptr%GetRefElemPointer()
    nns = (.NNE.refelem)

    CALL Reallocate(xij, nsd, nns)
    CALL Reallocate(Mmat, tdof * nns, tdof * nns)

    CijklField => Cijkl(id)%ptr

    DO iel = meshptr%minElemNum, meshptr%maxElemNum

      IF (.NOT. meshptr%IsElementPresent(iel)) CYCLE

      CALL CijklField%Get(fevar=CijklVar, globalElement=iel)

      nptrs = meshptr%GetConnectivity(iel)

      CALL dom%GetNodeCoord(globalNode=nptrs, nodeCoord=xij)

      CALL spaceFE%GetGlobalElemShapeData(elemsd=elemsd, xij=xij,   &
        & geoElemSD=linElemSD)

      Mmat = StiffnessMatrix(test=elemsd, trial=elemsd, Cijkl=CijklVar)

      CALL mat%Set(globalNode=nptrs, VALUE=Mmat,  &
        & storageFMT=DOF_FMT, scale=1.0_DFP, addContribution=.TRUE.)

    END DO
  END DO

  IF (ALLOCATED(Mmat)) DEALLOCATE (Mmat)
  IF (ALLOCATED(xij)) DEALLOCATE (xij)
  IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
  CALL DEALLOCATE (elemsd)
  CALL DEALLOCATE (linElemSD)
  CALL DEALLOCATE (CijklVar)
  NULLIFY (meshptr, spaceFE, CijklField, refelem, linSpaceFE)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER
END SUBROUTINE KernelAssembleCijklStiffMat

!----------------------------------------------------------------------------
!                                                   KernelAssembleBodyForce
!----------------------------------------------------------------------------

SUBROUTINE KernelAssembleBodyForce1(rhs, dom, bodyFunc, cellFE,  &
  & linCellFE, spaceElemSD, linSpaceElemSD, reset, scale)
  CLASS(VectorField_), INTENT(INOUT) :: rhs
  CLASS(Domain_), INTENT(INOUT) :: dom
  CLASS(UserFunction_), INTENT(INOUT) :: bodyFunc
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: cellFE(:)
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: linCellFE(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: spaceElemSD(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: linSpaceElemSD(:)
  LOGICAL(LGT), INTENT(IN) :: reset
  REAL(DFP), INTENT(IN) :: scale

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelAssembleBodyForce1()"
  LOGICAL(LGT) :: problem, isok
  INTEGER(I4B) :: tmesh, nsd, id, nns, iel
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)
  REAL(DFP), ALLOCATABLE :: fevec(:, :), xij(:, :)
  TYPE(FEVariable_) :: bodyvar
  TYPE(ElemShapeData_) :: elemsd, linElemSD
  CLASS(Mesh_), POINTER :: meshptr
  CLASS(ReferenceElement_), POINTER :: refelem
  CLASS(FiniteElement_), POINTER :: spaceFE, linSpaceFE

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  IF (reset) CALL rhs%Set(VALUE=0.0_DFP)

  nsd = dom%GetNSD()
  tmesh = dom%GetTotalMesh(dim=nsd)
  NULLIFY (meshptr, refelem, spaceFE, linSpaceFE)

  DO id = 1, tmesh
    meshptr => dom%GetMeshPointer(dim=nsd, entityNum=id)
    problem = meshptr%isEmpty()
    IF (problem) CYCLE

    spaceFE => cellFE(id)%ptr
    linSpaceFE => linCellFE(id)%ptr

    elemsd = spaceElemSD(id)
    linElemSD = linSpaceElemSD(id)

    refelem => meshptr%GetRefElemPointer()
    nns = (.nne.refelem)

    CALL Reallocate(xij, nsd, nns)
    CALL Reallocate(fevec, nsd, nns)

    DO iel = meshptr%minElemNum, meshptr%maxElemNum

      isok = meshptr%isElementPresent(iel)
      IF (.NOT. isok) CYCLE

      nptrs = meshptr%GetConnectivity(iel)

      CALL dom%GetNodeCoord(globalNode=nptrs, nodeCoord=xij)

      CALL spaceFE%GetGlobalElemShapeData(elemsd=elemsd, xij=xij,  &
        & geoElemSD=linElemSD)

      CALL bodyFunc%Get(fevar=bodyvar, xij=xij)

      fevec = ForceVector(test=elemsd, c=bodyvar,  &
        & crank=TypeFEVariableVector)

      CALL rhs%Set(globalNode=nptrs, scale=scale,  &
        & addContribution=.TRUE., VALUE=fevec)

    END DO

  END DO

  NULLIFY (meshptr, refelem, spaceFE, linSpaceFE)
  IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
  IF (ALLOCATED(fevec)) DEALLOCATE (fevec)
  CALL DEALLOCATE (bodyvar)
  CALL DEALLOCATE (elemsd)
  CALL DEALLOCATE (linElemSD)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER

END SUBROUTINE KernelAssembleBodyForce1

!----------------------------------------------------------------------------
!                                                   KernelAssembleBodyForce
!----------------------------------------------------------------------------

SUBROUTINE KernelAssembleBodyForce2(rhs, dom, bodyVec, cellFE,  &
  & linCellFE, spaceElemSD, linSpaceElemSD, reset, scale)
  CLASS(VectorField_), INTENT(INOUT) :: rhs
  CLASS(Domain_), INTENT(INOUT) :: dom
  CLASS(VectorField_), INTENT(INOUT) :: bodyVec
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: cellFE(:)
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: linCellFE(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: spaceElemSD(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: linSpaceElemSD(:)
  LOGICAL(LGT), INTENT(IN) :: reset
  REAL(DFP), INTENT(IN) :: scale

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelAssembleBodyForce2()"
  LOGICAL(LGT) :: problem, isok
  INTEGER(I4B) :: tmesh, nsd, id, nns, iel
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)
  REAL(DFP), ALLOCATABLE :: fevec(:, :), xij(:, :), aBodyVec(:, :)
  TYPE(FEVariable_) :: bodyvar
  TYPE(ElemShapeData_) :: elemsd, linElemSD
  CLASS(Mesh_), POINTER :: meshptr
  CLASS(ReferenceElement_), POINTER :: refelem
  CLASS(FiniteElement_), POINTER :: spaceFE, linSpaceFE

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  IF (reset) CALL rhs%Set(VALUE=0.0_DFP)

  nsd = dom%GetNSD()
  tmesh = dom%GetTotalMesh(dim=nsd)
  NULLIFY (meshptr, refelem, spaceFE, linSpaceFE)

  DO id = 1, tmesh
    meshptr => dom%GetMeshPointer(dim=nsd, entityNum=id)
    problem = meshptr%isEmpty()
    IF (problem) CYCLE

    spaceFE => cellFE(id)%ptr
    linSpaceFE => linCellFE(id)%ptr

    elemsd = spaceElemSD(id)
    linElemSD = linSpaceElemSD(id)

    refelem => meshptr%GetRefElemPointer()
    nns = (.nne.refelem)

    CALL Reallocate(xij, nsd, nns)
    CALL Reallocate(fevec, nsd, nns)

    DO iel = meshptr%minElemNum, meshptr%maxElemNum

      isok = meshptr%isElementPresent(iel)
      IF (.NOT. isok) CYCLE

      nptrs = meshptr%GetConnectivity(iel)

      CALL dom%GetNodeCoord(globalNode=nptrs, nodeCoord=xij)

      CALL spaceFE%GetGlobalElemShapeData(elemsd=elemsd, xij=xij,  &
        & geoElemSD=linElemSD)

      CALL bodyVec%Get(VALUE=aBodyVec, globalNode=nptrs)
      bodyvar = NodalVariable(val=aBodyVec, rank=TypeFEVariableVector,  &
        & vartype=TypeFEVariableSpace)

      fevec = ForceVector(test=elemsd, c=bodyvar,  &
        & crank=TypeFEVariableVector)

      CALL rhs%Set(globalNode=nptrs, scale=scale,  &
        & addContribution=.TRUE., VALUE=fevec)

    END DO

  END DO

  NULLIFY (meshptr, refelem, spaceFE, linSpaceFE)
  IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
  IF (ALLOCATED(fevec)) DEALLOCATE (fevec)
  IF (ALLOCATED(aBodyVec)) DEALLOCATE (aBodyVec)
  CALL DEALLOCATE (bodyvar)
  CALL DEALLOCATE (elemsd)
  CALL DEALLOCATE (linElemSD)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER

END SUBROUTINE KernelAssembleBodyForce2

END MODULE KernelUtility
