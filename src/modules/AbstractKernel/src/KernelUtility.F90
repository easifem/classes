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

MODULE KernelUtility
USE GlobalData
USE Field
USE AbstractLinSolver_Class
USE FieldFactory
USE SolidMaterial_Class
USE Domain_Class
USE Mesh_Class
USE BaseMethod
USE FiniteElement_Class
USE BaseType
USE BaseMethod, ONLY: MassMatrix
USE ExceptionHandler_Class, ONLY: e
USE AbstractKernelParam, ONLY: kernelProblemType
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

CONTAINS

!----------------------------------------------------------------------------
!                                                 InitiateScalarProperty
!----------------------------------------------------------------------------

SUBROUTINE KernelInitiateScalarProperty1(vars, materials, dom, nnt,  &
  & varname, matid, engine)
  TYPE(AbstractScalarMeshFieldPointer_), ALLOCATABLE, INTENT(INOUT) :: vars(:)
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

  CALL Display(nsd, "nsd = ")
  STOP

  isok = ALLOCATED(vars)
  IF (.NOT. isok) ALLOCATE (vars(tmesh))

  name = "STScalar"
  IF (nnt .EQ. 1) name = "Scalar"

  DO ii = 1, tmesh
    amesh => dom%GetMeshPointer(dim=nsd, entityNum=ii)
    isok = ASSOCIATED(amesh)

    IF (.NOT. isok) THEN
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        & '[NULL MESH] :: mesh('//tostring(ii)//') not ASSOCIATED.')
      CYCLE
    END IF

    problem = amesh%isEmpty()
    IF (problem) THEN
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        & '[EMPTY MESH] :: mesh('//tostring(ii)//') is EMPTY.')
      CYCLE
    END IF

    id = amesh%GetMaterial(matid)
    IF (id .EQ. 0_I4B) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[SKIPPING] :: For mesh('//tostring(ii)//') found  id = 0.')
      RETURN
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
      vars(ii)%ptr => ScalarMeshFieldFactory(engine=engine, name=name)

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

  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
    & '[1]')

  nsd = dom%GetNSD()
  tmesh = dom%GetTotalMesh(dim=nsd)

  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
    & '[2]')

  DO ii = 1, tmesh
    CALL e%RaiseDebug(modName//'::'//myName//' - '// &
      & '[3]')
    amesh => dom%GetMeshPointer(dim=nsd, entityNum=ii)
    problem = amesh%isEmpty()
    IF (problem) CYCLE

    CALL e%RaiseDebug(modName//'::'//myName//' - '// &
      & '[4]')
    id = amesh%GetMaterial(matid)
    IF (id .EQ. 0_I4B) THEN
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        & '[SKIPPING] :: for mesh = '//tostring(ii)//' found  id = 0.')
      CYCLE
    END IF

    CALL e%RaiseDebug(modName//'::'//myName//' - '// &
      & '[5]')
    material => materials(id)%ptr

    isok = ASSOCIATED(material)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[SKIPPING] :: AbstractKernel_::solidMaterial('//  &
        & tostring(ii)//') is not ASSOCIATED.')
      CYCLE
    END IF

    CALL e%RaiseDebug(modName//'::'//myName//' - '// &
      & '[6]')
    isok = material%IsMaterialPresent(name=varname)
    IF (isok) THEN
      var => vars(ii)%ptr
      CALL var%Set(material=material, dom=dom, name=varname, timeVec=timeVec)
      var => NULL()
    ELSE
      vars(ii)%ptr => NULL()
    END IF

    CALL e%RaiseDebug(modName//'::'//myName//' - '// &
      & '[7]')
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
  TYPE(AbstractTensorMeshFieldPointer_), ALLOCATABLE, INTENT(INOUT) ::  &
    & vars(:)
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

  isok = ALLOCATED(vars)
  IF (.NOT. isok) THEN
    ALLOCATE (vars(tmesh))
  END IF

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

SUBROUTINE KernelInitiateConstantElasticityProperties(lambda, mu,  &
  & Cijkl, dom, nnt, engine)
  TYPE(AbstractScalarMeshFieldPointer_), ALLOCATABLE, INTENT(INOUT) ::  &
  & lambda(:)
  TYPE(AbstractScalarMeshFieldPointer_), ALLOCATABLE, INTENT(INOUT) ::  &
  & mu(:)
  TYPE(AbstractTensorMeshFieldPointer_), ALLOCATABLE, INTENT(INOUT) ::  &
  & Cijkl(:)
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

  isok = ALLOCATED(lambda)
  IF (.NOT. isok) THEN
    ALLOCATE (lambda(tmesh))
    RETURN
  END IF

  isok = ALLOCATED(mu)
  IF (.NOT. isok) THEN
    ALLOCATE (mu(tmesh))
    RETURN
  END IF

  isok = ALLOCATED(Cijkl)
  IF (.NOT. isok) THEN
    ALLOCATE (Cijkl(tmesh))
    RETURN
  END IF

  IF (nnt .EQ. 1) THEN
    name = "Scalar"
    CALL SetScalarMeshFieldParam(param=param1, name="lambda", &
      & fieldType=FIELD_TYPE_CONSTANT, varType=Constant,  &
      & engine=engine, defineOn=Nodal, nns=1)

    CALL SetScalarMeshFieldParam(param=param1, name="mu", &
      & fieldType=FIELD_TYPE_CONSTANT, varType=Constant,  &
      & engine=engine, defineOn=Nodal, nns=1)

    CALL SetTensorMeshFieldParam(param=param3, name="Cijkl", &
      & fieldType=FIELD_TYPE_CONSTANT, varType=Constant, &
      & engine=engine, defineOn=Nodal, &
      & nns=1, dim1=6, dim2=6)

  ELSE
    name = "STScalar"

    CALL SetSTScalarMeshFieldParam(param=param1, name="lambda", &
      & fieldType=FIELD_TYPE_CONSTANT, varType=Constant, &
      & engine=engine, defineOn=Nodal, nns=1, nnt=1)

    CALL SetSTScalarMeshFieldParam( &
      & param=param2, name="mu", fieldType=FIELD_TYPE_CONSTANT, &
      & varType=Constant, engine=engine, defineOn=Nodal, &
      & nns=1, nnt=1)

    CALL SetSTTensorMeshFieldParam(param=param3, name="Cijkl", &
      & fieldType=FIELD_TYPE_CONSTANT, varType=Constant, &
      & engine=engine, defineOn=Nodal, nns=1, dim1=6, dim2=6,  &
      & nnt=1)
  END IF

  DO ii = 1, tmesh
    amesh => dom%GetMeshPointer(dim=nsd, entityNum=ii)

    isok = ASSOCIATED(lambda(ii)%ptr)
    IF (.NOT. isok) THEN
      lambda(ii)%ptr => ScalarMeshFieldFactory(engine=engine, name=name)
      CALL lambda(ii)%ptr%Initiate(param=param1, mesh=amesh)
    END IF

    isok = ASSOCIATED(mu(ii)%ptr)
    IF (.NOT. isok) THEN
      mu(ii)%ptr => ScalarMeshFieldFactory(engine=engine, name=name)
      CALL mu(ii)%ptr%Initiate(param=param2, mesh=amesh)
    END IF

    isok = ASSOCIATED(Cijkl(ii)%ptr)
    IF (.NOT. isok) THEN
      Cijkl(ii)%ptr => TensorMeshFieldFactory(engine=engine, name=name)
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
  & spaceElemSD, problemType, reset)
  CLASS(MatrixField_), INTENT(INOUT) :: mat
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: massDensity(:)
  CLASS(Domain_), INTENT(INOUT) :: dom
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: cellFE(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: spaceElemSD(:)
  INTEGER(I4B), INTENT(IN) :: problemType
  LOGICAL(LGT), INTENT(INOUT) :: reset

  ! internal variables
  INTEGER(I4B) :: id, tmesh, nsd, telems, nns, tdof, iel
  TYPE(ElemShapeData_) :: elemsd
  TYPE(FEVariable_) :: fevar
  CLASS(Mesh_), POINTER :: meshptr
  CLASS(FiniteElement_), POINTER :: spaceFE
  CLASS(AbstractScalarMeshField_), POINTER :: rhoField
  CLASS(ReferenceElement_), POINTER :: refelem
  REAL(DFP), ALLOCATABLE :: Mmat(:, :), xij(:, :)
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)

  IF (reset) CALL mat%Set(VALUE=0.0_DFP)

  nsd = dom%GetNSD()
  tmesh = dom%GetTotalMesh(dim=nsd)

  tdof = 1
  IF (problemType .EQ. KernelProblemType%vector) tdof = nsd

  NULLIFY (meshptr, spaceFE, rhoField, refelem)

  DO id = 1, tmesh
    meshptr => dom%GetMeshPointer(dim=nsd, entityNum=id)
    telems = meshptr%GetTotalElements()
    IF (telems .EQ. 0_I4B) CYCLE

    spaceFE => cellFE(id)%ptr
    elemsd = spaceElemSD(id)

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
      CALL spaceFE%GetGlobalElemShapeData(elemsd=elemsd, xij=xij)

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
  CALL DEALLOCATE (fevar)

END SUBROUTINE KernelAssembleMassMatrix

!----------------------------------------------------------------------------
!                                   KernelAssembleIsotropicStiffnessMatrix
!----------------------------------------------------------------------------

SUBROUTINE KernelAssembleIsoStiffMat(mat, lambda, mu,  &
  & dom, cellFE, spaceElemSD, reset)
  CLASS(MatrixField_), INTENT(INOUT) :: mat
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: lambda(:)
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: mu(:)
  CLASS(Domain_), INTENT(INOUT) :: dom
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: cellFE(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: spaceElemSD(:)
  LOGICAL(LGT), INTENT(INOUT) :: reset

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelAssembleIsoStiffMat()"
  INTEGER(I4B) :: id, tmesh, nsd, telems, nns, tdof, iel
  TYPE(ElemShapeData_) :: elemsd
  TYPE(FEVariable_) :: lambdaVar, muVar
  CLASS(Mesh_), POINTER :: meshptr
  CLASS(FiniteElement_), POINTER :: spaceFE
  CLASS(AbstractScalarMeshField_), POINTER :: lambdaField, muField
  CLASS(ReferenceElement_), POINTER :: refelem
  REAL(DFP), ALLOCATABLE :: Mmat(:, :), xij(:, :)
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)

  IF (reset) CALL mat%Set(VALUE=0.0_DFP)

  nsd = dom%GetNSD()
  tmesh = dom%GetTotalMesh(dim=nsd)

  tdof = nsd

  NULLIFY (meshptr, spaceFE, lambdaField, muField, refelem)

  DO id = 1, tmesh
    meshptr => dom%GetMeshPointer(dim=nsd, entityNum=id)
    telems = meshptr%GetTotalElements()
    IF (telems .EQ. 0_I4B) CYCLE

    spaceFE => cellFE(id)%ptr
    elemsd = spaceElemSD(id)

    refelem => meshptr%GetRefElemPointer()
    nns = (.NNE.refelem)

    CALL Reallocate(xij, nsd, nns)
    CALL Reallocate(Mmat, tdof * nns, tdof * nns)

    lambdaField => lambda(id)%ptr
    muField => mu(id)%ptr

    DO iel = meshptr%minElemNum, meshptr%maxElemNum

      IF (.NOT. meshptr%IsElementPresent(iel)) CYCLE
      CALL lambdaField%Get(fevar=lambdaVar, globalElement=iel)
      CALL muField%Get(fevar=muVar, globalElement=iel)
      nptrs = meshptr%GetConnectivity(iel)
      CALL dom%GetNodeCoord(globalNode=nptrs, nodeCoord=xij)
      CALL spaceFE%GetGlobalElemShapeData(elemsd=elemsd, xij=xij)

      Mmat = StiffnessMatrix(test=elemsd, trial=elemsd, &
        & lambda=lambdaVar, mu=muVar)

      CALL mat%Set(globalNode=nptrs, VALUE=Mmat,  &
        & storageFMT=DOF_FMT, scale=1.0_DFP, addContribution=.TRUE.)

    END DO
  END DO

  NULLIFY (meshptr, spaceFE, lambdaField, muField, refelem)

  IF (ALLOCATED(Mmat)) DEALLOCATE (Mmat)
  IF (ALLOCATED(xij)) DEALLOCATE (xij)
  IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
  CALL DEALLOCATE (elemsd)
  CALL DEALLOCATE (lambdaVar)
  CALL DEALLOCATE (muVar)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER

END SUBROUTINE KernelAssembleIsoStiffMat

!----------------------------------------------------------------------------
!                                             KernelAssembleStiffnessMatrix
!----------------------------------------------------------------------------
SUBROUTINE KernelAssembleCijklStiffMat(mat, Cijkl,  &
  & dom, cellFE, spaceElemSD, reset)
  CLASS(MatrixField_), INTENT(INOUT) :: mat
  CLASS(AbstractTensorMeshFieldPointer_), INTENT(INOUT) :: Cijkl(:)
  CLASS(Domain_), INTENT(INOUT) :: dom
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: cellFE(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: spaceElemSD(:)
  LOGICAL(LGT), INTENT(INOUT) :: reset

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelAssembleCijklStiffMat()"
  INTEGER(I4B) :: id, tmesh, nsd, telems, nns, tdof, iel
  TYPE(ElemShapeData_) :: elemsd
  TYPE(FEVariable_) :: CijklVar
  CLASS(Mesh_), POINTER :: meshptr
  CLASS(FiniteElement_), POINTER :: spaceFE
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

  NULLIFY (meshptr, spaceFE, CijklField, refelem)

  DO id = 1, tmesh
    meshptr => dom%GetMeshPointer(dim=nsd, entityNum=id)
    telems = meshptr%GetTotalElements()
    IF (telems .EQ. 0_I4B) CYCLE

    spaceFE => cellFE(id)%ptr
    elemsd = spaceElemSD(id)

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
      CALL spaceFE%GetGlobalElemShapeData(elemsd=elemsd, xij=xij)

      Mmat = StiffnessMatrix(test=elemsd, trial=elemsd, Cijkl=CijklVar)

      CALL mat%Set(globalNode=nptrs, VALUE=Mmat,  &
        & storageFMT=DOF_FMT, scale=1.0_DFP, addContribution=.TRUE.)

    END DO
  END DO

  IF (ALLOCATED(Mmat)) DEALLOCATE (Mmat)
  IF (ALLOCATED(xij)) DEALLOCATE (xij)
  IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
  CALL DEALLOCATE (elemsd)
  CALL DEALLOCATE (CijklVar)
  NULLIFY (meshptr, spaceFE, CijklField, refelem)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER
END SUBROUTINE KernelAssembleCijklStiffMat

END MODULE KernelUtility
