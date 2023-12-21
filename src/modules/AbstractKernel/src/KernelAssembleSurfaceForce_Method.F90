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

MODULE KernelAssembleSurfaceForce_Method
USE GlobalData
USE Field
USE BaseMethod
USE BaseType
USE FieldFactory
USE Domain_Class
USE Mesh_Class
USE FiniteElement_Class
USE ExceptionHandler_Class, ONLY: e
USE AbstractKernelParam, ONLY: KernelProblemType
USE UserFunction_Class
USE NeumannBC_Class
IMPLICIT NONE

PRIVATE
PUBLIC :: KernelAssembleSurfaceForce

CHARACTER(*), PARAMETER :: modName = "KernelAssembleSurfaceForce_Method"

INTERFACE KernelAssembleSurfaceForce
  MODULE PROCEDURE KernelAssembleSurfaceForce1
  MODULE PROCEDURE KernelAssembleSurfaceForce2
  MODULE PROCEDURE KernelAssembleSurfaceForce3
END INTERFACE KernelAssembleSurfaceForce

CONTAINS

!----------------------------------------------------------------------------
!                                                KernelAssembleSurfaceForce
!----------------------------------------------------------------------------

SUBROUTINE KernelAssembleSurfaceForce1(rhs, dom, nbcPtrs, func, fe,  &
  & linFE, spaceElemSD, linSpaceElemSD, reset, scale, times)
  CLASS(VectorField_), INTENT(INOUT) :: rhs
  CLASS(Domain_), INTENT(INOUT) :: dom
  TYPE(NeumannBCPointer_), INTENT(INOUT) :: nbcPtrs(:)
  CLASS(UserFunction_), TARGET, INTENT(INOUT) :: func
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: fe(:)
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: linFE(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: spaceElemSD(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: linSpaceElemSD(:)
  LOGICAL(LGT), INTENT(IN) :: reset
  REAL(DFP), INTENT(IN) :: scale
  REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelAssembleSurfaceForce1()"
  TYPE(UserFunctionPointer_), ALLOCATABLE :: funcPtrs(:)
  INTEGER(I4B) :: tnbc, ii

  tnbc = SIZE(nbcPtrs)

  ALLOCATE (funcPtrs(tnbc))
  DO ii = 1, tnbc; funcPtrs(ii)%ptr => func; END DO

  CALL KernelAssembleSurfaceForce2(rhs=rhs, dom=dom, nbcPtrs=nbcPtrs,  &
    & funcPtrs=funcPtrs, fe=fe, linFE=linFE, spaceElemSD=spaceElemSD,  &
    & linSpaceElemSD=linSpaceElemSD, reset=reset, scale=scale, times=times)

  DO ii = 1, tnbc; funcPtrs(ii)%ptr => NULL(); END DO
  DEALLOCATE (funcPtrs)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER

END SUBROUTINE KernelAssembleSurfaceForce1

!----------------------------------------------------------------------------
!                                               KernelAssembleSurfaceForce2
!----------------------------------------------------------------------------

SUBROUTINE KernelAssembleSurfaceForce2(rhs, dom, nbcPtrs, funcPtrs, fe,  &
  & linFE, spaceElemSD, linSpaceElemSD, reset, scale, times)
  CLASS(VectorField_), INTENT(INOUT) :: rhs
  CLASS(Domain_), INTENT(INOUT) :: dom
  TYPE(NeumannBCPointer_), INTENT(INOUT) :: nbcPtrs(:)
  TYPE(UserFunctionPointer_), INTENT(INOUT) :: funcPtrs(:)
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: fe(:)
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: linFE(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: spaceElemSD(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: linSpaceElemSD(:)
  LOGICAL(LGT), INTENT(IN) :: reset
  REAL(DFP), INTENT(IN) :: scale
  REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelAssembleSurfaceForce2()"
  TYPE(FEVariable_) :: forceVar
  TYPE(ElemShapeData_) :: elemsd, linElemSD
  CLASS(Mesh_), POINTER :: meshptr
  CLASS(ReferenceElement_), POINTER :: refelem
  CLASS(FiniteElement_), POINTER :: spaceFE, linSpaceFE
  CLASS(NeumannBC_), POINTER :: nbc
  CLASS(UserFunction_), POINTER :: func
  LOGICAL(LGT) :: problem, isNormal, isTangent
  INTEGER(I4B) :: tmesh, nsd, id, nns, iel, tnbc, nbcNo, idof, jd,  &
    & returnType, tfunc
  INTEGER(I4B), ALLOCATABLE :: nptrs(:), meshID(:)
  REAL(DFP), ALLOCATABLE :: fevec(:), xij(:, :)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  IF (reset) CALL rhs%Set(VALUE=0.0_DFP)

  tnbc = SIZE(nbcPtrs)
  tfunc = SIZE(funcPtrs)

  problem = tnbc .NE. tfunc
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
     & '[INTERNAL ERROR] :: Size of nbcPtrs not same as the size of funcPtrs')
    RETURN
  END IF

  nsd = dom%GetNSD()
  NULLIFY (meshptr, refelem, spaceFE, linSpaceFE, nbc, func)

  DO nbcNo = 1, tnbc

    func => NULL(); func => funcPtrs(nbcNo)%ptr

    problem = .NOT. ASSOCIATED(func)
    IF (problem) CYCLE

    returnType = func%GetReturnType()
    problem = returnType .NE. Scalar
    IF (problem) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: The return type of func should be a vector.')
    END IF

    nbc => nbcPtrs(nbcNo)%ptr
    problem = .NOT. ASSOCIATED(nbc)
    IF (problem) CYCLE

    CALL nbc%boundary%GetParam(isSelectionByMeshID=problem,  &
      & isNormal=isNormal, isTangent=isTangent)

    IF (problem) THEN
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        & '[SKIPPING] :: Currently, found isSelectionByMeshID false.')
      CYCLE
    END IF

    problem = isNormal .OR. isTangent
    IF (problem) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[SKIPPING] :: Currently, normal and tangential boundary '// &
        & ' conditions are not supported.')
      RETURN
    END IF

    CALL nbc%boundary%GetParam(isnor=problem)

    IF (problem) THEN
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        & '[SKIPPING] :: Currently, found isSelectionByMeshID false.')
      CYCLE
    END IF

    meshID = nbc%GetMeshID(dim=nsd - 1_I4B)
    idof = nbc%GetDOFNo()
    tmesh = SIZE(meshID)

    DO id = 1, tmesh
      jd = meshID(id)
      meshptr => dom%GetMeshPointer(dim=nsd - 1_I4B, entityNum=jd)

      problem = meshptr%isEmpty()
      IF (problem) CYCLE

      spaceFE => fe(jd)%ptr
      linSpaceFE => linFE(jd)%ptr

      elemsd = spaceElemSD(jd)
      linElemSD = linSpaceElemSD(jd)

      refelem => meshptr%GetRefElemPointer()
      nns = (.nne.refelem)

      CALL Reallocate(xij, nsd, nns)
      CALL Reallocate(fevec, nns)

      DO iel = meshptr%minElemNum, meshptr%maxElemNum

        problem = .NOT. meshptr%IsElementPresent(iel)
        IF (problem) CYCLE

        nptrs = meshptr%GetConnectivity(iel)
        CALL dom%GetNodeCoord(nodeCoord=xij, globalNode=nptrs)

        CALL spaceFE%GetGlobalElemShapeData(elemsd=elemsd, xij=xij,  &
          & geoElemSD=linElemSD)

        CALL func%Get(fevar=forceVar, xij=xij, times=times)

        fevec = ForceVector(test=elemsd, c=forceVar,  &
          & crank=TypeFEVariableScalar)

        CALL rhs%Set(globalNode=nptrs, VALUE=fevec, spacecompo=idof, &
          & scale=scale, addContribution=.TRUE.)
      END DO
    END DO

  END DO

  NULLIFY (meshptr, refelem, spaceFE, linSpaceFE, nbc, func)

  IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
  IF (ALLOCATED(meshID)) DEALLOCATE (meshID)
  IF (ALLOCATED(fevec)) DEALLOCATE (fevec)
  IF (ALLOCATED(xij)) DEALLOCATE (xij)

  CALL DEALLOCATE (forceVar)
  CALL DEALLOCATE (elemsd)
  CALL DEALLOCATE (linElemSD)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER

END SUBROUTINE KernelAssembleSurfaceForce2

!----------------------------------------------------------------------------
!                                                 KernelAssembleSurfaceForce
!----------------------------------------------------------------------------

SUBROUTINE KernelAssembleSurfaceForce3(rhs, extField, dom, nbcPtrs, fe,  &
  & linFE, spaceElemSD, linSpaceElemSD, reset, scale, times)
  CLASS(VectorField_), INTENT(INOUT) :: rhs
    !! rhs to assemble
  CLASS(VectorField_), INTENT(INOUT) :: extField
    !! Temporary field which will contains the nodal values of
    !! surface force
    !! If nbc isExternal is true then it means
    !! extField will contain the boundary condition
    !! otherwise we read boundary condition in the extField
  CLASS(Domain_), INTENT(INOUT) :: dom
    !! domain
  TYPE(NeumannBCPointer_), INTENT(INOUT) :: nbcPtrs(:)
    !! Neumann boundary condition pointers
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: fe(:)
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: linFE(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: spaceElemSD(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: linSpaceElemSD(:)
  LOGICAL(LGT), INTENT(IN) :: reset
  REAL(DFP), INTENT(IN) :: scale
  REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelAssembleSurfaceForce3()"
  TYPE(FEVariable_) :: forceVar
  TYPE(ElemShapeData_) :: elemsd, linElemSD
  CLASS(Mesh_), POINTER :: meshptr
  CLASS(ReferenceElement_), POINTER :: refelem
  CLASS(FiniteElement_), POINTER :: spaceFE, linSpaceFE
  CLASS(NeumannBC_), POINTER :: nbc
  LOGICAL(LGT) :: problem, isNormal, isTangent
  INTEGER(I4B) :: tmesh, nsd, id, nns, iel, tnbc, nbcNo, idof, jd,  &
    & returnType
  INTEGER(I4B), ALLOCATABLE :: nptrs(:), meshID(:)
  REAL(DFP), ALLOCATABLE :: fevec(:, :), xij(:, :), forceVec(:, :)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  IF (reset) CALL rhs%Set(VALUE=0.0_DFP)

  tnbc = SIZE(nbcPtrs)
  nsd = dom%GetNSD()
  NULLIFY (meshptr, refelem, spaceFE, linSpaceFE, nbc)

  DO nbcNo = 1, tnbc
    nbc => nbcPtrs(nbcNo)%ptr
    problem = .NOT. ASSOCIATED(nbc)
    IF (problem) CYCLE

    CALL nbc%GetParam(useExternal=problem)
    IF (problem) CYCLE

    CALL extField%ApplyDirichletBC(dbc=nbc, times=times)
    nbc => NULL()
  END DO

  DO nbcNo = 1, tnbc
    nbc => nbcPtrs(nbcNo)%ptr
    problem = .NOT. ASSOCIATED(nbc)
    IF (problem) CYCLE

    CALL nbc%boundary%GetParam(isSelectionByMeshID=problem,  &
      & isNormal=isNormal, isTangent=isTangent)

    IF (problem) THEN
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        & '[SKIPPING] :: Currently, found isSelectionByMeshID false.')
      CYCLE
    END IF

    problem = isTangent .OR. isNormal
    IF (problem) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: Currently, normal and tangential '//  &
        & ' Neumann boundary condition are not supported.')
      RETURN
    END IF

    meshID = nbc%GetMeshID(dim=nsd - 1_I4B)
    idof = nbc%GetDOFNo()
    tmesh = SIZE(meshID)

    DO id = 1, tmesh
      jd = meshID(id)
      meshptr => dom%GetMeshPointer(dim=nsd - 1_I4B, entityNum=jd)

      problem = meshptr%isEmpty()
      IF (problem) CYCLE

      spaceFE => fe(jd)%ptr
      linSpaceFE => linFE(jd)%ptr

      elemsd = spaceElemSD(jd)
      linElemSD = linSpaceElemSD(jd)

      refelem => meshptr%GetRefElemPointer()
      nns = (.nne.refelem)

      CALL Reallocate(xij, nsd, nns)
      CALL Reallocate(fevec, nsd, nns)
      CALL Reallocate(forceVec, nsd, nns)

      DO iel = meshptr%minElemNum, meshptr%maxElemNum

        problem = .NOT. meshptr%IsElementPresent(iel)
        IF (problem) CYCLE

        nptrs = meshptr%GetConnectivity(iel)
        CALL dom%GetNodeCoord(nodeCoord=xij, globalNode=nptrs)

        CALL spaceFE%GetGlobalElemShapeData(elemsd=elemsd, xij=xij,  &
          & geoElemSD=linElemSD)

        CALL extField%Get(VALUE=forceVec, globalNode=nptrs)

        forceVar = NodalVariable(val=forceVec, rank=TypeFEVariableVector,  &
          & vartype=TypeFEVariableSpace)

        fevec = ForceVector(test=elemsd, c=forceVar,  &
          & crank=TypeFEVariableVector)

        CALL rhs%Set(globalNode=nptrs, VALUE=fevec, scale=scale,  &
          & addContribution=.TRUE.)
      END DO
    END DO

  END DO

  NULLIFY (meshptr, refelem, spaceFE, linSpaceFE, nbc)

  IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
  IF (ALLOCATED(meshID)) DEALLOCATE (meshID)
  IF (ALLOCATED(fevec)) DEALLOCATE (fevec)
  IF (ALLOCATED(xij)) DEALLOCATE (xij)

  CALL DEALLOCATE (forceVar)
  CALL DEALLOCATE (elemsd)
  CALL DEALLOCATE (linElemSD)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER

END SUBROUTINE KernelAssembleSurfaceForce3

END MODULE KernelAssembleSurfaceForce_Method
