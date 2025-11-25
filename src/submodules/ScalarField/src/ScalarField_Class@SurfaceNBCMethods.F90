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

SUBMODULE(ScalarField_Class) SurfaceNBCMethods
USE Display_Method, ONLY: ToString
USE ReallocateUtility, ONLY: Reallocate
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: TypeFEVariableScalar
USE BaseType, ONLY: TypeFEVariableSpace
USE BaseType, ONLY: math => TypeMathOpt
USE FEVariable_Method, ONLY: NodalVariable
USE FEVariable_Method, ONLY: FEVariable_Set => Set
USE FEVariable_Method, ONLY: FEVariable_Deallocate => DEALLOCATE
USE QuadraturePoint_Method, ONLY: QuadraturePoint_Deallocate => DEALLOCATE
USE ElemshapeData_Method, ONLY: ElemShapeData_Deallocate => DEALLOCATE
USE AbstractFE_Class, ONLY: AbstractFE_
USE ForceVector_Method, ONLY: ForceVector_
USE NeumannBC_Class, ONLY: NeumannBC_
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE InputUtility, ONLY: Input

#ifdef DEBUG_VER
USE Display_Method, ONLY: Display
#endif

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       ApplySurfaceNeumannBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplySurfaceNeumannBC
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplySurfaceNeumannBC()"
#endif

REAL(DFP), PARAMETER :: zero = 0.0_DFP

CLASS(NeumannBC_), POINTER :: nbc
CLASS(AbstractMesh_), POINTER :: mesh

LOGICAL(LGT) :: isok

INTEGER(I4B) :: tbc, ibc, maxNNEGeo, maxNNE
INTEGER(I4B), ALLOCATABLE :: facetCon(:)
REAL(DFP) :: times0(1)
REAL(DFP), ALLOCATABLE :: xij(:, :), nbcValue(:), forceVec(:)
TYPE(FEVariable_) :: forceVar
TYPE(QuadraturePoint_) :: quad, facetQuad
TYPE(ElemShapeData_) :: elemsd, facetElemsd, geoElemsd, geoFacetElemsd

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

times0(1) = Input(option=times, default=math%zero)

tbc = obj%GetTotalNBC()

isok = tbc .GT. 0
IF (.NOT. isok) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

CALL nbcField%SetAll(VALUE=zero)

DO ibc = 1, tbc
  nbc => obj%GetNBCPointer(ibc)
  isok = ASSOCIATED(nbc)
  IF (.NOT. isok) CYCLE
  CALL nbcField%ApplyDirichletBC(dbc=nbc, times=times0, ivar=ivar, &
                                 extField=extField)
END DO
nbc => NULL()

maxNNEGeo = obj%geofedof%GetMaxTotalConnectivity()
maxNNE = obj%fedof%GetMaxTotalConnectivity()

CALL Reallocate(xij, 3, maxNNEGeo)
CALL Reallocate(facetCon, maxNNE)
CALL Reallocate(forceVec, maxNNE)
CALL Reallocate(nbcValue, maxNNE)

forceVar = NodalVariable(tsize=maxNNE, rank=TypeFEVariableScalar, &
                         vartype=TypeFEVariableSpace)

mesh => obj%fedof%GetMeshPointer()

DO ibc = 1, tbc
  nbc => obj%GetNBCPointer(ibc)

  isok = ASSOCIATED(nbc)
  IF (.NOT. isok) CYCLE

  CALL ScalarFieldAssembleSurfaceSource( &
    obj=obj, nbc=nbc, fedof=obj%fedof, geofedof=obj%geofedof, &
    mesh=mesh, nbcField=nbcField, scale=scale, xij=xij, &
    forceVec=forceVec, nbcValue=nbcValue, forceVar=forceVar, quad=quad, &
    facetQuad=facetQuad, elemsd=elemsd, facetElemsd=facetElemsd, &
    geoElemsd=geoElemsd, geoFacetElemsd=geoFacetElemsd, &
    facetCon=facetCon)
END DO

DEALLOCATE (facetCon, xij, nbcValue, forceVec)

CALL FEVariable_Deallocate(forceVar)
CALL QuadraturePoint_Deallocate(quad)
CALL QuadraturePoint_Deallocate(facetQuad)
CALL ElemShapeData_Deallocate(elemsd)
CALL ElemShapeData_Deallocate(facetElemsd)
CALL ElemShapeData_Deallocate(geoElemsd)
CALL ElemShapeData_Deallocate(geoFacetElemsd)

NULLIFY (nbc, mesh)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ApplySurfaceNeumannBC

!----------------------------------------------------------------------------
!                        ScalarFieldAssembleSurfaceSource@ScalarFieldMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-05
! summary: Assemble surface source into RHS
!
!# Introduction
!
! This is an internal routine that assemble surface source (Neumann BC)
! into the right-hand side (RHS) vector of a scalar field.

SUBROUTINE ScalarFieldAssembleSurfaceSource( &
  obj, nbc, fedof, geofedof, mesh, nbcField, scale, xij, forceVec, &
  nbcValue, forceVar, quad, facetQuad, elemsd, facetElemsd, geoElemsd, &
  geoFacetElemsd, facetCon)
  CLASS(ScalarField_), INTENT(INOUT) :: obj
  CLASS(NeumannBC_), INTENT(INOUT) :: nbc
  CLASS(FEDOF_), INTENT(INOUT) :: fedof
  CLASS(FEDOF_), INTENT(INOUT) :: geofedof
  CLASS(AbstractMesh_), INTENT(INOUT) :: mesh
  CLASS(ScalarField_), INTENT(INOUT) :: nbcField
  REAL(DFP), INTENT(IN) :: scale
  INTEGER(I4B), INTENT(INOUT) :: facetCon(:)
  !! Working arrays for connectivity
  !! geoCellCon: connectivity of the geometry
  !! geoFacetCon: connectivity of the geometry facet element
  !! cellCon: connectivity of the call
  REAL(DFP), INTENT(INOUT) :: xij(:, :), forceVec(:), nbcValue(:)
  !! Working arrays for element vectors and matrix
  TYPE(FEVariable_), INTENT(INOUT) :: forceVar
  !! Working variables
  TYPE(QuadraturePoint_), INTENT(INOUT) :: quad, facetQuad
  !! Working variables
  TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd, facetElemsd, geoElemsd, &
                                         geoFacetElemsd
  !! Working variables for containing shape function data

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ScalarFieldAssembleSurfaceSource()"
#endif

  LOGICAL(LGT), PARAMETER :: yes = .TRUE., no = .FALSE.
  REAL(DFP), PARAMETER :: one = 1.0_DFP

  LOGICAL(LGT) :: isElemToEdge, isElemToFace, isok
  INTEGER(I4B) :: tElemToFace, indx, localCellNumber, localFaceNumber, &
                  tnbcValue, tforceVec, &
                  xij_i, xij_j, tFacetCon
  CLASS(AbstractFE_), POINTER :: feptr, geofeptr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isElemToEdge = nbc%IsElemToEdgeInitiated()
  isElemToFace = nbc%IsElemToFaceInitiated()

  isok = isElemToEdge .OR. isElemToFace
  IF (.NOT. isok) THEN
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
          'isElemToEdge and isElemToFace are both .false. So, nothing to do.')

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif

    RETURN
  END IF

  tElemToFace = nbc%GetTotalElemToFace()

  DO indx = 1, tElemToFace
    CALL nbc%GetElemToFace(indx=indx, localCellNumber=localCellNumber, &
                           localFaceNumber=localFaceNumber)

    CALL fedof%SetFE(globalElement=localCellNumber, islocal=yes)
    feptr => fedof%GetFEPointer(globalElement=localCellNumber, &
                                islocal=yes)

    CALL geofedof%SetFE(globalElement=localCellNumber, islocal=yes)
    geofeptr => geofedof%GetFEPointer(globalElement=localCellNumber, &
                                      islocal=yes)

    CALL fedof%GetFacetConnectivity_( &
      globalElement=localCellNumber, islocal=yes, ans=facetCon, &
      tsize=tFacetCon, localFaceNumber=localFaceNumber)

    CALL mesh%GetNodeCoord( &
      nodeCoord=xij, nrow=xij_i, ncol=xij_j, islocal=yes, &
      globalElement=localCellNumber)

    CALL feptr%GetGlobalFacetElemShapeData2( &
      geofeptr=geofeptr, elemsd=elemsd, facetElemsd=facetElemsd, &
      geoElemsd=geoElemsd, geoFacetElemsd=geoFacetElemsd, &
      localFaceNumber=localFaceNumber, quad=quad, facetQuad=facetQuad, &
      xij=xij)

    CALL nbcField%Get(VALUE=nbcValue, globalNode=facetCon(1:tFacetCon), &
                      tsize=tnbcValue, islocal=yes)

    CALL FEVariable_Set( &
      obj=forceVar, val=nbcValue(1:tnbcValue), rank=TypeFEVariableScalar, &
      vartype=TypeFEVariableSpace, scale=one, addContribution=no)

    CALL ForceVector_( &
      test=facetElemsd, c=forceVar, crank=TypeFEVariableScalar, &
      ans=forceVec, tsize=tforceVec)

    CALL obj%Set( &
      globalNode=facetCon(1:tFacetCon), VALUE=forceVec(1:tforceVec), &
      scale=scale, addContribution=yes, islocal=yes)
  END DO

  NULLIFY (feptr, geofeptr)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ScalarFieldAssembleSurfaceSource

!----------------------------------------------------------------------------
!                                                             Include Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SurfaceNBCMethods
