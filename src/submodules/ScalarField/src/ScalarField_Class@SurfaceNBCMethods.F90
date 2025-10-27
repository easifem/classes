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
USE BaseType, ONLY: QuadraturePoint_, ElemShapeData_, TypeFEVariableScalar, &
                    TypeFEVariableSpace
USE FEVariable_Method, ONLY: NodalVariable, &
                             FEVariable_Set => Set
USE AbstractFE_Class, ONLY: AbstractFE_
USE ForceVector_Method, ONLY: ForceVector_

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
INTEGER(I4B), ALLOCATABLE :: cellCon(:), geoCellCon(:), geoFacetCon(:)
REAL(DFP), ALLOCATABLE :: xij(:, :), nbcValue(:), forceVec(:)
TYPE(FEVariable_) :: forceVar
TYPE(QuadraturePoint_) :: quad, facetQuad
TYPE(ElemShapeData_) :: elemsd, facetElemsd, geoElemsd, geoFacetElemsd

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL nbcField%SetAll(VALUE=zero)
tbc = obj%GetTotalNBC()
DO ibc = 1, tbc
  nbc => obj%GetNBCPointer(ibc)
  isok = ASSOCIATED(nbc)
  IF (.NOT. isok) CYCLE
  CALL nbcField%ApplyDirichletBC(dbc=nbc, times=times, ivar=ivar, &
                                 extField=extField)
END DO
nbc => NULL()

maxNNEGeo = obj%geofedof%GetMaxTotalConnectivity()
maxNNE = obj%fedof%GetMaxTotalConnectivity()

CALL Reallocate(geoCellCon, maxNNEGeo)
CALL Reallocate(geoFacetCon, maxNNEGeo)
CALL Reallocate(xij, 3, maxNNEGeo)
CALL Reallocate(cellCon, maxNNE)
CALL Reallocate(forceVec, maxNNE)
CALL Reallocate(nbcValue, maxNNE)

forceVar = NodalVariable(val=nbcValue, rank=TypeFEVariableScalar, &
                         vartype=TypeFEVariableSpace)

mesh => obj%fedof%GetMeshPointer()

DO ibc = 1, tbc
  nbc => obj%GetNBCPointer(ibc)

  isok = ASSOCIATED(nbc)
  IF (.NOT. isok) CYCLE

  CALL ScalarFieldAssembleSurfaceSource( &
    obj=obj, nbc=nbc, fedof=obj%fedof, geofedof=obj%geofedof, &
    mesh=mesh, nbcField=nbcField, scale=scale, geoCellCon=geoCellCon, &
    geoFacetCon=geoFacetCon, cellCon=cellCon, xij=xij, &
    forceVec=forceVec, nbcValue=nbcValue, forceVar=forceVar, quad=quad, &
    facetQuad=facetQuad, elemsd=elemsd, facetElemsd=facetElemsd, &
    geoElemsd=geoElemsd, geoFacetElemsd=geoFacetElemsd)
END DO

IF (ALLOCATED(geoCellCon)) DEALLOCATE (geoCellCon)
IF (ALLOCATED(geoFacetCon)) DEALLOCATE (geoFacetCon)
IF (ALLOCATED(xij)) DEALLOCATE (xij)
IF (ALLOCATED(cellCon)) DEALLOCATE (cellCon)
IF (ALLOCATED(forceVec)) DEALLOCATE (forceVec)
IF (ALLOCATED(nbcValue)) DEALLOCATE (nbcValue)
NULLIFY (nbc)

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
  obj, nbc, fedof, geofedof, mesh, nbcField, scale, geoCellCon, &
  geoFacetCon, cellCon, xij, forceVec, nbcValue, forceVar, &
  quad, facetQuad, elemsd, facetElemsd, geoElemsd, geoFacetElemsd)
  CLASS(ScalarField_), INTENT(INOUT) :: obj
  CLASS(NeumannBC_), INTENT(INOUT) :: nbc
  CLASS(FEDOF_), INTENT(INOUT) :: fedof
  CLASS(FEDOF_), INTENT(INOUT) :: geofedof
  CLASS(AbstractMesh_), INTENT(INOUT) :: mesh
  CLASS(ScalarField_), INTENT(INOUT) :: nbcField
  REAL(DFP), INTENT(IN) :: scale
  INTEGER(I4B), INTENT(INOUT) :: geoCellCon(:), geoFacetCon(:), &
                                 cellCon(:)
  !! Working arrays for connectivity
  !! geoCellCon: connectivity of the geometry
  !! geoFacetCon: connectivity of the geometry facet element
  !! cellCon: connectivity of the call
  REAL(DFP), INTENT(INOUT) :: xij(:, :), forceVec(:), &
                              nbcValue(:)
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
  INTEGER(I4B) :: tElemToFace, indx, localCellNumber, &
                  localFaceNumber, maxNNEGeo, maxNNE, tgeoCellCon, &
                  tgeoFacetCon, tcellCon, tnbcValue, &
                  tforceVec, xij_i, xij_j
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

  maxNNEGeo = geofedof%GetMaxTotalConnectivity()
  maxNNE = fedof%GetMaxTotalConnectivity()
  tElemToFace = nbc%GetTotalElemToFace()

  DO indx = 1, tElemToFace
    CALL nbc%GetElemToFace(indx=indx, localCellNumber=localCellNumber, &
                           localFaceNumber=localFaceNumber)

    CALL fedof%SetFE(globalElement=localCellNumber, islocal=yes)

    CALL geofedof%SetFE(globalElement=localCellNumber, islocal=yes)

    CALL fedof%GetConnectivity_( &
      globalElement=localCellNumber, islocal=yes, ans=cellCon, &
      tsize=tcellCon, opt="A")

    CALL geofedof%GetConnectivity_( &
      globalElement=localCellNumber, islocal=yes, ans=geoCellCon, &
      tsize=tgeoCellCon, opt="A")

    CALL geofedof%GetFacetConnectivity_( &
      globalElement=localCellNumber, islocal=yes, ans=geoFacetCon, &
      tsize=tgeoFacetCon, localFaceNumber=localFaceNumber)

    feptr => fedof%GetFEPointer(globalElement=localCellNumber, &
                                islocal=yes)

    geofeptr => geofedof%GetFEPointer(globalElement=localCellNumber, &
                                      islocal=yes)

    CALL feptr%GetFacetQuadraturePoints( &
      quad=quad, facetQuad=facetQuad, localFaceNumber=localFaceNumber)

    CALL feptr%GetLocalFacetElemShapeData( &
     elemsd=elemsd, facetElemsd=facetElemsd, quad=quad, facetQuad=facetQuad, &
      localFaceNumber=localFaceNumber)

    CALL geofeptr%GetLocalFacetElemShapeData( &
      elemsd=geoElemsd, facetElemsd=geoFacetElemsd, quad=quad, &
      facetQuad=facetQuad, localFaceNumber=localFaceNumber)

    CALL mesh%GetNodeCoord( &
      nodeCoord=xij, nrow=xij_i, ncol=xij_j, islocal=yes, &
      globalElement=localCellNumber)

    CALL feptr%GetGlobalFacetElemShapeData( &
    elemsd=elemsd, facetElemsd=facetElemsd, localFaceNumber=localFaceNumber, &
      geoElemsd=geoElemsd, geoFacetElemsd=geoFacetElemsd, xij=xij)

    CALL nbcField%Get(VALUE=nbcValue, globalNode=cellCon(1:tcellCon), &
                      tsize=tnbcValue, islocal=yes)

    CALL FEVariable_Set( &
      obj=forceVar, val=nbcValue(1:tnbcValue), rank=TypeFEVariableScalar, &
      vartype=TypeFEVariableSpace, scale=one, addContribution=no)

    CALL ForceVector_(test=elemsd, c=forceVar, crank=TypeFEVariableScalar, &
                      ans=forceVec, tsize=tforceVec)

    CALL obj%Set(globalNode=cellCon(1:tcellCon), &
                 VALUE=forceVec(1:tforceVec), scale=scale, &
                 addContribution=yes, islocal=yes)

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
