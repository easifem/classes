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

SUBMODULE(STScalarField_Class) SurfaceNBCMethods
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: TypeFEVariableScalar
USE BaseType, ONLY: TypeFEVariableSpaceTime
USE BaseType, ONLY: math => TypeMathOpt
USE ReallocateUtility, ONLY: Reallocate
USE FEVariable_Method, ONLY: NodalVariable
USE FEVariable_Method, ONLY: FEVariable_Set => Set
USE FEVariable_Method, ONLY: FEVariable_Deallocate => DEALLOCATE
USE QuadraturePoint_Method, ONLY: QuadraturePoint_Deallocate => DEALLOCATE
USE ElemshapeData_Method, ONLY: ElemShapeData_Deallocate => DEALLOCATE
USE AbstractFE_Class, ONLY: AbstractFE_
USE AbstractOneDimFE_Class, ONLY: AbstractOneDimFE_
USE STForceVector_Method, ONLY: STForceVector_
USE NeumannBC_Class, ONLY: NeumannBC_
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE FieldOpt_Class, ONLY: TypeFieldOpt

#ifdef DEBUG_VER
USE Display_Method, ONLY: Display
USE QuadraturePoint_Method, ONLY: QuadraturePoint_Display => Display
USE ElemshapeData_Method, ONLY: ElemshapeData_Display => Display
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

LOGICAL(LGT) :: isok

CLASS(NeumannBC_), POINTER :: nbc
CLASS(AbstractMesh_), POINTER :: mesh
CLASS(AbstractOneDimFE_), POINTER :: timefeptr

INTEGER(I4B) :: tbc, ibc, maxNNSGeo, maxNNS, maxNNT
INTEGER(I4B), ALLOCATABLE :: facetCon(:)
REAL(DFP), ALLOCATABLE :: xij(:, :), nbcValue(:, :), forceVec(:, :)

TYPE(FEVariable_) :: forceVar
TYPE(QuadraturePoint_) :: quad, facetQuad, timeQuad
TYPE(ElemShapeData_) :: elemsd, facetElemsd, geoElemsd, geoFacetElemsd, &
                        timeelemsd, timegeoelemsd

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tbc = obj%GetTotalNBC()
isok = tbc .GT. 0

IF (.NOT. isok) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

CALL nbcField%SetAll(VALUE=math%zero)

DO ibc = 1, tbc
  nbc => obj%GetNBCPointer(ibc)
  isok = ASSOCIATED(nbc)
  IF (.NOT. isok) CYCLE
  CALL nbcField%ApplyDirichletBC(dbc=nbc, times=times)
END DO
nbc => NULL()

mesh => obj%fedof%GetMeshPointer()
CALL obj%timefedof%SetFE()
timefeptr => obj%timefedof%GetFEPointer()
CALL timefeptr%GetGlobalTimeElemShapeData( &
  elemsd=timeelemsd, geoelemsd=timegeoelemsd, quad=timeQuad, times=times)

maxNNSGeo = obj%geofedof%GetMaxTotalConnectivity()
maxNNS = obj%fedof%GetMaxTotalConnectivity()
maxNNT = obj%timefedof%GetMaxTotalConnectivity()

CALL Reallocate(xij, 3, maxNNSGeo)
CALL Reallocate(facetCon, maxNNS)
CALL Reallocate(forceVec, maxNNS, maxNNT)
CALL Reallocate(nbcValue, maxNNS, maxNNT)

forceVar = NodalVariable( &
           nrow=maxNNS, ncol=maxNNT, rank=TypeFEVariableScalar, &
           vartype=TypeFEVariableSpaceTime)

DO ibc = 1, tbc
  nbc => obj%GetNBCPointer(ibc)

  isok = ASSOCIATED(nbc)
  IF (.NOT. isok) CYCLE

  CALL STScalarFieldAssembleSurfaceSource( &
    obj=obj, nbc=nbc, fedof=obj%fedof, geofedof=obj%geofedof, &
    mesh=mesh, nbcField=nbcField, scale=scale, xij=xij, &
    forceVec=forceVec, nbcValue=nbcValue, forceVar=forceVar, quad=quad, &
    facetQuad=facetQuad, elemsd=elemsd, facetElemsd=facetElemsd, &
    geoElemsd=geoElemsd, geoFacetElemsd=geoFacetElemsd, &
    facetCon=facetCon, timeelemsd=timeelemsd)
END DO

DEALLOCATE (xij, nbcValue, forceVec, facetCon)
NULLIFY (timefeptr, mesh, nbc)
CALL FEVariable_Deallocate(forceVar)
CALL QuadraturePoint_Deallocate(quad)
CALL QuadraturePoint_Deallocate(facetQuad)
CALL QuadraturePoint_Deallocate(timeQuad)
CALL ElemshapeData_Deallocate(elemsd)
CALL ElemshapeData_Deallocate(facetElemsd)
CALL ElemshapeData_Deallocate(geoElemsd)
CALL ElemshapeData_Deallocate(geoFacetElemsd)
CALL ElemshapeData_Deallocate(timeelemsd)
CALL ElemshapeData_Deallocate(timegeoelemsd)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ApplySurfaceNeumannBC

!----------------------------------------------------------------------------
!                        STScalarFieldAssembleSurfaceSource@ScalarFieldMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-05
! summary: Assemble surface source into RHS
!
!# Introduction
!
! This is an internal routine that assemble surface source (Neumann BC)
! into the right-hand side (RHS) vector of a scalar field.

SUBROUTINE STScalarFieldAssembleSurfaceSource( &
  obj, nbc, fedof, geofedof, mesh, nbcField, scale, xij, forceVec, &
  nbcValue, forceVar, quad, facetQuad, elemsd, facetElemsd, geoElemsd, &
  geoFacetElemsd, facetCon, timeelemsd)
  CLASS(STScalarField_), INTENT(INOUT) :: obj
  CLASS(NeumannBC_), INTENT(INOUT) :: nbc
  CLASS(FEDOF_), INTENT(INOUT) :: fedof
  CLASS(FEDOF_), INTENT(INOUT) :: geofedof
  CLASS(AbstractMesh_), INTENT(INOUT) :: mesh
  CLASS(STScalarField_), INTENT(INOUT) :: nbcField
  REAL(DFP), INTENT(IN) :: scale
  INTEGER(I4B), INTENT(INOUT) :: facetCon(:)
  !! Working arrays for connectivity
  !! geoCellCon: connectivity of the geometry
  !! geoFacetCon: connectivity of the geometry facet element
  !! cellCon: connectivity of the call
  REAL(DFP), INTENT(INOUT) :: xij(:, :), forceVec(:, :), nbcValue(:, :)
  !! Working arrays for element vectors and matrix
  TYPE(FEVariable_), INTENT(INOUT) :: forceVar
  !! Working variables
  TYPE(QuadraturePoint_), INTENT(INOUT) :: quad, facetQuad
  !! Working variables
  TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd, facetElemsd, geoElemsd, &
                                         geoFacetElemsd, timeelemsd
  !! Working variables for containing shape function data

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "STScalarFieldAssembleSurfaceSource()"
#endif

  LOGICAL(LGT) :: isElemToEdge, isElemToFace, isok
  INTEGER(I4B) :: tElemToFace, indx, localCellNumber, localFaceNumber, &
                  forceVec_i, forceVec_j, xij_i, xij_j, tFacetCon, &
                  nbcValue_i, nbcValue_j
  CLASS(AbstractFE_), POINTER :: feptr, geofeptr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isElemToEdge = nbc%IsElemToEdgeInitiated()
  isElemToFace = nbc%IsElemToFaceInitiated()

  isok = isElemToEdge .OR. isElemToFace
  IF (.NOT. isok) THEN
    CALL e%RaiseDebug(modName//'::'//myName//' - '// &
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

    CALL fedof%SetFE(globalElement=localCellNumber, islocal=math%yes)
    feptr => fedof%GetFEPointer(globalElement=localCellNumber, &
                                islocal=math%yes)

    CALL geofedof%SetFE(globalElement=localCellNumber, islocal=math%yes)
    geofeptr => geofedof%GetFEPointer(globalElement=localCellNumber, &
                                      islocal=math%yes)

    CALL fedof%GetFacetConnectivity_( &
      globalElement=localCellNumber, islocal=math%yes, ans=facetCon, &
      tsize=tFacetCon, localFaceNumber=localFaceNumber)

    CALL mesh%GetNodeCoord( &
      nodeCoord=xij, nrow=xij_i, ncol=xij_j, islocal=math%yes, &
      globalElement=localCellNumber)

    CALL feptr%GetGlobalFacetElemShapeData2( &
      geofeptr=geofeptr, elemsd=elemsd, facetElemsd=facetElemsd, &
      geoElemsd=geoElemsd, geoFacetElemsd=geoFacetElemsd, &
      localFaceNumber=localFaceNumber, quad=quad, facetQuad=facetQuad, &
      xij=xij)

    CALL nbcField%Get( &
      VALUE=nbcValue, globalNode=facetCon(1:tFacetCon), &
      nrow=nbcValue_i, ncol=nbcValue_j, islocal=math%yes, &
      storageFMT=TypeFieldOpt%storageFormatDOF)

    CALL FEVariable_Set( &
      obj=forceVar, val=nbcValue(1:nbcValue_i, 1:nbcValue_j), &
      rank=TypeFEVariableScalar, vartype=TypeFEVariableSpaceTime, &
      scale=math%one, addContribution=math%no)

    CALL STForceVector_( &
      testSpace=facetElemsd, testTime=timeelemsd, c=forceVar, &
      crank=TypeFEVariableScalar, ans=forceVec, nrow=forceVec_i, &
      ncol=forceVec_j)

    CALL obj%Set( &
      globalNode=facetCon(1:tFacetCon), &
      VALUE=forceVec(1:forceVec_i, 1:forceVec_j), &
      scale=scale, addContribution=math%yes, islocal=math%yes, &
      storageFMT=TypeFieldOpt%storageFormatDOF)
  END DO

  NULLIFY (feptr, geofeptr)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE STScalarFieldAssembleSurfaceSource

!----------------------------------------------------------------------------
!                                                             Include Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SurfaceNBCMethods
