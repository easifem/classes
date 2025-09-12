! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

SUBMODULE(AssembleSurfaceSourceUtility) ScalarFieldMethods
USE Display_Method, ONLY: ToString, Display
USE ReallocateUtility, ONLY: Reallocate
USE BaseType, ONLY: QuadraturePoint_, ElemShapeData_, &
                    FEVariable_, TypeFEVariableScalar, &
                    TypeFEVariableSpace
USE ElemshapeData_Method, ONLY: Elemsd_Set => Set

USE FEVariable_Method, ONLY: NodalVariable, Fevar_Set => Set

USE forceVector_Method, ONLY: forceVector_

#ifdef DEBUG_VER
USE QuadraturePoint_Method, ONLY: QP_Display => Display
USE ElemshapeData_Method, ONLY: Elemsd_Display => Display
USE FEVariable_Method, ONLY: Fevar_Display => Display
#endif

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                            ScalarFieldAssembleSurfaceSource
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarFieldAssembleSurfaceSource1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ScalarFieldAssembleSurfaceSource1()"
#endif

LOGICAL(LGT) :: isElemToEdge, isElemToFace, isok
INTEGER(I4B) :: tElemToFace, indx, localCellNumber, &
                localFaceNumber, maxNNEGeo, maxNNE, tgeoCellCon, &
                tgeoFacetCon, nrow, ncol, tcellCon, tnbcValue, &
                tforceVec

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

  CALL fedof%GetfacetQuadraturePoints( &
    quad=quad, facetQuad=facetQuad, globalElement=localCellNumber, &
    localFaceNumber=localFaceNumber, islocal=defaultOpt%yes)

  CALL fedof%GetLocalFacetElemShapeData( &
    globalElement=localCellNumber, elemsd=elemsd, facetElemsd=facetElemsd, &
    islocal=defaultOpt%yes, quad=quad, facetQuad=facetQuad, &
    localFaceNumber=localFaceNumber)

  CALL fedof%GetConnectivity_( &
    globalElement=localCellNumber, islocal=defaultOpt%yes, ans=cellCon, &
    tsize=tcellCon, opt="A")

  CALL geofedof%GetLocalFacetElemShapeData( &
    globalElement=localCellNumber, elemsd=geoElemsd, &
    facetElemsd=geoFacetElemsd, islocal=defaultOpt%yes, quad=quad, &
    facetQuad=facetQuad, localFaceNumber=localFaceNumber)

  CALL geofedof%GetConnectivity_( &
    globalElement=localCellNumber, islocal=defaultOpt%yes, ans=geoCellCon, &
    tsize=tgeoCellCon, opt="A")

  CALL geofedof%GetFacetConnectivity_( &
    globalElement=localCellNumber, islocal=defaultOpt%yes, ans=geoFacetCon, &
    tsize=tgeoFacetCon, localFaceNumber=localFaceNumber)

  CALL nodeCoord%Get( &
    VALUE=xij, nrow=nrow, ncol=ncol, &
    storageFMT=defaultOpt%storageFormatNodes, &
    globalNode=geoCellCon(1:tgeoCellCon), islocal=defaultOpt%yes)

  CALL nodeCoord%Get( &
    VALUE=facetXij, nrow=nrow, ncol=ncol, &
    storageFMT=TypeFieldOpt%storageFormatNodes, &
    globalNode=geoFacetCon(1:tgeoFacetCon), islocal=defaultOpt%yes)

  CALL Elemsd_Set( &
    facetobj=facetElemsd, &
    cellobj=elemsd, &
    cellval=xij(1:geoElemsd%nsd, 1:tgeoCellCon), &
    facetval=facetXij(1:geoFacetElemsd%nsd, 1:tgeoFacetCon), &
    cellN=geoElemsd%N(1:geoElemsd%nns, 1:geoElemsd%nips), &
    celldNdXi=geoElemsd%dNdXi(1:geoElemsd%nns, 1:geoElemsd%xidim, &
                              1:geoElemsd%nips), &
    facetN=geoFacetElemsd%N(1:geoFacetElemsd%nns, 1:geoFacetElemsd%nips), &
    facetdNdXi=geoFacetElemsd%dNdXi(1:geoFacetElemsd%nns, &
                                    1:geoFacetElemsd%xidim, &
                                    1:geoFacetElemsd%nips))

  CALL nbcField%Get(VALUE=nbcValue, globalNode=cellCon(1:tcellCon), &
                    tsize=tnbcValue, islocal=defaultOpt%yes)

  ! forceVar = NodalVariable(val=nbcValue, rank=TypeFEVariableScalar, &
  !                          vartype=TypeFEVariableSpace)

  CALL Fevar_Set(obj=forceVar, val=nbcValue(1:tnbcValue), &
                 rank=TypeFEVariableScalar, &
                 vartype=TypeFEVariableSpace, &
                 scale=defaultOpt%one, &
                 addContribution=defaultOpt%no)

  CALL forceVector_(test=elemsd, c=forceVar, crank=TypeFEVariableScalar, &
                    ans=forceVec, tsize=tforceVec)

  CALL obj%Set(globalNode=cellCon(1:tcellCon), VALUE=forceVec(1:tforceVec), &
               scale=scale, addContribution=defaultOpt%yes, &
               islocal=defaultOpt%yes)

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE ScalarFieldAssembleSurfaceSource1

!----------------------------------------------------------------------------
!                                            ScalarFieldAssembleSurfaceSource
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarFieldAssembleSurfaceSource2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ScalarFieldAssembleSurfaceSource2()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: tbc, ibc, maxNNEGeo, maxNNE
CLASS(NeumannBC_), POINTER :: nbc
INTEGER(I4B), ALLOCATABLE :: cellCon(:), geoCellCon(:), geoFacetCon(:)
REAL(DFP), ALLOCATABLE :: xij(:, :), facetXij(:, :), nbcValue(:), &
                          forceVec(:)
TYPE(FEVariable_) :: forceVar
TYPE(QuadraturePoint_) :: quad, facetQuad
TYPE(ElemShapeData_) :: elemsd, facetElemsd, geoElemsd, geoFacetElemsd

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

maxNNEGeo = geofedof%GetMaxTotalConnectivity()
maxNNE = obj%fedof%GetMaxTotalConnectivity()

CALL Reallocate(geoCellCon, maxNNEGeo)
CALL Reallocate(geoFacetCon, maxNNEGeo)
CALL Reallocate(xij, 3, maxNNEGeo)
CALL Reallocate(facetXij, 3, maxNNEGeo)
CALL Reallocate(cellCon, maxNNE)
CALL Reallocate(forceVec, maxNNE)
CALL Reallocate(nbcValue, maxNNE)
forceVar = NodalVariable(val=nbcValue, rank=TypeFEVariableScalar, &
                         vartype=TypeFEVariableSpace)

tbc = obj%GetTotalNBC()

DO ibc = 1, tbc
  nbc => obj%GetNBCPointer(ibc)

#ifdef DEBUG_VER
  isok = ASSOCIATED(nbc)
  CALL AssertError1(isok, myName, &
                    "nbc pointer is not associated. ibc="//ToString(ibc))
#endif

  CALL ScalarFieldAssembleSurfaceSource( &
    obj=obj, nbc=nbc, fedof=obj%fedof, geofedof=geofedof, &
    nodeCoord=nodeCoord, nbcField=nbcField, scale=scale, &
    geoCellCon=geoCellCon, geoFacetCon=geoFacetCon, cellCon=cellCon, &
    xij=xij, facetXij=facetXij, forceVec=forceVec, nbcValue=nbcValue, &
    forceVar=forceVar, quad=quad, facetQuad=facetQuad, elemsd=elemsd, &
    facetElemsd=facetElemsd, geoElemsd=geoElemsd, &
    geoFacetElemsd=geoFacetElemsd)
END DO

IF (ALLOCATED(geoCellCon)) DEALLOCATE (geoCellCon)
IF (ALLOCATED(geoFacetCon)) DEALLOCATE (geoFacetCon)
IF (ALLOCATED(xij)) DEALLOCATE (xij)
IF (ALLOCATED(facetXij)) DEALLOCATE (facetXij)
IF (ALLOCATED(cellCon)) DEALLOCATE (cellCon)
IF (ALLOCATED(forceVec)) DEALLOCATE (forceVec)
IF (ALLOCATED(nbcValue)) DEALLOCATE (nbcValue)
NULLIFY (nbc)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE ScalarFieldAssembleSurfaceSource2

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ScalarFieldMethods
