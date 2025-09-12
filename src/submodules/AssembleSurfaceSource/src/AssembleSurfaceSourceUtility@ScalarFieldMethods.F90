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

USE ForceVector_Method, ONLY: ForceVector_

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
                tgeoFacetCon, nrow, ncol, tcellcon, tnbcvalue, &
                tforcevec

TYPE(QuadraturePoint_) :: quad, facetQuad
TYPE(ElemShapeData_) :: elemsd, facetElemsd, geoelemsd, geofacetElemsd
INTEGER(I4B), ALLOCATABLE :: cellcon(:), geoCellCon(:), geoFacetCon(:)
REAL(DFP), ALLOCATABLE :: xij(:, :), facetXij(:, :), nbcvalue(:), &
                          forcevec(:)
TYPE(FEVariable_) :: forceVar

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

CALL Reallocate(geoCellCon, maxNNEGeo)
CALL Reallocate(geoFacetCon, maxNNEGeo)
CALL Reallocate(xij, 3, maxNNEGeo)
CALL Reallocate(facetXij, 3, maxNNEGeo)
CALL Reallocate(cellcon, maxNNE)
CALL Reallocate(forcevec, maxNNE)
CALL Reallocate(nbcvalue, maxNNE)

tElemToFace = nbc%GetTotalElemToFace()

forceVar = NodalVariable(val=nbcvalue, rank=TypeFEVariableScalar, &
                         vartype=TypeFEVariableSpace)

DO indx = 1, tElemToFace
  CALL nbc%GetElemToFace(indx=indx, localCellNumber=localCellNumber, &
                         localFaceNumber=localFaceNumber)

  CALL fedof%GetFacetQuadraturePoints( &
    quad=quad, facetQuad=facetQuad, globalElement=localCellNumber, &
    localFaceNumber=localFaceNumber, islocal=defaultOpt%yes)

  CALL fedof%GetLocalFacetElemShapeData( &
    globalElement=localCellNumber, elemsd=elemsd, facetElemsd=facetElemsd, &
    islocal=defaultOpt%yes, quad=quad, facetQuad=facetQuad, &
    localFaceNumber=localFaceNumber)

  CALL fedof%GetConnectivity_( &
    globalElement=localCellNumber, islocal=defaultOpt%yes, ans=cellcon, &
    tsize=tcellcon, opt="A")

  CALL geofedof%GetLocalFacetElemShapeData( &
    globalElement=localCellNumber, elemsd=geoelemsd, &
    facetElemsd=geofacetElemsd, islocal=defaultOpt%yes, quad=quad, &
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
    cellval=xij(1:geoelemsd%nsd, 1:tgeoCellCon), &
    facetval=facetXij(1:geofacetElemsd%nsd, 1:tgeoFacetCon), &
    cellN=geoelemsd%N(1:geoelemsd%nns, 1:geoelemsd%nips), &
    celldNdXi=geoelemsd%dNdXi(1:geoelemsd%nns, 1:geoelemsd%xidim, &
                              1:geoelemsd%nips), &
    facetN=geofacetElemsd%N(1:geofacetElemsd%nns, 1:geofacetElemsd%nips), &
    facetdNdXi=geofacetElemsd%dNdXi(1:geofacetElemsd%nns, &
                                    1:geofacetElemsd%xidim, &
                                    1:geofacetElemsd%nips))

  CALL nbcField%Get(VALUE=nbcvalue, globalNode=cellcon(1:tcellcon), &
                    tsize=tnbcvalue, islocal=defaultOpt%yes)

  ! forceVar = NodalVariable(val=nbcvalue, rank=TypeFEVariableScalar, &
  !                          vartype=TypeFEVariableSpace)

  CALL Fevar_Set(obj=forceVar, val=nbcvalue(1:tnbcvalue), &
                 rank=TypeFEVariableScalar, &
                 vartype=TypeFEVariableSpace, &
                 scale=defaultOpt%one, &
                 addContribution=defaultOpt%no)

  CALL ForceVector_(test=elemsd, c=forceVar, crank=TypeFEVariableScalar, &
                    ans=forcevec, tsize=tforcevec)

  CALL obj%Set(globalNode=cellcon(1:tcellcon), VALUE=forcevec(1:tforcevec), &
               scale=scale, addContribution=defaultOpt%yes, &
               islocal=defaultOpt%yes)

END DO

IF (ALLOCATED(geoCellCon)) DEALLOCATE (geoCellCon)
IF (ALLOCATED(geoFacetCon)) DEALLOCATE (geoFacetCon)
IF (ALLOCATED(xij)) DEALLOCATE (xij)
IF (ALLOCATED(facetXij)) DEALLOCATE (facetXij)
IF (ALLOCATED(cellcon)) DEALLOCATE (cellcon)
IF (ALLOCATED(forcevec)) DEALLOCATE (forcevec)
IF (ALLOCATED(nbcvalue)) DEALLOCATE (nbcvalue)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE ScalarFieldAssembleSurfaceSource1

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

END SUBMODULE ScalarFieldMethods
