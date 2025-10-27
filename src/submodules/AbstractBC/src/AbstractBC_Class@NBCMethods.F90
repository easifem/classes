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

SUBMODULE(AbstractBC_Class) NBCMethods
USE AbstractFE_Class, ONLY: AbstractFE_

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 GetNBCValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNBCValue
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNBCValue()"
#endif

LOGICAL(LGT), PARAMETER :: yes = .TRUE., no = .FALSE.
INTEGER(I4B) :: localCellNumber, localFaceNumber
CLASS(AbstractFE_), POINTER :: feptr, geofeptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%GetElemToFace(indx=indx, localCellNumber=localCellNumber, &
                       localFaceNumber=localFaceNumber)
CALL fedof%SetFE(globalElement=localCellNumber, islocal=yes)
CALL geofedof%SetFE(globalElement=localCellNumber, islocal=yes)

CALL fedof%GetConnectivity_( &
  globalElement=localCellNumber, islocal=yes, ans=cellCon, tsize=tcellCon, &
  opt="A")

CALL geofedof%GetConnectivity_( &
  globalElement=localCellNumber, islocal=yes, ans=geoCellCon, &
  tsize=tGeoCellCon, opt="A")

CALL geofedof%GetFacetConnectivity_( &
  globalElement=localCellNumber, islocal=yes, ans=geoFacetCon, &
  tsize=tgeoFacetCon, localFaceNumber=localFaceNumber)

feptr => fedof%GetFEPointer(globalElement=localCellNumber, islocal=yes)
geofeptr => geofedof%GetFEPointer(globalElement=localCellNumber, islocal=yes)

CALL feptr%GetFacetQuadraturePoints( &
  quad=quad, facetQuad=facetQuad, localFaceNumber=localFaceNumber)

CALL feptr%GetLocalFacetElemShapeData( &
  elemsd=elemsd, facetElemsd=facetElemsd, quad=quad, facetQuad=facetQuad, &
  localFaceNumber=localFaceNumber)

CALL geofeptr%GetLocalFacetElemShapeData( &
  elemsd=geoElemsd, facetElemsd=geoFacetElemsd, quad=quad, &
  facetQuad=facetQuad, localFaceNumber=localFaceNumber)

CALL mesh%GetNodeCoord(nodeCoord=xij, nrow=xij_i, ncol=xij_j, islocal=yes, &
                       globalElement=localCellNumber)

CALL feptr%GetGlobalFacetElemShapeData( &
  elemsd=elemsd, facetElemsd=facetElemsd, localFaceNumber=localFaceNumber, &
  geoElemsd=geoElemsd, geoFacetElemsd=geoFacetElemsd, xij=xij)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetNBCValue

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE NBCMethods
