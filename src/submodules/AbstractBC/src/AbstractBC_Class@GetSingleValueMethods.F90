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

SUBMODULE(AbstractBC_Class) GetSingleValueMethods
USE ReallocateUtility, ONLY: Reallocate
USE Display_Method, ONLY: ToString, Display
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE AbstractFE_Class, ONLY: AbstractFE_
USE BaseType, ONLY: ElemShapeData_, QuadraturePoint_

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get3()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL GetSpaceValue_uf( &
  obj=obj, indx=indx, mesh=mesh, fedof=fedof, geofedof=geofedof, &
  nodenum=nodenum, nodalvalue=nodalvalue, nrow=nrow, ncol=ncol, &
  massMat=massMat, funcValue=funcValue, ipiv=ipiv, elemsd=elemsd, &
  facetElemsd=facetElemsd, geoElemsd=geoElemsd, &
  geoFacetElemsd=geoFacetElemsd, quad=quad, facetQuad=facetQuad, &
  times=times)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Get3

!----------------------------------------------------------------------------
!                                                          GetSpaceValue_uf
!----------------------------------------------------------------------------

SUBROUTINE GetSpaceValue_uf( &
  obj, indx, mesh, fedof, geofedof, nodeNum, nodalValue, nrow, ncol, &
  massMat, funcValue, ipiv, elemsd, facetElemsd, geoElemsd, geoFacetElemsd, &
  quad, facetQuad, times)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  !! Abstract boundary condition
  INTEGER(I4B), INTENT(IN) :: indx
  !! index of boundary element
  CLASS(AbstractMesh_), INTENT(INOUT) :: mesh
  !! mesh pointer
  CLASS(FEDOF_), INTENT(INOUT) :: fedof, geofedof
  !! FEDOF for variable and geometry
  INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
  !! node number should be allocated
  !! size of nodeNum is nrow
  REAL(DFP), INTENT(INOUT) :: nodalValue(:, :)
  !! nodal values
  INTEGER(I4B) :: nrow, ncol
  !! nrow is size of nodeNum and size of number of rows in nodalvalue
  !! ncol is colsize of nodalvalue
  REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
  !! time values for time-dependent BCs
  REAL(DFP), INTENT(INOUT) :: massMat(:, :)
  !! mass Matrix
  REAL(DFP), INTENT(INOUT) :: funcValue(:)
  !! function value
  INTEGER(I4B), INTENT(INOUT) :: ipiv(:)
  !! pivot
  TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd, facetElemsd, &
                                         geoElemsd, geoFacetElemsd
  TYPE(QuadraturePoint_), INTENT(INOUT) :: quad, facetQuad

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetSpaceValue_uf()"
#endif

  LOGICAL(LGT), PARAMETER :: yes = .TRUE., no = .FALSE.
  LOGICAL(LGT) :: istimes

  INTEGER(I4B) :: localFaceNumber, localCellNumber, mysize, &
                  elemCoord_i, elemCoord_j, &
                  itime

  REAL(DFP) :: elemCoord(3, 8)

  CLASS(AbstractFE_), POINTER :: feptr, geofeptr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  istimes = PRESENT(times)
  nrow = 0; ncol = 1
  IF (istimes) ncol = SIZE(times)

  CALL obj%GetElemToFace( &
    indx=indx, localFaceNumber=localFaceNumber, &
    localCellNumber=localCellNumber)

  CALL mesh%GetNodeCoord( &
    nodeCoord=elemCoord, nrow=elemCoord_i, ncol=elemCoord_j, islocal=yes, &
    globalElement=localCellNumber)

  CALL fedof%SetFE(globalElement=localCellNumber, islocal=yes)
  feptr => fedof%GetFEPointer(globalElement=localCellNumber, islocal=yes)

  CALL geofedof%SetFE(globalElement=localCellNumber, islocal=yes)
  geofeptr => geofedof%GetFEPointer( &
              globalElement=localCellNumber, islocal=yes)

  CALL feptr%GetFacetQuadraturePoints( &
    quad=quad, facetQuad=facetQuad, localFaceNumber=localFaceNumber)

  CALL feptr%GetLocalFacetElemShapeData( &
    elemsd=elemsd, facetElemsd=facetElemsd, quad=quad, &
    facetQuad=facetQuad, localFaceNumber=localFaceNumber)

  CALL geofeptr%GetLocalFacetElemShapeData( &
    elemsd=geoElemsd, facetElemsd=geoFacetElemsd, quad=quad, &
    facetQuad=facetQuad, localFaceNumber=localFaceNumber)

  CALL feptr%GetGlobalFacetElemShapeData( &
    elemsd=elemsd, facetElemsd=facetElemsd, &
    localFaceNumber=localFaceNumber, geoElemsd=geoElemsd, &
    geoFacetElemsd=geoFacetElemsd, xij=elemCoord)

  IF (istimes) THEN

    DO itime = 1, ncol
      CALL feptr%GetFacetDOFValue( &
        elemsd=elemsd, facetElemsd=facetElemsd, xij=elemCoord, &
        times=times(itime), localFaceNumber=localFaceNumber, &
        func=obj%func, ans=nodalValue(:, itime), tsize=mysize, &
        massMat=massMat, ipiv=ipiv, funcValue=funcValue, &
        onlyFaceBubble=no)
    END DO

  ELSE

    CALL feptr%GetFacetDOFValue( &
      elemsd=elemsd, facetElemsd=facetElemsd, xij=elemCoord, times=0.0_DFP, &
      localFaceNumber=localFaceNumber, func=obj%func, ans=nodalValue(:, 1), &
      tsize=mysize, massMat=massMat, ipiv=ipiv, funcValue=funcValue, &
      onlyFaceBubble=no)

    DO itime = 2, ncol
      nodalValue(1:mysize, itime) = nodalValue(1:mysize, 1)
    END DO
  END IF

  ! Get the end points of face number
  CALL mesh%GetFacetConnectivity_( &
    globalElement=localCellNumber, iface=localFaceNumber, ans=nodeNum, &
    tsize=mysize, islocal=yes)

  nrow = nrow + mysize

  ! Get the internal face nodes
  CALL fedof%GetFaceDOF( &
    globalElement=localCellNumber, localFaceNumber=localFaceNumber, &
    ans=nodeNum(nrow + 1:), tsize=mysize, islocal=yes)

  nrow = nrow + mysize

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetSpaceValue_uf

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetSingleValueMethods
