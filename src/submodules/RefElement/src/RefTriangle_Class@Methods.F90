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

SUBMODULE(RefTriangle_Class) Methods
USE BaseMethod
USE RefElementFactory
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                RefCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_RefCoord
TYPE(String) :: baseContinuity0, baseInterpol0
CHARACTER(*), PARAMETER :: myName = "refelem_RefCoord"

baseContinuity0 = UpperCase(baseContinuity)
baseInterpol0 = UpperCase(baseInterpol)

SELECT CASE (baseContinuity0%chars())
CASE ("H1")
  SELECT CASE (baseInterpol0%chars())
  CASE (  &
    & "LAGRANGEPOLYNOMIAL", &
    & "LAGRANGE", &
    & "LAGRANGEINTERPOLATION", &
    & "SERENDIPITYPOLYNOMIAL", &
    & "SERENDIPITY", &
    & "SERENDIPITYINTERPOLATION")

    ans = RefCoord_Triangle("UNIT")

  CASE ("HERMITPOLYNOMIAL", "HERMIT", "HERMITINTERPOLATION")

    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'NOT IMPLEMETED! WIP! baseInterpol='//baseInterpol0)

  CASE ( &
    & "HIERARCHYPOLYNOMIAL", &
    & "HIERARCHY", &
    & "HEIRARCHYPOLYNOMIAL", &
    & "HEIRARCHY", &
    & "HIERARCHYINTERPOLATION", &
    & "HEIRARCHYINTERPOLATION", &
    & "ORTHOGONALPOLYNOMIAL", &
    & "ORTHOGONAL", &
    & "ORTHOGONALINTERPOLATION")

    ans = RefCoord_Triangle("BIUNIT")

  CASE DEFAULT
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'NO CASE FOUND! for baseContinuity='//baseContinuity0)
  END SELECT

CASE DEFAULT
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Currently, only baseContinuity=H1 allowed!')
END SELECT

END PROCEDURE refelem_RefCoord

!----------------------------------------------------------------------------
!                                                                    GetName
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetName
ans = Triangle3
END PROCEDURE refelem_GetName

!----------------------------------------------------------------------------
!                                                           GetFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetFacetElements
INTEGER(I4B), PARAMETER :: tfacet = 3_I4B
INTEGER(I4B) :: ii
TYPE(string) :: baseContinuity0, baseInterpol0
REAL(DFP), ALLOCATABLE :: xij(:, :)
INTEGER(I4B) :: faceCon(2, 3)

CALL obj%getParam( &
  & baseInterpol=baseInterpol0, &
  & baseContinuity=baseContinuity0, &
  & xij=xij)

faceCon = FacetConnectivity_Triangle( &
  & baseInterpol0%chars(), &
  & baseContinuity0%chars())

ALLOCATE (ans(tfacet))

DO ii = 1, tfacet
  ALLOCATE (RefLine_ :: ans(ii)%ptr)
  CALL ans(ii)%ptr%Initiate( &
    & nsd=obj%getNSD(),  &
    & baseContinuity=baseContinuity0%chars(),  &
    & baseInterpol=baseInterpol0%chars(), &
    & xij=xij(:, faceCon(:, ii)) &
    & )
END DO

IF (ALLOCATED(xij)) DEALLOCATE (xij)
END PROCEDURE refelem_GetFacetElements

!----------------------------------------------------------------------------
!                                                           GenerateTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GenerateTopology
INTEGER(I4B), PARAMETER :: np = 3_I4B
INTEGER(I4B), PARAMETER :: ne = 3_I4B
INTEGER(I4B), PARAMETER :: nf = 1_I4B
INTEGER(I4B) :: edges(2, ne)
INTEGER(I4B) :: ii
TYPE(string) :: baseContinuity0, baseInterpol0

CALL obj%getParam( &
  & baseInterpol=baseInterpol0, &
  & baseContinuity=baseContinuity0)

edges = FacetConnectivity_Triangle( &
  & baseInterpol0%chars(), &
  & baseContinuity0%chars())

ALLOCATE (obj%pointTopology(np))
ALLOCATE (obj%edgeTopology(ne))
ALLOCATE (obj%faceTopology(nf))

!! point
DO ii = 1, np
  CALL obj%pointTopology(ii)%Initiate( &
    & nptrs=[ii], &
    & name=Point, &
    & xidimension=0_I4B)
END DO

!! edge
DO ii = 1, ne
  CALL obj%edgeTopology(ii)%Initiate( &
    & nptrs=edges(:, ii), &
    & name=Line2, &
    & xidimension=1_I4B)
END DO

!! face
CALL obj%faceTopology(1)%Initiate( &
  & nptrs=[1_I4B, 2_I4B, 3_I4B], &
  & name=Triangle3, &
  & xidimension=2_I4B)

END PROCEDURE refelem_GenerateTopology

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
