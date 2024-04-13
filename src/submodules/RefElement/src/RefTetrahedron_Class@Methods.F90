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

SUBMODULE(RefTetrahedron_Class) Methods
USE BaseMethod
USE RefElementFactory
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                RefCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_RefCoord
TYPE(String) :: baseContinuity0, baseInterpolation0
CHARACTER(*), PARAMETER :: myName = "refelem_RefCoord()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

baseContinuity0 = UpperCase(baseContinuity)
baseInterpolation0 = UpperCase(baseInterpolation)

SELECT CASE (baseContinuity0%chars())
CASE ("H1")
  SELECT CASE (baseInterpolation0%chars())
  CASE (  &
    & "LAGRANGEPOLYNOMIAL", &
    & "LAGRANGE", &
    & "LAGRANGEINTERPOLATION", &
    & "SERENDIPITYPOLYNOMIAL", &
    & "SERENDIPITY", &
    & "SERENDIPITYINTERPOLATION")

    ans = RefCoord_Tetrahedron("UNIT")

  CASE ("HERMITPOLYNOMIAL", "HERMIT", "HERMITINTERPOLATION")

    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'NOT IMPLEMETED! WIP! baseInterpolation='//baseInterpolation0)

  CASE ( &
    & "HIERARCHYPOLYNOMIAL", &
    & "HEIRARCHYPOLYNOMIAL", &
    & "HIERARCHY", &
    & "HEIRARCHY", &
    & "HIERARCHYINTERPOLATION", &
    & "HEIRARCHYINTERPOLATION", &
    & "ORTHOGONALPOLYNOMIAL", &
    & "ORTHOGONAL", &
    & "ORTHOGONALINTERPOLATION")

    ans = RefCoord_Tetrahedron("BIUNIT")

  CASE DEFAULT
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'NO CASE FOUND! for baseContinuity='//baseContinuity0)
  END SELECT

CASE DEFAULT
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Currently, only baseContinuity=H1 allowed!')
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE refelem_RefCoord

!----------------------------------------------------------------------------
!                                                                 GetName
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetName
ans = Tetrahedron4
END PROCEDURE refelem_GetName

!----------------------------------------------------------------------------
!                                                           GetFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetFacetElements
CHARACTER(*), PARAMETER :: myName = "refelem_GetFacetElements()"
INTEGER(I4B), PARAMETER :: tface = 4_I4B
INTEGER(I4B) :: ii
TYPE(string) :: baseContinuity0, baseInterpolation0
INTEGER(I4B) :: faceCon(3, tface)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

CALL obj%getParam(baseInterpolation=baseInterpolation0, &
  & baseContinuity=baseContinuity0)

faceCon = FacetConnectivity_Tetrahedron( &
  & baseInterpolation0%chars(), &
  & baseContinuity0%chars())

ALLOCATE (ans(tface))
DO ii = 1, tface
  ALLOCATE (RefTriangle_ :: ans(ii)%ptr)
  CALL ans(ii)%ptr%Initiate(nsd=obj%getNSD(),  &
    & baseContinuity=baseContinuity0%chars(),  &
    & baseInterpolation=baseInterpolation0%chars())
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE refelem_GetFacetElements

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
