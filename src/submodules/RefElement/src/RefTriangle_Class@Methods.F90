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
TYPE(String) :: baseContinuity0, baseInterpolation0
CHARACTER(*), PARAMETER :: myName = "refelem_RefCoord"

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

    ans = RefCoord_Triangle("UNIT")

  CASE ("HERMITPOLYNOMIAL", "HERMIT", "HERMITINTERPOLATION")

    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & 'NOT IMPLEMETED! WIP! baseInterpolation='//baseInterpolation0)

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
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: NO CASE FOUND! for baseContinuity=' &
      & //baseContinuity0)
    RETURN
  END SELECT

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: Currently, only baseContinuity=H1 allowed!')
  RETURN
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
CHARACTER(*), PARAMETER :: myName = "refelem_GetFacetElements()"
INTEGER(I4B), PARAMETER :: tfacet = 3_I4B
INTEGER(I4B) :: ii
TYPE(string) :: baseContinuity0, baseInterpolation0
INTEGER(I4B) :: faceCon(2, 3)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

CALL obj%GetParam(baseInterpolation=baseInterpolation0, &
  & baseContinuity=baseContinuity0)

faceCon = FacetConnectivity_Triangle(baseInterpolation0%chars(), &
  & baseContinuity0%chars())

ALLOCATE (ans(tfacet))

DO ii = 1, tfacet
  ALLOCATE (RefLine_ :: ans(ii)%ptr)
  CALL ans(ii)%ptr%Initiate(nsd=obj%GetNSD(),  &
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
