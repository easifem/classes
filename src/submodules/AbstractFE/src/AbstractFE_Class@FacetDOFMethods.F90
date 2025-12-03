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

SUBMODULE(AbstractFE_Class) FacetDOFMethods
USE BaseType, ONLY: TypeFEVariableOpt
USE BaseType, ONLY: math => TypeMathOpt
USE Display_Method, ONLY: ToString, Display
USE TomlUtility, ONLY: GetValue, GetValue_
USE tomlf, ONLY: toml_get => get_value
USE MassMatrix_Method, ONLY: MassMatrix_
USE ForceVector_Method, ONLY: ForceVector_
USE Lapack_Method, ONLY: GetLU, LUSolve, GetInvMat
USE InputUtility, ONLY: Input
USE Projection_Method, ONLY: GetL2ProjectionDOFValueFromQuadrature

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                 GetFacetDOFValueFromVertex
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetDOFValueFromVertex
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetFacetDOFValueFromVertex()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: ii, nns, nips

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nns = geoFacetElemsd%nns
nips = geoFacetElemsd%nips

#ifdef DEBUG_VER
isok = SIZE(func) .GE. nns
CALL AssertError1(isok, myName, &
                  "Size of func is less than nns in geoFacetElemsd")

isok = SIZE(funcValue) .GE. nips
CALL AssertError1(isok, myName, &
                  "Size of funcValue is less than nips in facetElemsd")
#endif

! Now we will perform interpolation from vertex to quadrature points
! The result will be stored in funcValue
! Here why are not we using geoFacetElemsd instead of facetElemsd?
DO ii = 1, nips
  funcValue(ii) = DOT_PRODUCT(geoFacetElemsd%N(1:nns, ii), func(1:nns))
END DO

CALL GetL2ProjectionDOFValueFromQuadrature( &
  elemsd=facetElemsd, func=funcValue, ans=ans, tsize=tsize, massMat=massMat, &
  ipiv=ipiv, skipVertices=onlyFaceBubble, tVertices=tVertices)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetFacetDOFValueFromVertex

!----------------------------------------------------------------------------
!                                                GetFacetDOFValueFromConstant
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetDOFValueFromConstant
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetFacetDOFValueFromConstant()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  'child classes')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetFacetDOFValueFromConstant

!----------------------------------------------------------------------------
!                                            GetFacetDOFValueFromUserFunction
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetDOFValueFromSTFunc
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetFacetDOFValueFromSTFunc()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  'child classes')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetFacetDOFValueFromSTFunc

!----------------------------------------------------------------------------
!                                          GetSTFacetDOFValueFromUserFunction
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSTFacetDOFValueFromSTFunc
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetSTFacetDOFValueFromSTFunc()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  'child classes')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetSTFacetDOFValueFromSTFunc

!----------------------------------------------------------------------------
!                                              GetSTFacetDOFValueFromConstant
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSTFacetDOFValueFromConstant
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetSTFacetDOFValueFromConstant()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  'child classes')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetSTFacetDOFValueFromConstant

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE FacetDOFMethods
