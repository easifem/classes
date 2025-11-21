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

SUBMODULE(OneDimQuadratureOpt_Class) SetMethods
USE QuadraturePoint_Method, ONLY: QuadraturePoint_ToChar
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    SetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetParam()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! Set quadratureType if present
isok = PRESENT(quadratureType)
IF (isok) THEN
  obj%quadratureType = quadratureType
  obj%quadratureType_char = QuadraturePoint_ToChar( &
                            obj%quadratureType, isUpper=.TRUE.)
END IF

! Set order if present
isok = PRESENT(order)
IF (isok) THEN
  obj%order = order
  obj%isOrder = .TRUE.
END IF

! Set number of integration points if present
isok = PRESENT(nips)
IF (isok) THEN
  obj%nips(1) = nips
  obj%isNips = .TRUE.
END IF

! Set alpha parameter for Jacobi polynomials if present
isok = PRESENT(alpha)
IF (isok) obj%alpha = alpha

! Set beta parameter for Jacobi polynomials if present
isok = PRESENT(beta)
IF (isok) obj%beta = beta

! Set lambda parameter for Ultraspherical polynomials if present
isok = PRESENT(lambda)
IF (isok) obj%lambda = lambda

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetParam

!----------------------------------------------------------------------------
!                                                                   SetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetOrder()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isOrder = .TRUE.
obj%isNips = .FALSE.
obj%order = order

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetOrder

!----------------------------------------------------------------------------
!                                                           SetQuadratureType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetQuadratureType
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetQuadratureType()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%quadratureType = quadratureType
obj%quadratureType_char = QuadraturePoint_ToChar( &
                          quadratureType, isUpper=.TRUE.)

IF (PRESENT(alpha)) obj%alpha = alpha
IF (PRESENT(beta)) obj%beta = beta
IF (PRESENT(lambda)) obj%lambda = lambda

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetQuadratureType

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods

