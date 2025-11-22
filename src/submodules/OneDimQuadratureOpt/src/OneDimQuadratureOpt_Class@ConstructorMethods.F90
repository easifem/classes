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

SUBMODULE(OneDimQuadratureOpt_Class) ConstructorMethods
USE QuadraturePoint_Method, ONLY: QuadraturePoint_ToChar
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

obj%isOrder = (PRESENT(order))
obj%isNips = (PRESENT(nips))

#ifdef DEBUG_VER
isok = obj%isOrder .OR. obj%isNips
CALL AssertError1(isok, myName, &
                  "Either 'order' or 'nips' must be provided to initiate.")

isok = .NOT. (obj%isOrder .AND. obj%isNips)
CALL AssertError1(isok, myName, &
                "'order' and 'nips' cannot be provided together to initiate.")
#endif

CALL obj%SetParam( &
  quadratureType=quadratureType, order=order, nips=nips, alpha=alpha, &
  beta=beta, lambda=lambda)

! Set the quadrature type character
obj%quadratureType_char = QuadraturePoint_ToChar( &
                          obj%quadratureType, isUpper=.TRUE.)

isok = PRESENT(isOrder)
IF (isok) obj%isOrder = isOrder

isok = PRESENT(isNips)
IF (isok) obj%isNips = isNips

isok = PRESENT(refelemCoord)
IF (isok) obj%refelemCoord(1:1, 1:2) = refelemCoord(1:1, 1:2)

isok = PRESENT(refelemDomain)
IF (isok) obj%refelemDomain(1:1) = refelemDomain(1:1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                                       Copy
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-27
! summary: Copy the content from obj2 to obj

MODULE PROCEDURE obj_Copy
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Copy()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%quadratureType = obj2%quadratureType
obj%alpha = obj2%alpha
obj%beta = obj2%beta
obj%lambda = obj2%lambda
obj%order = obj2%order
obj%isOrder = obj2%isOrder
obj%nips = obj2%nips
obj%isNips = obj2%isNips
obj%quadratureType_char = obj2%quadratureType_char
obj%refelemCoord = obj2%refelemCoord
obj%refelemDomain = obj2%refelemDomain

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Copy

!----------------------------------------------------------------------------
!                                                                  Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%Copy(TypeOneDimQuadratureOpt)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods

