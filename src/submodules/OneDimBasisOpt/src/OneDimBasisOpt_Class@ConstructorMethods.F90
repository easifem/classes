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

SUBMODULE(OneDimBasisOpt_Class) ConstructorMethods
USE String_Class, ONLY: String
USE LineInterpolationUtility, ONLY: RefElemDomain_Line
USE ReferenceLine_Method, ONLY: RefCoord_Line
USE BaseInterpolation_Method, ONLY: BaseType_ToChar
USE BaseInterpolation_Method, ONLY: InterpolationPoint_ToChar
USE FEVariable_Method, ONLY: FEVariable_ToChar
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myname = "obj_Initiate()"
#endif

TYPE(String) :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

obj%isInit = .TRUE.

CALL obj%SetParam( &
  firstCall=.TRUE., order=order, fetype=fetype, doftype=doftype, &
  transformType=transformType, ipType=ipType, basisType=basisType, &
  alpha=alpha, beta=beta, lambda=lambda, baseContinuity=baseContinuity, &
  baseInterpolation=baseInterpolation)

obj%tdof = obj%order + 1

astr = RefElemDomain_Line(baseContinuity=obj%baseContinuity, &
                          baseInterpol=obj%baseInterpolation)

obj%refelemDomain = astr%slice(1, 1)
astr = ""

obj%refelemCoord(1:1, 1:2) = RefCoord_Line(obj%refelemDomain)

CALL obj%quadOpt%Initiate( &
  quadratureType=quadratureType, order=quadratureOrder, &
  nips=quadratureNips, alpha=quadratureAlpha, beta=quadratureBeta, &
  lambda=quadratureLambda, isOrder=quadratureIsOrder, &
  isNips=quadratureIsNips, refelemCoord=obj%refelemCoord)

obj%basisType_char = BaseType_ToChar(obj%basisType, isupper=.TRUE.)
obj%ipType_char = InterpolationPoint_ToChar(obj%ipType, isupper=.TRUE.)
obj%feType_char = FEVariable_ToChar(obj%feType, isupper=.TRUE.)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%Copy(TypeOneDimBasisOpt)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                   Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Copy()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! Do not call obj%Deallocate(), here,
! because in obj%Deallocate() we are calling obj%Copy for
! resetting the object

! Copy the values from obj2 to obj
obj%firstCall = obj2%firstCall
obj%tdof = obj2%tdof
obj%order = obj2%order
obj%fetype = obj2%fetype
obj%ipType = obj2%ipType
obj%basisType = obj2%basisType
obj%alpha = obj2%alpha
obj%beta = obj2%beta
obj%lambda = obj2%lambda
obj%refelemDomain = obj2%refelemDomain
obj%baseContinuity = obj2%baseContinuity
obj%baseInterpolation = obj2%baseInterpolation
obj%refelemCoord = obj2%refelemCoord
obj%basisType_char = obj2%basisType_char
obj%ipType_char = obj2%ipType_char
obj%feType_char = obj2%feType_char

CALL obj%quadOpt%Copy(obj2%quadOpt)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Copy

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
