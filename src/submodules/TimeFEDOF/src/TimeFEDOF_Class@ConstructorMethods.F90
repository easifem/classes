! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

SUBMODULE(TimeFEDOF_Class) ConstructorMethods
USE GlobalData, ONLY: stdout, CHAR_LF
USE TomlUtility, ONLY: GetValue
USE tomlf, ONLY: toml_get => get_value, toml_serialize
USE Display_Method, ONLY: ToString, Display
USE ReallocateUtility, ONLY: Reallocate
USE InputUtility, ONLY: Input
USE Display_Method, ONLY: Display, ToString
USE IntVector_Method, ONLY: IntegerCopy => Copy
USE StringUtility, ONLY: UpperCase
USE ReallocateUtility, ONLY: Reallocate
USE FPL_Method, ONLY: Set, GetValue, CheckEssentialParam
USE BaseType, ONLY: TypeInterpolationOpt, TypePolynomialOpt
USE LagrangeOneDimFE_Class, ONLY: LagrangeOneDimFEPointer
USE HierarchicalOneDimFE_Class, ONLY: HierarchicalOneDimFEPointer
USE OrthogonalOneDimFE_Class, ONLY: OrthogonalOneDimFEPointer
USE OneDimFEFactoryUtility, ONLY: OneDimFEFactory

#ifdef DEBUG_VER
USE Display_Method, ONLY: Display
#endif

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
obj%isInit = .TRUE.
obj%opt => timeOpt

obj%fe => OneDimFEFactory( &
          baseContinuity=baseContinuity, &
          baseInterpolation=baseInterpolation, &
          order=order, feType=feType, ipType=ipType, &
          basisType=basisType, alpha=alpha, &
          beta=beta, lambda=lambda, &
          quadratureType=quadratureType, &
          quadratureOrder=quadratureOrder, &
          quadratureNips=quadratureNips, &
          quadratureAlpha=quadratureAlpha, &
          quadratureBeta=quadratureBeta, &
          quadratureLambda=quadratureLambda)

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

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInit = .FALSE.
obj%opt => NULL()

isok = ASSOCIATED(obj%fe)

IF (isok) CALL obj%fe%DEALLOCATE()

obj%fe => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                    Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Copy()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
obj%isInit = obj2%isInit
obj%opt => obj2%opt
obj%fe => obj2%fe

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
