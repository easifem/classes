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

SUBMODULE(OneDimQuadratureOpt_Class) Methods
USE GlobalData, ONLY: stdout, CHAR_LF
USE Display_Method, ONLY: Display, ToString
USE QuadraturePoint_Method, ONLY: QuadraturePoint_ToChar, &
                                  QuadraturePoint_ToInteger
USE LineInterpolationUtility, ONLY: QuadratureNumber_Line
USE InputUtility, ONLY: Input
USE tomlf, ONLY: toml_get => get_value, &
                 toml_serialize
USE TomlUtility, ONLY: GetValue, GetValue_

IMPLICIT NONE

CONTAINS

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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Copy

!----------------------------------------------------------------------------
!                                                                     Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitno=unitno)
CALL Display(obj%quadratureType, "quadratureType: ", unitNo)
CALL Display(TRIM(obj%quadratureType_char), "quadratureType_char: ", unitNo)
CALL Display(obj%alpha, "alpha: ", unitNo)
CALL Display(obj%beta, "beta: ", unitNo)
CALL Display(obj%lambda, "lambda: ", unitNo)
CALL Display(obj%order, "order: ", unitNo)
CALL Display(obj%nips(1), "nips(1): ", unitNo)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

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
IF (PRESENT(alpha)) obj%alpha = alpha

! Set beta parameter for Jacobi polynomials if present
IF (PRESENT(beta)) obj%beta = beta

! Set lambda parameter for Ultraspherical polynomials if present
IF (PRESENT(lambda)) obj%lambda = lambda

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetParam

!----------------------------------------------------------------------------
!                                                                    Initiate
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

CALL obj%SetParam( &
  quadratureType=quadratureType, order=order, nips=nips, alpha=alpha, &
  beta=beta, lambda=lambda)

! Set the quadrature type character
obj%quadratureType_char = QuadraturePoint_ToChar( &
                          obj%quadratureType, isUpper=.TRUE.)

! Set isOrder and isNips flags
obj%isOrder = (PRESENT(order))
obj%isNips = (PRESENT(nips))

IF (PRESENT(isOrder)) obj%isOrder = isOrder
IF (PRESENT(isNips)) obj%isNips = isNips

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                                   GetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetParam()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(quadratureType)) quadratureType = obj%quadratureType
IF (PRESENT(order)) order = obj%order
IF (PRESENT(nips)) nips = obj%nips(1)
IF (PRESENT(alpha)) alpha = obj%alpha
IF (PRESENT(beta)) beta = obj%beta
IF (PRESENT(lambda)) lambda = obj%lambda

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetParam

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
INTEGER(I4B) :: origin, stat, order, nips, tsize, quadratureType
LOGICAL(LGT) :: isFound, isOrder, isNips
TYPE(String) :: quadratureType_char
REAL(DFP) :: alpha, beta, lambda

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'Reading quadratureType...')
#endif

CALL GetValue(table=table, key="quadratureType", &
              VALUE=quadratureType_char, &
              default_value=TypeOneDimQuadratureOpt%quadratureType_char, &
              origin=origin, stat=stat, isFound=isFound)

quadratureType = QuadraturePoint_ToInteger(quadratureType_char%chars())

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'Reading order...')
#endif

CALL GetValue( &
  table=table, key="order", VALUE=order, &
  default_value=TypeOneDimQuadratureOpt%order, &
  origin=origin, stat=stat, isFound=isOrder)

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'Reading nips...')
#endif

CALL GetValue(table=table, key="nips", VALUE=nips, &
              default_value=TypeOneDimQuadratureOpt%nips(1), &
              origin=origin, stat=stat, isFound=isNips)

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'Reading alpha...')
#endif

CALL GetValue(table=table, key="alpha", &
              VALUE=alpha, &
              default_value=TypeOneDimQuadratureOpt%alpha, &
              origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'Reading beta...')
#endif

CALL GetValue(table=table, key="beta", &
              VALUE=beta, &
              default_value=TypeOneDimQuadratureOpt%beta, &
              origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'Reading lambda...')
#endif

CALL GetValue(table=table, key="lambda", &
              VALUE=lambda, &
              default_value=TypeOneDimQuadratureOpt%lambda, &
              origin=origin, stat=stat, isFound=isFound)

! Here we call initiate methods with above parameters

CALL obj%Initiate(quadratureType=quadratureType, &
                  order=order, nips=nips, alpha=alpha, &
                  beta=beta, lambda=lambda, isOrder=isOrder, &
                  isNips=isNips)

! clean up
quadratureType_char = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL GetValue(table=table, afile=afile, filename=filename)

node => NULL()
CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE., &
              stat=stat)

#ifdef DEBUG_VER
isok = ASSOCIATED(node)
CALL AssertError1(isok, myName, &
                  'following error occured while reading '// &
             'the toml file :: cannot find ['//tomlName//"] table in config.")
#endif

CALL obj%ImportFromToml(table=node)

#ifdef DEBUG_VER
IF (PRESENT(printToml)) THEN
  CALL Display(toml_serialize(node), "toml config = "//CHAR_LF, &
               unitNo=stdout)
END IF
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!                                                                  Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
CALL obj%Copy(TypeOneDimQuadratureOpt)
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                    GetTotalQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalQuadraturePoints()"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = obj%isNips .OR. obj%isOrder
CALL AssertError1(isok, myName, &
                  'Either nips or order must be specified before '// &
                  'getting total quadrature points.')
#endif

#ifdef DEBUG_VER
isok = .NOT. (obj%isNips .AND. obj%isOrder)
CALL AssertError1(isok, myName, &
                  'Both nips or order cannot be specified')
#endif

IF (obj%isNips) THEN
  ans = obj%nips(1)
ELSE
  ans = QuadratureNumber_Line(order=obj%order, quadType=obj%quadratureType)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalQuadraturePoints

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
obj%quadratureType_char = QuadraturePoint_ToChar(quadratureType, &
                                                 isUpper=.TRUE.)

IF (PRESENT(alpha)) obj%alpha = alpha
IF (PRESENT(beta)) obj%beta = beta
IF (PRESENT(lambda)) obj%lambda = lambda

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetQuadratureType

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods

