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
!

MODULE OneDimQuadratureOpt_Class
USE GlobalData, ONLY: I4B, DFP, LGT, stdout, CHAR_LF
USE String_Class, ONLY: String
USE BaseType, ONLY: ipopt => TypeInterpolationOpt
USE ExceptionHandler_Class, ONLY: e
USE Display_Method, ONLY: Display, ToString
USE FPL, ONLY: ParameterList_
USE FPL_Method, ONLY: Set, GetValue
USE QuadraturePoint_Method, ONLY: QuadraturePoint_ToChar, &
                                  QuadraturePoint_ToInteger

USE InputUtility, ONLY: Input
USE TxtFile_Class, ONLY: TxtFile_
USE tomlf, ONLY: toml_table, &
                 toml_get => get_value, &
                 toml_serialize

USE TomlUtility, ONLY: GetValue, GetValue_

IMPLICIT NONE

PRIVATE

PUBLIC :: OneDimQuadratureOpt_
PUBLIC :: TypeOneDimQuadratureOpt
PUBLIC :: SetOneDimQuadratureOptParam

CHARACTER(*), PARAMETER :: modName = "OneDimQuadratureOpt_Class"

!----------------------------------------------------------------------------
!                                                       OneDimQuadratureOpt_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-18
! summary: Quadrature options in 1D

TYPE :: OneDimQuadratureOpt_
  PRIVATE
  INTEGER(I4B) :: quadratureType = ipopt%GaussLegendre
  !! quadrature type

  REAL(DFP) :: alpha = 0.0_DFP
  !! alpha parameter for Jacobi polynomials

  REAL(DFP) :: beta = 0.0_DFP
  !! beta parameter for Jacobi polynomials

  REAL(DFP) :: lambda = 0.5_DFP
  !! lambda parameter for Ultraspherical polynomials

  INTEGER(I4B) :: order = 0_I4B
  !! order of accuracy of the quadrature

  INTEGER(I4B) :: nips(1) = 0_I4B
  !! number of integration points

  CHARACTER(128) :: quadratureType_char = "GAUSSLEGENDRE"
  !! quadrature type

  LOGICAL(LGT) :: isOrder = .FALSE.
  !! is order specified?

  LOGICAL(LGT) :: isNips = .FALSE.
  !! is number of integration points specified?

CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: Copy => obj_Copy
  !! Copy the options from another object

  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content of the object

  PROCEDURE, PUBLIC, PASS(obj) :: SetParam => obj_SetParam
  !! Set the parameters

  PROCEDURE, PUBLIC, PASS(obj) :: GetParam => obj_GetParam
  !! Get the parameters

  PROCEDURE, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Intiate the object with parameterList

  PROCEDURE, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Intiate by using parameters directly

  GENERIC, PUBLIC :: Initiate => Initiate1, Initiate2
  !! Generic method for initiating the object

  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! Import from toml table
  PROCEDURE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  !! Import from toml file
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, &
    ImportFromToml2

  PROCEDURE, PUBLIC :: DEALLOCATE => obj_Deallocate
  !! Deallocate the object

END TYPE OneDimQuadratureOpt_

!----------------------------------------------------------------------------
!                                                    TypeOneDimQuadratureOpt
!----------------------------------------------------------------------------

TYPE(OneDimQuadratureOpt_) :: TypeOneDimQuadratureOpt = &
                              OneDimQuadratureOpt_()

!----------------------------------------------------------------------------
CONTAINS
!----------------------------------------------------------------------------

!----------------------------------------------------------------------------
!                                                                       Copy
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-27
! summary: Copy the content from obj2 to obj

SUBROUTINE obj_Copy(obj, obj2)
  CLASS(OneDimQuadratureOpt_), INTENT(INOUT) :: obj
  CLASS(OneDimQuadratureOpt_), INTENT(IN) :: obj2

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

END SUBROUTINE obj_Copy

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-27
! summary: Display the content of the object

SUBROUTINE obj_Display(obj, msg, unitNo)
  CLASS(OneDimQuadratureOpt_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN), OPTIONAL :: msg
  !! Message to display
  INTEGER(I4B), INTENT(IN), OPTIONAL :: unitNo
  !! Unit number for writing

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

END SUBROUTINE obj_Display

!----------------------------------------------------------------------------
!                                                SetOneDimQuadratureOptParam
!----------------------------------------------------------------------------

SUBROUTINE SetOneDimQuadratureOptParam(param, prefix, quadratureType, &
                                       order, nips, alpha, beta, lambda)

  TYPE(ParameterList_), INTENT(INOUT) :: param
  CHARACTER(*), INTENT(IN), OPTIONAL :: prefix
  INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureType
  INTEGER(I4B), INTENT(IN), OPTIONAL :: order
  INTEGER(I4B), INTENT(IN), OPTIONAL :: nips(1)
  REAL(DFP), INTENT(IN), OPTIONAL :: alpha
  REAL(DFP), INTENT(IN), OPTIONAL :: beta
  REAL(DFP), INTENT(IN), OPTIONAL :: lambda

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "SetOneDimQuadratureOptParam()"
#endif
  INTEGER(I4B) :: aint, bint(1)
  REAL(DFP) :: areal
  LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  ! setting quadratureType
  aint = Input(default=TypeOneDimQuadratureOpt%quadratureType, &
               option=quadratureType)
  CALL Set(obj=param, prefix=prefix, key="quadratureType", &
           datatype=aint, VALUE=aint)

  ! setting order
  abool = PRESENT(order)
  CALL Set(obj=param, prefix=prefix, key="isOrder", &
           datatype=abool, VALUE=abool)
  aint = Input(default=TypeOneDimQuadratureOpt%order, &
               option=order)
  CALL Set(obj=param, prefix=prefix, key="order", &
           datatype=aint, VALUE=aint)

  ! setting nips
  abool = PRESENT(order)
  CALL Set(obj=param, prefix=prefix, key="isNips", &
           datatype=abool, VALUE=abool)

  bint = Input(default=TypeOneDimQuadratureOpt%nips, &
               option=nips)
  CALL Set(obj=param, prefix=prefix, key="nips", &
           datatype=bint(1), VALUE=bint(1))

  ! setting alpha
  areal = Input(default=TypeOneDimQuadratureOpt%alpha, &
                option=alpha)
  CALL Set(obj=param, prefix=prefix, key="alpha", &
           datatype=areal, VALUE=areal)

  ! setting beta
  areal = Input(default=TypeOneDimQuadratureOpt%beta, &
                option=beta)
  CALL Set(obj=param, prefix=prefix, key="beta", &
           datatype=areal, VALUE=areal)

  ! setting lambda
  areal = Input(default=TypeOneDimQuadratureOpt%lambda, &
                option=lambda)
  CALL Set(obj=param, prefix=prefix, key="lambda", &
           datatype=areal, VALUE=areal)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE SetOneDimQuadratureOptParam

!----------------------------------------------------------------------------
!                                                                SetParam
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-07-05
! summary: Sets the parameters for 1D quadrature options

SUBROUTINE obj_SetParam(obj, quadratureType, order, nips, alpha, beta, lambda)
  CLASS(OneDimQuadratureOpt_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureType
  INTEGER(I4B), INTENT(IN), OPTIONAL :: order
  INTEGER(I4B), INTENT(IN), OPTIONAL :: nips(1)
  REAL(DFP), INTENT(IN), OPTIONAL :: alpha
  REAL(DFP), INTENT(IN), OPTIONAL :: beta
  REAL(DFP), INTENT(IN), OPTIONAL :: lambda

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_SetParam()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  ! Set quadratureType if present
  IF (PRESENT(quadratureType)) THEN
    obj%quadratureType = quadratureType
    obj%quadratureType_char = QuadraturePoint_ToChar(obj%quadratureType, &
                                                     isUpper=.TRUE.)
  END IF

  ! Set order if present
  IF (PRESENT(order)) THEN
    obj%order = order
    obj%isOrder = .TRUE.
  END IF

  ! Set number of integration points if present
  IF (PRESENT(nips)) THEN
    obj%nips = nips
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

END SUBROUTINE obj_SetParam

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

SUBROUTINE obj_Initiate1(obj, param, prefix)
  CLASS(OneDimQuadratureOpt_), INTENT(INOUT) :: obj
  TYPE(ParameterList_), INTENT(IN) :: param
  CHARACTER(*), INTENT(IN) :: prefix

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(obj=param, prefix=prefix, key="quadratureType", &
                VALUE=obj%quadratureType)

  CALL GetValue(obj=param, prefix=prefix, key="order", &
                VALUE=obj%order)

  CALL GetValue(obj=param, prefix=prefix, key="nips", &
                VALUE=obj%nips(1))

  CALL GetValue(obj=param, prefix=prefix, key="alpha", &
                VALUE=obj%alpha)

  CALL GetValue(obj=param, prefix=prefix, key="beta", &
                VALUE=obj%beta)

  CALL GetValue(obj=param, prefix=prefix, key="lambda", &
                VALUE=obj%lambda)

  CALL GetValue(obj=param, prefix=prefix, key="isNips", &
                VALUE=obj%isNips)

  CALL GetValue(obj=param, prefix=prefix, key="isOrder", &
                VALUE=obj%isOrder)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary:  Initiate by parameters

SUBROUTINE obj_Initiate2(obj, quadratureType, order, nips, alpha, &
                         beta, lambda, isOrder, isNips)
  CLASS(OneDimQuadratureOpt_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureType
  INTEGER(I4B), INTENT(IN), OPTIONAL :: order
  INTEGER(I4B), INTENT(IN), OPTIONAL :: nips(1)
  REAL(DFP), INTENT(IN), OPTIONAL :: alpha
  REAL(DFP), INTENT(IN), OPTIONAL :: beta
  REAL(DFP), INTENT(IN), OPTIONAL :: lambda
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isOrder
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isNips

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL obj%DEALLOCATE()

  CALL obj%SetParam(quadratureType=quadratureType, &
                    order=order, nips=nips, alpha=alpha, &
                    beta=beta, lambda=lambda)

  ! Set the quadrature type character
  obj%quadratureType_char = QuadraturePoint_ToChar(obj%quadratureType, &
                                                   isUpper=.TRUE.)

  ! Set isOrder and isNips flags
  obj%isOrder = (PRESENT(order))
  obj%isNips = (PRESENT(nips))

  IF (PRESENT(isOrder)) obj%isOrder = isOrder
  IF (PRESENT(isNips)) obj%isNips = isNips

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE obj_Initiate2

!----------------------------------------------------------------------------
!                                                              GetParam
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-19
! summary: Get the parameters of the 1D quadrature

SUBROUTINE obj_GetParam(obj, quadratureType, order, nips, alpha, beta, lambda)
  CLASS(OneDimQuadratureOpt_), INTENT(in) :: obj
  INTEGER(i4b), INTENT(out), OPTIONAL :: quadratureType
  INTEGER(i4b), INTENT(out), OPTIONAL :: order
  INTEGER(i4b), INTENT(out), OPTIONAL :: nips(1)
  REAL(DFP), INTENT(out), OPTIONAL :: alpha
  REAL(DFP), INTENT(out), OPTIONAL :: beta
  REAL(DFP), INTENT(out), OPTIONAL :: lambda

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetParam()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  IF (PRESENT(quadratureType)) quadratureType = obj%quadratureType
  IF (PRESENT(order)) order = obj%order
  IF (PRESENT(nips)) nips(1) = obj%nips(1)
  IF (PRESENT(alpha)) alpha = obj%alpha
  IF (PRESENT(beta)) beta = obj%beta
  IF (PRESENT(lambda)) lambda = obj%lambda

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE obj_GetParam

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary: Import data from toml table
!
!# Introduction
! The toml table should have following contents:
!
!```toml
!```

SUBROUTINE obj_ImportFromToml1(obj, table)
  CLASS(OneDimQuadratureOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! Internal variables
  CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
  INTEGER(I4B) :: origin, stat, order, nips(1), tsize, quadratureType
  LOGICAL(LGT) :: isFound, isOrder, isNips
  TYPE(String) :: quadratureType_char
  REAL(DFP) :: alpha, beta, lambda

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading quadratureType...')
#endif

  CALL GetValue(table=table, key="quadratureType", &
                VALUE=quadratureType_char, &
                default_value=TypeOneDimQuadratureOpt%quadratureType_char, &
                origin=origin, stat=stat, isFound=isFound)
  quadratureType = QuadraturePoint_ToInteger(quadratureType_char%chars())

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading order...')
#endif

  CALL GetValue(table=table, key="order", &
                VALUE=order, &
                default_value=TypeOneDimQuadratureOpt%order, &
                origin=origin, stat=stat, isFound=isOrder)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading nips...')
#endif

  CALL GetValue_(table=table, key="nips", &
                 VALUE=nips, tsize=tsize, &
                 origin=origin, stat=stat, isFound=isNips)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading alpha...')
#endif

  CALL GetValue(table=table, key="alpha", &
                VALUE=alpha, &
                default_value=TypeOneDimQuadratureOpt%alpha, &
                origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading beta...')
#endif

  CALL GetValue(table=table, key="beta", &
                VALUE=beta, &
                default_value=TypeOneDimQuadratureOpt%beta, &
                origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
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

END SUBROUTINE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary:  Import TimeOpt from toml file

SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, filename, printToml)
  CLASS(OneDimQuadratureOpt_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: tomlName
  TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
  CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml

  ! internal variables
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

  isok = ASSOCIATED(node)
  CALL AssertError1(isok, myName, &
                    'following error occured while reading '// &
             'the toml file :: cannot find ['//tomlName//"] table in config.")

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

END SUBROUTINE obj_ImportFromToml2

!----------------------------------------------------------------------------
!                                                               Deallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary:  Deallocate the object

SUBROUTINE obj_Deallocate(obj)
  CLASS(OneDimQuadratureOpt_), INTENT(inout) :: obj
  CALL obj%Copy(TypeOneDimQuadratureOpt)
END SUBROUTINE obj_Deallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../../submodules/include/errors.F90"

END MODULE OneDimQuadratureOpt_Class
