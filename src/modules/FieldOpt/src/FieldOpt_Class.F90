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

MODULE FieldOpt_Class
USE GlobalData, ONLY: I4B, Constant, Space, Time, SpaceTime, &
                      DOF_FMT, NODES_FMT, NodesToDOF, DOFToNodes
USE ExceptionHandler_Class, ONLY: e
USE StringUtility, ONLY: UpperCase
USE Display_Method, ONLY: Tostring
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt

IMPLICIT NONE

PRIVATE
PUBLIC :: FieldOpt_, TypeFieldOpt

CHARACTER(*), PARAMETER :: modName = "FieldOpt_Class"

INTEGER(I4B), PARAMETER, PUBLIC :: FIELD_TYPE_NORMAL = 100
INTEGER(I4B), PARAMETER, PUBLIC :: FIELD_TYPE_CONSTANT = Constant
INTEGER(I4B), PARAMETER, PUBLIC :: FIELD_TYPE_SPACE = Space
INTEGER(I4B), PARAMETER, PUBLIC :: FIELD_TYPE_TIME = Time
INTEGER(I4B), PARAMETER, PUBLIC :: FIELD_TYPE_SPACETIME = SpaceTime
INTEGER(I4B), PARAMETER, PUBLIC :: FIELD_TYPE_CONSTANT_SPACE = Time
INTEGER(I4B), PARAMETER, PUBLIC :: FIELD_TYPE_CONSTANT_TIME = Space

!----------------------------------------------------------------------------
!                                                                  FieldOpt_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-14
! summary: Options for field variables

TYPE :: FieldOpt_
  INTEGER(I4B) :: normal = FIELD_TYPE_NORMAL
  INTEGER(I4B) :: constantSpace = fevaropt%time
  INTEGER(I4B) :: constantTime = fevaropt%space
  INTEGER(I4B) :: constant = fevaropt%constant
  INTEGER(I4B) :: space = fevaropt%space
  INTEGER(I4B) :: time = fevaropt%time
  INTEGER(I4B) :: spaceTime = fevaropt%spaceTime
  INTEGER(I4B) :: scalar = fevaropt%scalar
  INTEGER(I4B) :: vector = fevaropt%vector
  INTEGER(I4B) :: matrix = fevaropt%matrix
  INTEGER(I4B) :: nodal = fevaropt%nodal
  INTEGER(i4b) :: quadrature = fevaropt%quadrature
  INTEGER(I4B) :: solutionDependent = fevaropt%solutionDependent
  INTEGER(I4B) :: randomSpace = fevaropt%randomSpace
  CHARACTER(6) :: normal_char = "NORMAL"
  CHARACTER(8) :: constant_char = "CONSTANT"
  CHARACTER(5) :: space_char = "SPACE"
  CHARACTER(4) :: time_char = "TIME"
  CHARACTER(9) :: spaceTime_char = "SPACETIME"
  CHARACTER(13) :: constantSpace_char = "CONSTANTSPACE"
  CHARACTER(12) :: constantTime_char = "CONSTANTTIME"
  CHARACTER(5) :: default_nodalValueType_char = "SPACE"
  INTEGER(I4B) :: storageFormatDOF = DOF_FMT
  INTEGER(I4B) :: storageFormatNodes = NODES_FMT
  INTEGER(I4B) :: conversionNodesToDOF = NodesToDOF
  INTEGER(I4B) :: conversionDOFToNodes = DOFToNodes

CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: ToString => obj_ToString
    !! Convert the number (id) to string name of field
  PROCEDURE, PUBLIC, PASS(obj) :: ToNumber => obj_ToNumber
    !! Convert the string name of field to number (id)
  PROCEDURE, PUBLIC, PASS(obj) :: RankToString => obj_RankToString
  !! Convert the rank to string name
END TYPE FieldOpt_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE(FieldOpt_), PARAMETER :: TypeFieldOpt = FieldOpt_()

CONTAINS

!----------------------------------------------------------------------------
!                                                               obj_ToString
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-14
! summary:  Convert the number (id) to string name of field

FUNCTION obj_ToString(obj, id) RESULT(RESULT)
  CLASS(FieldOpt_) :: obj
  INTEGER(I4B), INTENT(IN) :: id
  !! Id of the field type
  CHARACTER(:), ALLOCATABLE :: RESULT
  !! id will be converted to string

  ! internal variables

  CHARACTER(*), PARAMETER :: myName = "obj_ToString()"

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  SELECT CASE (id)
  CASE (FIELD_TYPE_NORMAL)
    RESULT = obj%normal_char
  CASE (FIELD_TYPE_CONSTANT)
    RESULT = obj%constant_char
  CASE (FIELD_TYPE_SPACE)
    RESULT = obj%space_char
  CASE (FIELD_TYPE_TIME)
    RESULT = obj%time_char
  CASE (FIELD_TYPE_SPACETIME)
    RESULT = obj%spaceTime_char
  CASE DEFAULT
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      'No case found for id = '//Tostring(id))
  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END FUNCTION obj_ToString

!----------------------------------------------------------------------------
!                                                               obj_ToNumber
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-14
! summary: Convert the string to the number

FUNCTION obj_ToNumber(obj, name) RESULT(RESULT)
  CLASS(FieldOpt_) :: obj
  CHARACTER(*), INTENT(IN) :: name
  !! Name of the field type
  INTEGER(I4B) :: RESULT
  !! Name will be converted to id

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "obj_ToNumber()"

  CHARACTER(:), ALLOCATABLE :: nameUpper

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  ! Convert the name to upper case
  nameUpper = UpperCase(name)

  SELECT CASE (nameUpper)

  CASE ("NORMAL")
    RESULT = obj%normal
  CASE ("CONSTANT")
    RESULT = obj%constant
  CASE ("SPACE")
    RESULT = obj%space
  CASE ("TIME")
    RESULT = obj%time
  CASE ("SPACETIME")
    RESULT = obj%spaceTime
  CASE ("CONSTANTSPACE")
    RESULT = obj%constantSpace
  CASE ("CONSTANTTIME")
    RESULT = obj%constantTime
  CASE DEFAULT
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      'No case found for name = '//nameUpper)
  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END FUNCTION obj_ToNumber

!----------------------------------------------------------------------------
!                                                           obj_RankToString
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-14
! summary:  Convert the rank to string name

FUNCTION obj_RankToString(obj, id) RESULT(RESULT)
  CLASS(FieldOpt_) :: obj
  INTEGER(I4B), INTENT(IN) :: id
  !! Id of the field type
  CHARACTER(:), ALLOCATABLE :: RESULT
  !! id will be converted to string

  ! internal variables

  CHARACTER(*), PARAMETER :: myName = "obj_RankToString()"

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  SELECT CASE (id)
  CASE (TypeFieldOpt%scalar)
    RESULT = "SCALAR"
  CASE (TypeFieldOpt%vector)
    RESULT = "VECTOR"
  CASE (TypeFieldOpt%matrix)
    RESULT = "MATRIX"
  CASE DEFAULT
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      'No case found for id = '//Tostring(id))
  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END FUNCTION obj_RankToString

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FieldOpt_Class
