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

MODULE TimeOpt_Class
USE GlobalData, ONLY: I4B, DFP, LGT, stdout, CHAR_LF
USE String_Class, ONLY: String
USE StringUtility, ONLY: Uppercase
USE Display_Method, ONLY: Display, ToString
USE TxtFile_Class, ONLY: TxtFile_
USE tomlf, ONLY: toml_table, &
                 toml_get => get_value, &
                 toml_serialize

USE TomlUtility, ONLY: GetValue

USE ExceptionHandler_Class, ONLY: e

IMPLICIT NONE

PRIVATE

PUBLIC :: TimeOpt_, TypeTimeOpt
CHARACTER(*), PARAMETER :: modName = "TimeOpt_Class"

!----------------------------------------------------------------------------
!                                                                   TimeOpt_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-11
! summary: This class contains options related to time discretization

TYPE :: TimeOpt_
  INTEGER(I4B) :: static = 0
  !! PDE defines a Static problem

  INTEGER(I4B) :: steady = 0
  !! PDE defines a Static problem

  INTEGER(I4B) :: pseudostatic = 1
  !! PDE defines a Static problem

  INTEGER(I4B) :: transient = 2
  !! PDE defines a Transient problem

  INTEGER(I4B) :: dynamic = 2
  !! PDE defines a Transient problem

  INTEGER(I4B) :: default = 2
  !! Default time dependency

  INTEGER(I4B) :: timeDependency = 2
  !! time dependency of the problem
  !! it can be set to one of the following
  !! static, steady, pseudostatic, transient, dynamic

  CHARACTER(9) :: default_char = "TRANSIENT"
  !! Default time dependency

  INTEGER(I4B) :: totalTimeSteps = 1
  !! Total number of time steps

  INTEGER(I4B) :: currentTimeStep = 1
  !! Current time step

  REAL(DFP) :: currentTime = 0.0
  !! Current time

  REAL(DFP) :: dt = 0.0
  !! Time step

  REAL(DFP) :: startTime = 0.0
  !! Start time

  REAL(DFP) :: endTime = 0.0
  !! End time

CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: ToNumber => obj_ToNumber
  !! Convert time option name to number
  PROCEDURE, PUBLIC, PASS(obj) :: ToString => obj_ToString
  !! Convert timeDependency to string
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the object information
  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! import timeOpt from toml table
  PROCEDURE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  !! import timeOpt from toml file
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, &
    ImportFromToml2

END TYPE TimeOpt_

!----------------------------------------------------------------------------
!                                                             TimeOpt
!----------------------------------------------------------------------------

TYPE(TimeOpt_), PARAMETER :: TypeTimeOpt = TimeOpt_()

!----------------------------------------------------------------------------
!                                                                    Methods
!----------------------------------------------------------------------------

CONTAINS

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary:  Convert timeDependency to string

FUNCTION obj_ToString(obj) RESULT(ans)
  CLASS(TimeOpt_), INTENT(IN) :: obj
  CHARACTER(:), ALLOCATABLE :: ans

  ! Internal variables
  CHARACTER(*), PARAMETER :: myName = "obj_ToString()"

  SELECT CASE (obj%timeDependency)
  CASE (TypeTimeOpt%static)
    ans = "STATIC"
  CASE (TypeTimeOpt%pseudostatic)
    ans = "PSEUDOSTATIC"
  CASE (TypeTimeOpt%transient)
    ans = "TRANSIENT"
  CASE default
    ans = ""
    CALL e%RaiseError(modName//'::'//myName//' - '// &
          'No case found for timeDependency = '//Tostring(obj%timeDependency))
  END SELECT

END FUNCTION obj_ToString

!----------------------------------------------------------------------------
!                                                                   ToNumber
!----------------------------------------------------------------------------

FUNCTION obj_ToNumber(obj, name) RESULT(ans)
  CLASS(TimeOpt_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: name
  INTEGER(I4B) :: ans

  ! Internal variables
  TYPE(String) :: astr

  ! main code
  astr = Uppercase(name)
  ans = TypeTimeOpt%default
  SELECT CASE (astr%chars())
  CASE ("STATIC", "STEADY")
    ans = TypeTimeOpt%steady
  CASE ("TRANSIENT", "DYNAMIC")
    ans = TypeTimeOpt%dynamic
  CASE ("PSEUDOSTATIC")
    ans = TypeTimeOpt%pseudostatic
  END SELECT
  astr = ""
END FUNCTION obj_ToNumber

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

SUBROUTINE obj_Display(obj, msg, unitno)
  CLASS(TimeOpt_), INTENT(in) :: obj
  CHARACTER(*), INTENT(IN) :: msg
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno

  CALL Display(msg, unitno)
  CALL Display(obj%static, 'static: ', unitno=unitno)
  CALL Display(obj%steady, 'steady: ', unitno=unitno)
  CALL Display(obj%pseudostatic, 'pseudostatic: ', unitno=unitno)
  CALL Display(obj%transient, 'transient: ', unitno=unitno)
  CALL Display(obj%dynamic, 'dynamic: ', unitno=unitno)
  CALL Display(obj%default, 'default: ', unitno=unitno)
  CALL Display(obj%default_char, 'default_char: ', unitno=unitno)
  CALL Display(obj%timeDependency, 'timeDependency: ', unitno=unitno)
  CALL Display(obj%totalTimeSteps, 'totalTimeSteps: ', unitno=unitno)
  CALL Display(obj%currentTimeStep, 'currentTimeStep: ', unitno=unitno)
  CALL Display(obj%currentTime, 'currentTime: ', unitno=unitno)
  CALL Display(obj%dt, 'dt: ', unitno=unitno)
  CALL Display(obj%startTime, 'startTime: ', unitno=unitno)
  CALL Display(obj%endTime, 'endTime: ', unitno=unitno)
END SUBROUTINE obj_Display

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary: Import TimeOpt from toml table
!
!# Introduction
! The toml table should have following contents:
!
!```toml
! [timeOpt]
! timeDependency = "Transient"
! # "Static", "Steady", "Pseudostatic", "Transient", "Dynamic"
! totalTimeStep = 1
! dt = 0.0
! startTime = 0.0
! endTime = 0.0
!```

SUBROUTINE obj_ImportFromToml1(obj, table)
  CLASS(TimeOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! Internal variables
  CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isFound
  TYPE(String) :: astr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading timeDependency...')
#endif

  CALL GetValue(table=table, key="timeDependency", &
                VALUE=astr, default_value=TypeTimeOpt%default_char, &
                origin=origin, stat=stat, isFound=isFound)

  obj%timeDependency = obj%ToNumber(astr%chars())

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading totalTimeStep...')
#endif

  CALL GetValue(table=table, key="totalTimeStep", &
                VALUE=obj%totalTimeSteps, &
                default_value=TypeTimeOpt%totalTimeSteps, &
                origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading dt...')
#endif

  CALL GetValue(table=table, key="dt", &
                VALUE=obj%dt, &
                default_value=TypeTimeOpt%dt, &
                origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading startTime...')
#endif

  CALL GetValue(table=table, key="startTime", &
                VALUE=obj%startTime, &
                default_value=TypeTimeOpt%startTime, &
                origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading endTime...')
#endif

  CALL GetValue(table=table, key="endTime", &
                VALUE=obj%endTime, &
                default_value=TypeTimeOpt%endTime, &
                origin=origin, stat=stat, isFound=isFound)

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

SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, &
                               filename, printToml)
  CLASS(TimeOpt_), INTENT(INOUT) :: obj
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
!                                                                  SetParam
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary:  Set parameters for timeOpt

SUBROUTINE obj_SetParam(obj, timeDependency, totalTimeSteps, &
                        currentTimeStep, currentTime, dt, startTime, endTime)
  CLASS(TimeOpt_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN), OPTIONAL :: timeDependency
  INTEGER(I4B), INTENT(IN), OPTIONAL :: totalTimeSteps
  INTEGER(I4B), INTENT(IN), OPTIONAL :: currentTimeStep
  REAL(DFP), INTENT(IN), OPTIONAL :: currentTime
  REAL(DFP), INTENT(IN), OPTIONAL :: dt
  REAL(DFP), INTENT(IN), OPTIONAL :: startTime
  REAL(DFP), INTENT(IN), OPTIONAL :: endTime

  ! Internal variables
  CHARACTER(*), PARAMETER :: myName = "obj_SetParam()"

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  IF (PRESENT(timeDependency)) obj%timeDependency = timeDependency
  IF (PRESENT(totalTimeSteps)) obj%totalTimeSteps = totalTimeSteps
  IF (PRESENT(currentTimeStep)) obj%currentTimeStep = currentTimeStep
  IF (PRESENT(currentTime)) obj%currentTime = currentTime
  IF (PRESENT(dt)) obj%dt = dt
  IF (PRESENT(startTime)) obj%startTime = startTime
  IF (PRESENT(endTime)) obj%endTime = endTime

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE obj_SetParam

!----------------------------------------------------------------------------
!                                                            Include errors
!----------------------------------------------------------------------------

#include "../../../submodules/include/errors.F90"

END MODULE TimeOpt_Class
