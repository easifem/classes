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
!

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Constructor methods of VTKHDFFile_

SUBMODULE(VTKHDFFile_Class) ConstructorMethods
USE Display_Method, ONLY: Display
USE ExceptionHandler_Class, ONLY: e
USE TomlUtility, ONLY: GetValue
USE tomlf, ONLY: toml_get => get_value
IMPLICIT NONE

CHARACTER(*), PARAMETER :: defaultFile = "./result.vtkhdf"

CONTAINS

!----------------------------------------------------------------------------
!                                                            InitiateVTKHDF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateVTKHDF
CHARACTER(*), PARAMETER :: myName = "obj_InitiateVTKHDF()"
CHARACTER(:), ALLOCATABLE :: filename0, mode0
TYPE(String) :: vtkType
INTEGER(I4B) :: zlib0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(filename)) THEN
  filename0 = TRIM(filename)
ELSE IF (LEN_TRIM(obj%outputFile%chars()) .GT. 0) THEN
  filename0 = obj%outputFile%chars()
ELSE
  filename0 = defaultFile
END IF

mode0 = "NEW"
IF (PRESENT(mode)) mode0 = TRIM(ADJUSTL(mode))
IF (PRESENT(isTransient)) obj%isTransient = isTransient
IF (PRESENT(frequency)) obj%frequency = MAX(frequency, 1)
IF (PRESENT(zlibOpt)) obj%zlib = zlibOpt
zlib0 = obj%zlib

IF (obj%IsInitiated()) CALL obj%HDF5File_%DEALLOCATE()
CALL ResetState(obj)

SELECT CASE (mode0)
CASE ("NEW", "new", "New")
  CALL obj%Initiate(filename=filename0, mode="NEW", zlibOpt=zlib0)
  CALL obj%OPEN()
  CALL obj%setOverwriteStat(math%yes)
  CALL obj%mkdir("/VTKHDF")
  CALL obj%WriteAttribute("/VTKHDF", "Version", VTKHDF_VERSION)
  CALL obj%WriteAttribute("/VTKHDF", "Type", "UnstructuredGrid")

CASE ("READ", "read", "Read")
  CALL obj%Initiate(filename=filename0, mode="READ")
  CALL obj%OPEN()

  IF (.NOT. obj%pathExists("/VTKHDF")) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[ERROR] :: /VTKHDF group not found in '// &
                      filename0)
    RETURN
  END IF

  CALL obj%ReadAttribute("/VTKHDF", "Type", vtkType)
  ! INDEX is used because the string may be padded by other writers
  IF (INDEX(vtkType%chars(), "UnstructuredGrid") .NE. 1) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[NOT IMPLEMENTED] :: Only UnstructuredGrid is '// &
                      'supported, found '//vtkType%chars())
    RETURN
  END IF

  obj%isTransient = obj%pathExists("/VTKHDF/Steps")

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[ERROR] :: Unknown mode '//mode0// &
                    ', it should be NEW or READ.')
  RETURN
END SELECT

obj%isVTKHDFInit = math%yes

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_InitiateVTKHDF

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml()"
#endif
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

node => NULL()
CALL toml_get(table, "vtkhdf", node, origin=origin, requested=math%no, &
              stat=stat)

IF (.NOT. ASSOCIATED(node)) THEN
  obj%isActive = math%no
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] vtkhdf table not found.')
#endif
  RETURN
END IF

CALL GetValue(table=node, key="isActive", VALUE=obj%isActive, &
              default_value=math%yes, origin=origin, stat=stat)

CALL GetValue(table=node, key="filename", VALUE=obj%outputFile, &
              default_value=defaultFile, origin=origin, stat=stat)

CALL GetValue(table=node, key="isTransient", VALUE=obj%isTransient, &
              default_value=math%yes, origin=origin, stat=stat)

CALL GetValue(table=node, key="frequency", VALUE=obj%frequency, &
              default_value=math%one_i, origin=origin, stat=stat)
obj%frequency = MAX(obj%frequency, 1)

CALL GetValue(table=node, key="zlib", VALUE=obj%zlib, &
              default_value=-1_I4B, origin=origin, stat=stat)

node => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
IF (obj%IsInitiated()) CALL obj%HDF5File_%DEALLOCATE(Delete=Delete)
CALL ResetState(obj)
obj%isActive = math%yes
obj%isTransient = math%yes
obj%frequency = 1
obj%zlib = -1
obj%outputFile = ""
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
INTEGER(I4B) :: ii

CALL Display(msg, unitno=unitno)
CALL Display(obj%isActive, "isActive: ", unitno=unitno)
CALL Display(obj%isVTKHDFInit, "isVTKHDFInit: ", unitno=unitno)
CALL Display(obj%outputFile%chars(), "outputFile: ", unitno=unitno)
CALL Display(obj%isTransient, "isTransient: ", unitno=unitno)
CALL Display(obj%frequency, "frequency: ", unitno=unitno)
CALL Display(obj%zlib, "zlib: ", unitno=unitno)
CALL Display(obj%isLagrange, "isLagrange: ", unitno=unitno)
CALL Display(obj%isMeshWritten, "isMeshWritten: ", unitno=unitno)
CALL Display(obj%nPoints, "nPoints: ", unitno=unitno)
CALL Display(obj%nCells, "nCells: ", unitno=unitno)
CALL Display(obj%nIds, "nIds: ", unitno=unitno)
CALL Display(obj%stepCounter, "stepCounter: ", unitno=unitno)
CALL Display(obj%nSteps, "nSteps: ", unitno=unitno)
CALL Display(obj%nArrays, "nArrays: ", unitno=unitno)
DO ii = 1, obj%nArrays
  CALL Display(obj%arrayNames(ii)%chars(), "array: ", unitno=unitno)
END DO
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                  IsActive
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsActive
ans = obj%isActive
END PROCEDURE obj_IsActive

!----------------------------------------------------------------------------
!                                                              IsOutputStep
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsOutputStep
ans = obj%isVTKHDFInit
IF (.NOT. ans) RETURN
IF (.NOT. obj%isTransient) RETURN
ans = obj%isStepOpen .AND. (.NOT. obj%isSkipped)
END PROCEDURE obj_IsOutputStep

!----------------------------------------------------------------------------
!                                                            GetTotalPoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalPoints
ans = obj%nPoints
END PROCEDURE obj_GetTotalPoints

!----------------------------------------------------------------------------
!                                                             GetTotalCells
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalCells
ans = obj%nCells
END PROCEDURE obj_GetTotalCells

!----------------------------------------------------------------------------
!                                                                ResetState
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Reset the state of the file (options are not changed)

SUBROUTINE ResetState(obj)
  CLASS(VTKHDFFile_), INTENT(INOUT) :: obj

  obj%isVTKHDFInit = math%no
  obj%isLagrange = math%no
  obj%isMeshWritten = math%no
  obj%isStepOpen = math%no
  obj%isSkipped = math%no
  obj%stepCounter = 0
  obj%nSteps = 0
  obj%nPoints = 0
  obj%nCells = 0
  obj%nIds = 0
  obj%partOffset = 0
  obj%pointOffset = 0
  obj%cellOffset = 0
  obj%connOffset = 0
  obj%nArrays = 0
  IF (ALLOCATED(obj%arrayNames)) DEALLOCATE (obj%arrayNames)
  IF (ALLOCATED(obj%arrayLoc)) DEALLOCATE (obj%arrayLoc)
  IF (ALLOCATED(obj%arrayOffset)) DEALLOCATE (obj%arrayOffset)
  IF (ALLOCATED(obj%arraySize)) DEALLOCATE (obj%arraySize)
END SUBROUTINE ResetState

END SUBMODULE ConstructorMethods
