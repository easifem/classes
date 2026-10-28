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
! summary: Constructor methods of VtkHDF5File_

SUBMODULE(VtkHDF5File_Class) ConstructorMethods
USE Display_Method, ONLY: Display
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "VtkHDF5File_Class@ConstructorMethods.F90"
#endif

#include "./include/vtkhdf5_definitions.F90"

CONTAINS

!----------------------------------------------------------------------------
!                                                           InitiateVTKHDF5
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateVTKHDF5
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_InitiateVTKHDF5()"
LOGICAL(LGT) :: isok
#endif
LOGICAL(LGT) :: isNewFile, isReadOnly

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = CheckVtkTypeSupport(vtkType)
CALL AssertError1(isok, myName, &
                  "vtkType="//vtkType//" not supported yet.")
#endif

! Clean the obj
CALL obj%DEALLOCATE()

! Initiate obj
CALL obj%Initiate(filename=filename, mode=mode, &
                  zlibOpt=zlibOpt, &
                  chunkPolicy=chunkPolicy, &
                  libverLow=libverLow, &
                  compactBytes=compactBytes)

! NOTE:
! The access mode is taken before OPEN: HDF5File_%Open clears the new
! status after creating the file, and a new file then has the same
! status as a file opened with mode OVERWRITE
isNewFile = obj%IsNew()
isReadOnly = obj%IsRead() .AND. (.NOT. obj%IsWrite())

CALL obj%OPEN()
! After calling open, IsNew will always return false.

#ifdef DEBUG_VER
CALL AssertError1(obj%IsOpen(), myName, &
                  "file "//filename//" cannot be opened.")
#endif

obj%vtkType = vtkType
IF (PRESENT(isTransient)) obj%isTransient = isTransient
IF (PRESENT(frequency)) obj%frequency = MAX(frequency, 1)

! Mode NEW sets both the read and the write status, so the mode is
! checked in this order: NEW, READ (read only), WRITE/OVERWRITE
IF (isNewFile) THEN
  ! New file: write the root group (NEW already allows overwriting)
  CALL obj%Mkdir("/VTKHDF")
  CALL obj%WriteAttribute("/VTKHDF", "Version", _VTKHDF_VERSION_)
  CALL obj%WriteAttribute("/VTKHDF", "Type", obj%vtkType)

ELSE IF (isReadOnly) THEN
  ! Read only file

#ifdef DEBUG_VER
  isok = obj%PathExists("/VTKHDF")
  CALL AssertError1(isok, myName, &
                    "/VTKHDF group not found in VtkHDF5File: "// &
                    obj%GetFileName())
#endif

  CALL obj%ReadAttribute("/VTKHDF", "Type", obj%vtkType)

  ! Check supported vtkType
#ifdef DEBUG_VER
  isok = CheckVtkTypeSupport(obj%vtkType%Chars())
  CALL AssertError1(isok, myName, &
                    "vtkType="//obj%vtkType%Chars()// &
                    " is not supported yet")
#endif

  IF (obj%isTransient) THEN
#ifdef DEBUG_VER
    isok = obj%PathExists("/VTKHDF/Steps")
    CALL AssertError1(isok, myName, &
                      "/VTKHDF/Steps group not found in VtkHDF5File: "// &
                      obj%GetFileName())
#endif
  END IF

ELSE
  ! WRITE or OVERWRITE of an existing file: the state of the steps
  ! and data arrays is not read from the file, so appending would
  ! corrupt the file
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
                    "mode "//mode//" is not supported, use NEW "// &
                    "to write or READ to read a VtkHDF5File.")
#endif

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_InitiateVTKHDF5

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%HDF5File_%DEALLOCATE(delete=delete)
CALL ResetState(obj)
obj%isTransient = math%no
obj%frequency = 1
obj%vtkType = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif
INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitno=unitno)
IF (.NOT. obj%IsInitiated()) THEN
  CALL Display("VtkHDF5File NOT Initiated", unitno=unitno)
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

CALL Display("vtkType: "//obj%vtkType, unitno=unitno)
CALL Display(obj%isRequested, "isRequested: ", unitno=unitno)
CALL Display(obj%IsInitiated(), "isInitiated: ", unitno=unitno)
CALL Display(obj%isTransient, "isTransient: ", unitno=unitno)
CALL Display(obj%frequency, "frequency: ", unitno=unitno)
CALL Display(obj%isMeshWritten, "isMeshWritten: ", unitno=unitno)
CALL Display(obj%nPoints, "nPoints: ", unitno=unitno)
CALL Display(obj%nCells, "nCells: ", unitno=unitno)
CALL Display(obj%nIds, "nIds: ", unitno=unitno)
CALL Display(obj%stepCounter, "stepCounter: ", unitno=unitno)
CALL Display(obj%nSteps, "nSteps: ", unitno=unitno)
CALL Display(obj%nArrays, "nArrays: ", unitno=unitno)
DO ii = 1, obj%nArrays
  CALL Display(obj%arrays(ii)%name%chars(), "array: ", unitno=unitno)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                  IsActive
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsActive
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_IsActive()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%isRequested

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_IsActive

!----------------------------------------------------------------------------
!                                                              IsOutputStep
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsOutputStep
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_IsOutputStep()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! Data can be written only in a file opened for writing (mode NEW)
ans = obj%IsInitiated() .AND. obj%IsWrite()

! In a transient file, only between BeginTimeStep and EndTimeStep of a step
! which is not skipped
IF (ans .AND. obj%isTransient) THEN
  ans = obj%isStepOpen .AND. (.NOT. obj%isSkipped)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_IsOutputStep

!----------------------------------------------------------------------------
!                                                            GetTotalPoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalPoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalPoints()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%nPoints

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalPoints

!----------------------------------------------------------------------------
!                                                             GetTotalCells
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalCells
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalCells()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%nCells

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalCells

!----------------------------------------------------------------------------
!                                                                ResetState
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Reset the state of the file (options are not changed)

SUBROUTINE ResetState(obj)
  CLASS(VtkHDF5File_), INTENT(INOUT) :: obj

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ResetState()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

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
  IF (ALLOCATED(obj%arrays)) DEALLOCATE (obj%arrays)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ResetState

!----------------------------------------------------------------------------
!                                                        CheckVtkTypeSupport
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Returns true if the VTK data type can be written and read
!
! The supported VTK data types are listed in the SELECT CASE below.
! Currently only "UnstructuredGrid" is supported. The comparison is
! case sensitive, as in VTK.

FUNCTION CheckVtkTypeSupport(vtkType) RESULT(ans)
  CHARACTER(*), INTENT(IN) :: vtkType
  !! name of VTK data type
  LOGICAL(LGT) :: ans
  !! true if vtkType is supported

  SELECT CASE (vtkType)
  CASE ("UnstructuredGrid")
    ans = math%yes
  CASE DEFAULT
    ans = math%no
  END SELECT
END FUNCTION CheckVtkTypeSupport

!----------------------------------------------------------------------------
!                                                            Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
