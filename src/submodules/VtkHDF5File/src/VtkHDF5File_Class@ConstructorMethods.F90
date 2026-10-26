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
!                                                            InitiateVTKHDF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateVTKHDF5
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_InitiateVTKHDF5()"
#endif
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  CALL obj%DEALLOCATE()

  CALL obj%Initiate(filename=filename, mode=mode, &
                    chunkPolicy=chunkPolicy, &
                    libverLow=libverLow, &
                    compactBytes=compactBytes)

  CALL obj%OPEN()

  obj%vtkType = vtkType
  IF (PRESENT(isTransient)) obj%isTransient = isTransient
  IF (PRESENT(frequency)) obj%frequency = MAX(frequency, 1)

  ! Handle new file with write access
  IF (obj%IsNew() .OR. obj%IsWrite()) THEN
    CALL obj%SetOverwriteStat(math%yes)
    CALL obj%mkdir("/VTKHDF")
    CALL obj%WriteAttribute("/VTKHDF", "Version", _VTKHDF_VERSION_)
    CALL obj%WriteAttribute("/VTKHDF", "Type", obj%vtkType)
  END IF

  ! Handle read access
  IF (obj%IsRead()) THEN

#ifdef DEBUG_VER
    isok = obj%PathExists("/VTKHDF")
    CALL AssertError1(isok, myName, &
                      "/VTKHDF group not found in VtkHDF5File: " // &
                      obj%GetFileName())
#endif

    CALL obj%ReadAttribute("/VTKHDF", "Type", obj%vtkType)
    ! INDEX is used because the string may be padded by other writers

    ! TODO:
    ! Check supported vtkType
    ! Call CheckVtkTypeSupport(obj%vtkType)

    IF (obj%isTransient) THEN
#ifdef DEBUG_VER
      isok = obj%PathExists("/VTKHDF/Steps")
      CALL AssertError1(isok, myName, &
                        "/VTKHDF/Steps group not found in VtkHDF5File: " // &
                        obj%GetFileName())
#endif
    END IF

  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_InitiateVTKHDF5

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
  IF (obj%IsInitiated()) THEN
    CALL obj%HDF5File_%DEALLOCATE(delete=delete)
  END IF
  CALL ResetState(obj)
  obj%isRequested = math%yes
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
  CALL Display(obj%isRequested, "isRequested: ", unitno=unitno)
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
  ans = obj%isRequested
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
  CLASS(VtkHDF5File_), INTENT(INOUT) :: obj

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
  IF (ALLOCATED(obj%arrayNames)) DEALLOCATE(obj%arrayNames)
  IF (ALLOCATED(obj%arrayLoc)) DEALLOCATE(obj%arrayLoc)
  IF (ALLOCATED(obj%arrayOffset)) DEALLOCATE(obj%arrayOffset)
  IF (ALLOCATED(obj%arraySize)) DEALLOCATE(obj%arraySize)
END SUBROUTINE ResetState

!----------------------------------------------------------------------------
!                                                            Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
