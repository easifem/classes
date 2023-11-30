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

SUBMODULE(VTKFile_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 VTKFile
!----------------------------------------------------------------------------

MODULE PROCEDURE InitiateVTKFile
CHARACTER(*), PARAMETER :: myName = "InitiateVTKFile()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] InitiateVTKFile()')
#endif

IF (obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: VTKFile is already initiated, '//  &
    & ' call Deallocate() first')
  RETURN
END IF

obj%dataFormat = Input(option=dataFormat, default=VTK_BINARY_APPENDED)
obj%dataStructureType = Input(option=dataStructureType,  &
  & default=VTK_UnstructuredGrid)

obj%dataStructureName = TRIM(dataStructureName(obj%dataStructureType))
obj%WholeExtent = 0
obj%origin = 0
obj%spacing = 1

IF (PRESENT(WholeExtent)) THEN
  obj%WholeExtent(1:SIZE(WholeExtent)) = WholeExtent(:)
END IF

IF (PRESENT(origin)) THEN
  obj%origin(1:SIZE(origin)) = origin(:)
END IF

IF (PRESENT(spacing)) THEN
  obj%SPACING(1:SIZE(spacing)) = SPACING(:)
END IF

IF (PRESENT(isVolatile)) THEN
  obj%isVolatile = isVolatile
ELSE
  obj%isVolatile = .FALSE.
END IF

SELECT CASE (obj%dataStructureType)

! Structured case
CASE (VTK_Imagedata, VTK_RectilinearGrid, VTK_StructuredGrid, &
  & PARALLEL_VTK_Imagedata, PARALLEL_VTK_RectilinearGrid, &
  & PARALLEL_VTK_StructuredGrid)
  obj%isStructured = .TRUE.

  IF (.NOT. PRESENT(WholeExtent)) &
    &  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & ' - In case of structured data set WholeExtent should be given')

  ! Unstructured case
CASE (VTK_Polydata, VTK_UnstructuredGrid, PARALLEL_VTK_Polydata, &
  & PARALLEL_VTK_UnstructuredGrid)
  obj%isStructured = .FALSE.

  ! Default case prints error
CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: Cannot recognize dataStructureType')
  RETURN
END SELECT

! Handle appended case
IF (obj%dataFormat .EQ. VTK_APPENDED) THEN
  obj%encoding4Appended = 'raw'
  obj%offset = 0
ELSE IF (obj%dataFormat .EQ. VTK_BINARY_APPENDED) THEN
  obj%encoding4Appended = 'base64'
  obj%dataFormat = VTK_APPENDED
  obj%offset = 0
END IF

IF (obj%isVolatile) THEN
  obj%volatileBuffer = ''
ELSE
  CALL obj%Initiate(filename=filename,  &
    & mode=Input(option=mode, default="NEW"))
  CALL obj%OPEN()
END IF

CALL obj%WriteRootTag()
CALL obj%WritedataStructureTag(meshdataFormat=meshdataFormat)
CALL obj%OpenScratchFile()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] InitiateVTKFile()')
#endif
END PROCEDURE InitiateVTKFile

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_Deallocate
CALL xmlFile_Deallocate(obj, delete)
obj%isStructured = .FALSE.
obj%dataStructureType = 0
obj%dataStructureName = ''
obj%dataFormat = 0
obj%WholeExtent = 0
obj%indent = 0
obj%offset = 0
obj%encoding4Appended = ""
obj%scratch = 0
obj%offset = 0
obj%isVolatile = .FALSE.
obj%VolatileBuffer = ''
obj%origin = 0
obj%spacing = 1
END PROCEDURE VTKFile_Deallocate

!----------------------------------------------------------------------------
!                                                                      Final
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_Final
CALL obj%DEALLOCATE()
END PROCEDURE VTKFile_Final

!----------------------------------------------------------------------------
!                                                                      Close
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_Close
IF (obj%isOpen()) THEN
  CALL obj%WriteEndTag(name=String(obj%dataStructureName))
  CALL obj%WritedataArray()
  CALL obj%WriteEndTag(name=String('VTKFile'))
  IF (.NOT. obj%isVolatile) CALL xmlFile_Close(obj)
  CALL obj%closeScratchFile()
END IF
END PROCEDURE VTKFile_Close

!----------------------------------------------------------------------------
!                                                               UpdateOffset
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_UpdateOffset
IF (obj%dataFormat .EQ. VTK_APPENDED) THEN
  IF (obj%encoding4Appended .EQ. 'raw') THEN
    obj%offset = obj%offset + BYInt32 + nByte
  ELSE
    obj%offset = obj%offset + ((nByte + BYInt32 + 2_I4B) / 3_I4B) * 4_I4B
  END IF
END IF
END PROCEDURE VTKFile_UpdateOffset

!----------------------------------------------------------------------------
!                                                          OpenScratchFile
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_OpenScratchFile
CHARACTER(*), PARAMETER :: myName = "VTKFile_OpenScratchFile"
INTEGER(I4B) :: iostat
!
IF (obj%dataFormat .EQ. VTK_APPENDED) THEN
  ! obj%scratch = getUnitNo()
  OPEN (newunit=obj%scratch, &
    & form='UNFORMATTED',   &
    & access='STREAM',      &
    & action='READWRITE',   &
    & status='SCRATCH', &
    & iostat=iostat)
  !
  IF (iostat .NE. 0) &
    & CALL e%RaiseError(modName//'::'//myName//" - "// &
    & ' - Some error has occured while opening scratch file')
END IF
!
END PROCEDURE VTKFile_OpenScratchFile

!----------------------------------------------------------------------------
!                                                          CloseScratchFile
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_CloseScratchFile
INTEGER(I4B) :: iostat
CHARACTER(*), PARAMETER :: myName = "VTKFile_CloseScratchFile"
!
IF (obj%dataFormat .EQ. VTK_APPENDED) THEN
  CLOSE (unit=obj%scratch, iostat=iostat)
  IF (iostat .NE. 0) &
    & CALL e%RaiseError(modName//'::'//myName//" - "// &
    & ' - Some error has occured while closing scratch file')
END IF
END PROCEDURE VTKFile_CloseScratchFile

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
