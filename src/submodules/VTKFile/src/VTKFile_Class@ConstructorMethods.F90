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
USE InputUtility, ONLY: Input
USE GlobalData, ONLY: BYInt32, endianL
USE XMLFile_Class, ONLY: xmlFile_Deallocate, xmlFile_Close

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 VTKFile
!----------------------------------------------------------------------------

MODULE PROCEDURE InitiateVTKFile
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "InitiateVTKFile()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER
isok = .NOT. obj%isInitiated
CALL AssertError1(isok, myName, &
                  'VTKFile is already initiated, call Deallocate() first')
#endif

obj%dataFormat = Input(option=dataFormat, default=VTK_BINARY_APPENDED)
obj%dataStructureType = Input(option=dataStructureType, &
                              default=VTK_UnstructuredGrid)

obj%dataStructureName = TRIM(dataStructureName(obj%dataStructureType))
obj%WholeExtent = 0
obj%origin = 0
obj%spacing = 1

isok = PRESENT(WholeExtent)
IF (isok) obj%WholeExtent(1:SIZE(WholeExtent)) = WholeExtent(:)

isok = PRESENT(origin)
IF (isok) obj%origin(1:SIZE(origin)) = origin(:)

isok = PRESENT(spacing)
IF (isok) obj%SPACING(1:SIZE(spacing)) = SPACING(:)

isok = PRESENT(isVolatile)
obj%isVolatile = .FALSE.
IF (isok) obj%isVolatile = isVolatile

SELECT CASE (obj%dataStructureType)

! Structured case
CASE (VTK_Imagedata, VTK_RectilinearGrid, VTK_StructuredGrid, &
      PARALLEL_VTK_Imagedata, PARALLEL_VTK_RectilinearGrid, &
      PARALLEL_VTK_StructuredGrid)

  obj%isStructured = .TRUE.

#ifdef DEBUG_VER
  isok = PRESENT(WholeExtent)
  CALL AssertError1(isok, myName, &
                  'In case of structured data set WholeExtent should be give')
#endif

  ! Unstructured case
CASE (VTK_Polydata, VTK_UnstructuredGrid, PARALLEL_VTK_Polydata, &
      PARALLEL_VTK_UnstructuredGrid)
  obj%isStructured = .FALSE.

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myName, &
                    "No case found for dataStructureType")
#endif

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
  CALL obj%Initiate(filename=filename, &
                    mode=Input(option=mode, default="NEW"))
  CALL obj%OPEN()
END IF

CALL obj%WriteRootTag()
CALL obj%WritedataStructureTag(meshdataFormat=meshdataFormat)
CALL obj%OpenScratchFile()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE InitiateVTKFile

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
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
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VTKFile_Close()"
#endif

LOGICAL(LGT) :: isok
TYPE(String) :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = obj%isOpen()

IF (isok) THEN
  astr = obj%dataStructureName
  CALL obj%WriteEndTag(name=astr)
  CALL obj%WritedataArray()
  astr = 'VTKFile'
  CALL obj%WriteEndTag(name=astr)
  IF (.NOT. obj%isVolatile) CALL xmlFile_Close(obj)
  CALL obj%CloseScratchFile()
  astr = ''
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKFile_Close

!----------------------------------------------------------------------------
!                                                               UpdateOffset
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_UpdateOffset
LOGICAL(LGT) :: isok

isok = obj%dataFormat .EQ. VTK_APPENDED

IF (isok) THEN

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
        form='UNFORMATTED', &
        access='STREAM', &
        action='READWRITE', &
        status='SCRATCH', &
        iostat=iostat)

  IF (iostat .NE. 0) &
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                      ' - Some error has occured while opening scratch file')
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
!                                                             Include errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
