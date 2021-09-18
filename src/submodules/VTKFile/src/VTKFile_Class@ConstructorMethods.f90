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

SUBMODULE( VTKFile_Class ) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 VTKFile
!----------------------------------------------------------------------------

MODULE PROCEDURE InitiateVTKFile
  CHARACTER( LEN = * ), PARAMETER :: myName = "InitiateVTKFile"
  !> main
  IF( obj%isInitiated ) THEN
    CALL e%raiseError(modName//'::'//myName// &
      & ' - VTKFile is already initiated, use DeallocateData() first')
  END IF
  !>
  obj%DataFormat = DataFormat
  obj%DataStructureType = DataStructureType
  obj%DataStructureName = TRIM( DataStructureName( DataStructureType ) )
  IF( PRESENT( WholeExtent ) ) THEN
    obj%WholeExtent=WholeExtent
  ELSE
    obj%WholeExtent = 0
  END IF
  IF( PRESENT( isVolatile ) ) THEN
    obj%isVolatile=isVolatile
  ELSE
    obj%isVolatile=.FALSE.
  END IF
  !>
  SELECT CASE( DataStructureType )
  !> Structured case
  CASE( VTK_ImageData, VTK_RectilinearGrid, VTK_StructuredGrid, &
    & PARALLEL_VTK_ImageData, PARALLEL_VTK_RectilinearGrid, &
    & PARALLEL_VTK_StructuredGrid )
    obj%isStructured = .TRUE.
    IF( .NOT. PRESENT( WholeExtent ) ) &
      &  CALL e%raiseError(modName//'::'//myName// &
      & ' - In case of structured data set WholeExtent should be given')
  !> Unstructured case
  CASE( VTK_PolyData, VTK_UnstructuredGrid, PARALLEL_VTK_PolyData, &
    & PARALLEL_VTK_UnstructuredGrid )
    obj%isStructured = .FALSE.
  !> Default case prints error
  CASE DEFAULT
    CALL e%raiseError(modName//'::'//myName// &
      & ' - Cannot recognize DataStructureType')
  END SELECT
  !> Handle appended case
  IF( DataFormat .EQ. VTK_APPENDED ) THEN
    obj%encoding4Appended='raw'
    obj%offset = 0
  ELSE IF ( DataFormat .EQ. VTK_BINARY_APPENDED ) THEN
    obj%encoding4Appended='base64'
    obj%DataFormat = VTK_APPENDED
    obj%offset = 0
  END IF
  !>
  IF( obj%isVolatile ) THEN
    obj%volatileBuffer = ''
  ELSE
    CALL obj%initiate( filename=filename, mode=mode )
    CALL obj%Open()
  END IF
  CALL obj%WriteRootTag()
  CALL obj%WriteDataStructureTag(meshDataFormat = meshDataFormat)
  CALL obj%OpenScratchFile()
END PROCEDURE InitiateVTKFile

!----------------------------------------------------------------------------
!                                                              AddSurrogate
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_AddSurrogate
  CALL e%addSurrogate( UserObj )
END PROCEDURE VTKFile_AddSurrogate

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_DeallocateData
  CALL xmlFile_DeallocateData( obj, Delete )
  obj%isStructured=.FALSE.
  obj%DataStructureType = 0
  obj%DataStructureName = ''
  obj%DataFormat = 0
  obj%WholeExtent = 0
  obj%indent = 0
  obj%offset = 0
  obj%encoding4Appended=""
  obj%scratch=0
  obj%offset=0
  obj%isVolatile = .FALSE.
  obj%VolatileBuffer = ''
END PROCEDURE VTKFile_DeallocateData

!----------------------------------------------------------------------------
!                                                                      Final
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_Final
  CALL obj%DeallocateData()
END PROCEDURE VTKFile_Final

!----------------------------------------------------------------------------
!                                                                      Close
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_Close
  IF( obj%isOpen( ) ) THEN
    CALL obj%WriteEndTag( name=String(obj%DataStructureName) )
    CALL obj%WriteDataArray()
    CALL obj%WriteEndTag( name=String( 'VTKFile' ) )
    IF( .NOT. obj%isVolatile ) CALL xmlFile_Close( obj )
    CALL obj%closeScratchFile()
  END IF
END PROCEDURE VTKFile_Close

!----------------------------------------------------------------------------
!                                                               UpdateOffset
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_UpdateOffset
  IF( obj%DataFormat .EQ. VTK_APPENDED ) THEN
    IF( obj%encoding4Appended .EQ. 'raw') THEN
      obj%offset = obj%offset + BYInt32 + nByte
    ELSE
      obj%offset = obj%offset + ((nByte + BYInt32 + 2_I4B)/3_I4B)*4_I4B
    END IF
  END IF
END PROCEDURE VTKFile_UpdateOffset

!----------------------------------------------------------------------------
!                                                          OpenScratchFile
!----------------------------------------------------------------------------

MODULE PROCEDURE  VTKFile_OpenScratchFile
  CHARACTER( LEN = * ), PARAMETER :: myName="VTKFile_OpenScratchFile"
  INTEGER( I4B ) :: iostat
  IF( obj%DataFormat .EQ. VTK_APPENDED ) THEN
    ! obj%scratch = getUnitNo()
    OPEN(newunit=obj%scratch, &
      & form='UNFORMATTED',   &
      & access='STREAM',      &
      & action='READWRITE',   &
      & status='SCRATCH', &
      & iostat=iostat )
    IF( iostat .NE. 0 ) &
      & CALL e%raiseError(modName//'::'//myName// &
      & ' - Some error has occured while opening scratch file')
  END IF
END PROCEDURE VTKFile_OpenScratchFile

!----------------------------------------------------------------------------
!                                                          CloseScratchFile
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_CloseScratchFile
  INTEGER( I4B ) :: iostat
  CHARACTER( LEN = * ), PARAMETER :: myName="VTKFile_CloseScratchFile"
  IF( obj%DataFormat .EQ. VTK_APPENDED ) THEN
    CLOSE(unit=obj%scratch, iostat=iostat )
    IF( iostat .NE. 0 ) &
      & CALL e%raiseError(modName//'::'//myName// &
      & ' - Some error has occured while closing scratch file')
  END IF
END PROCEDURE VTKFile_CloseScratchFile

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods