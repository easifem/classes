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
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 VTKFile
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile1
  CHARACTER( LEN = * ), PARAMETER :: myName = "VTKFile1"
  CALL ans%initiate( filename=filename, mode=mode )
  ans%DataFormat = DataFormat
  ans%DataStructureType = DataStructureType
  ans%DataStructureName = TRIM( DataStructureName( DataStructureType ) )
  IF( PRESENT( its ) ) ans%its=its
  IF( PRESENT( iter ) ) ans%iter=iter
  IF( PRESENT( WholeExtent ) ) ans%WholeExtent=WholeExtent

  SELECT CASE( DataStructureType )
  CASE( VTK_ImageData, VTK_RectilinearGrid, VTK_StructuredGrid, &
    & PARALLEL_VTK_ImageData, PARALLEL_VTK_RectilinearGrid, &
    & PARALLEL_VTK_StructuredGrid )
    ans%isStructured = .TRUE.
    IF( .NOT. PRESENT( WholeExtent ) ) &
      &  CALL e%raiseError(modName//'::'//myName// &
      & ' - In case of structured data set WholeExtent should be given')

  CASE( VTK_PolyData, VTK_UnstructuredGrid, PARALLEL_VTK_PolyData, &
    & PARALLEL_VTK_UnstructuredGrid )
    ans%isStructured = .FALSE.

  CASE DEFAULT
    CALL e%raiseError(modName//'::'//myName// &
      & ' - Cannot recognize DataStructureType')
  END SELECT
END PROCEDURE VTKFile1

!----------------------------------------------------------------------------
!                                                          VTKFile_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_Pointer1
  CHARACTER( LEN = * ), PARAMETER :: myName= "VTKFile_Pointer1"
  ALLOCATE( ans )
  SELECT TYPE( ans ); TYPE IS ( VTKFile_ )
    ans = VTKFile1( filename, mode, DataFormat, DataStructureType, its, &
      & iter, WholeExtent )
  END SELECT
END PROCEDURE VTKFile_Pointer1

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
  obj%its = -1
  obj%iter = -1
  obj%WholeExtent = 0.0_DFP
  obj%indent = 0
  obj%encoding4Appended=""
  obj%scratch=0
  obj%offset=0
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
  CHARACTER( LEN = * ), PARAMETER :: myName="VTKFile_Close"
  CALL obj%WriteEndTag( name=String(obj%DataStructureName) )
  CALL obj%WriteDataArray()
  CALL obj%WriteEndTag( name=String( 'VTKFile' ) )
  CALL xmlFile_Close( obj )
  CALL obj%closeScratchFile()
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
  IF( obj%DataFormat .EQ. VTK_APPENDED ) THEN
    OPEN(newunit=obj%scratch, &
      & form='UNFORMATTED',   &
      & access='STREAM',      &
      & action='READWRITE',   &
      & status='SCRATCH')
  END IF
END PROCEDURE VTKFile_OpenScratchFile

!----------------------------------------------------------------------------
!                                                          CloseScratchFile
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_CloseScratchFile
  IF( obj%DataFormat .EQ. VTK_APPENDED ) THEN
    CLOSE(unit=obj%scratch)
  END IF
END PROCEDURE VTKFile_CloseScratchFile

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods