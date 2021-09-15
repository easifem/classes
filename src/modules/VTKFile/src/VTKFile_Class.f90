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

MODULE VTKFile_Class
USE GlobalData
USE BaseType
USE VTKDataArrayEncoder
USE XMLFile_Class
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
IMPLICIT NONE
!>
PUBLIC :: encodeVTKDataArray
CHARACTER( LEN = * ), PARAMETER :: modName="VTKFILE_CLASS"
TYPE( ExceptionHandler_ ) :: e
INTEGER( I4B ), PARAMETER :: MAX_LEN_DATA_STRUCTURENAME = 256

INTEGER( I4B ), PARAMETER, PUBLIC :: VTK_ImageData = 1
INTEGER( I4B ), PARAMETER, PUBLIC :: VTK_RectilinearGrid = 2
INTEGER( I4B ), PARAMETER, PUBLIC :: VTK_StructuredGrid = 3
INTEGER( I4B ), PARAMETER, PUBLIC :: VTK_PolyData = 4
INTEGER( I4B ), PARAMETER, PUBLIC :: VTK_UnstructuredGrid = 5

INTEGER( I4B ), PARAMETER, PUBLIC :: PARALLEL_VTK_ImageData = 6
INTEGER( I4B ), PARAMETER, PUBLIC :: PARALLEL_VTK_RectilinearGrid = 7
INTEGER( I4B ), PARAMETER, PUBLIC :: PARALLEL_VTK_StructuredGrid = 8
INTEGER( I4B ), PARAMETER, PUBLIC :: PARALLEL_VTK_PolyData = 9
INTEGER( I4B ), PARAMETER, PUBLIC :: PARALLEL_VTK_UnstructuredGrid = 10

CHARACTER( LEN=* ), PARAMETER, DIMENSION( 10 ) :: DataStructureName = &
  & [ &
  & "ImageData        ", &
  & "RectilinearGrid  ", &
  & "StructuredGrid   ", &
  & "PolyData         ", &
  & "UnstructuredGrid ", &
  & "PImageData       ", &
  & "PRectilinearGrid ", &
  & "PStructuredGrid  ", &
  & "PPolyData        ", &
  & "PUnstructuredGrid" &
  & ]

INTEGER( I4B ), PARAMETER, PUBLIC :: VTK_ASCII = 1
INTEGER( I4B ), PARAMETER, PUBLIC :: VTK_BINARY = 2
INTEGER( I4B ), PARAMETER, PUBLIC :: VTK_APPENDED = 3

CHARACTER( LEN = * ), PARAMETER, DIMENSION( 3 ) :: DataFormatName = &
  & [ &
  & "ascii   ", &
  & "binary  ", &
  & "appended" &
  & ]

!----------------------------------------------------------------------------
!                                                                   VTKFile_
!----------------------------------------------------------------------------

TYPE, EXTENDS( XMLFile_ ) :: VTKFile_
  PRIVATE
  LOGICAL( LGT ) :: isStructured = .FALSE.
    !! Is dataset structured
  INTEGER( I4B ) :: DataStructureType = 0
  CHARACTER( LEN = MAX_LEN_DATA_STRUCTURENAME ) :: DataStructureName
    !! ImageData, RectilinearGrid, StructuredGrid, PolyData,
    !! Unstructured Grid, Unstructured Grid
  INTEGER( I4B ) :: DataFormat = 0
    !! ASCII, BINARY, APPENDED
  INTEGER( I4B ) :: its = -1
    !!Time step
  INTEGER( I4B ) :: iter = -1
    !! Iteration number
  REAL( DFP ) :: WholeExtent( 6 ) = 0.0_DFP
    !! Whole extent
  INTEGER( I4B ) :: indent = 0
    !! Indent
  INTEGER( I4B ) :: offset = 0
    !! offset for appended mode
  !< VTK file XML writer, appended.
  TYPE( String ) :: encoding4Appended
    !! appended data encoding: "raw" or "base64".
  INTEGER( I4B ) :: scratch=0
    !! Used for scratch file
  CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS( obj ) :: addSurrogate => VTKFile_addSurrogate
  PROCEDURE, PUBLIC, PASS( obj ) :: DeallocateData => VTKFile_DeallocateData
  FINAL :: VTKFile_Final
  PROCEDURE, PUBLIC, PASS( obj ) :: WriteRootTag => VTKFile_WriteRootTag
  PROCEDURE, PUBLIC, PASS( obj ) :: WriteDataStructureTag => VTKFile_WriteDataStructureTag
  PROCEDURE, PUBLIC, PASS( obj ) :: WriteStartTag => VTKFile_WriteStartTag
  PROCEDURE, PUBLIC, PASS( obj ) :: WriteEndTag => VTKFile_WriteEndTag
  PROCEDURE, PUBLIC, PASS( obj ) :: WriteSelfClosingTag => &
    & VTKFile_WriteSelfClosingTag
  PROCEDURE, PUBLIC, PASS( obj ) :: WriteTag => VTKFile_WriteTag
  PROCEDURE, PUBLIC, PASS( obj ) :: Close => VTKFile_Close
  PROCEDURE, PUBLIC, PASS( obj ) :: WriteDataArrayTag => &
    & VTKFile_WriteDataArrayTag
  PROCEDURE, PUBLIC, PASS( obj ) :: WriteDataArrayLocationTag => &
    & VTKFile_WriteDataArrayLocationTag
  PROCEDURE, PASS( obj ) :: UpdateOffset => VTKFile_UpdateOffset
  PROCEDURE, PASS( obj ) :: OpenScratchFile => VTKFile_OpenScratchFile
  PROCEDURE, PASS( obj ) :: CloseScratchFile => VTKFile_CloseScratchFile
  GENERIC :: WriteToScratch => &
    & VTKFile_WriteToScratch1, VTKFile_WriteToScratch2, &
    & VTKFile_WriteToScratch3, VTKFile_WriteToScratch4, &
    & VTKFile_WriteToScratch5, VTKFile_WriteToScratch6, &
#ifdef USE_Int64
    & VTKFile_WriteToScratch7, &
#endif
    & VTKFile_WriteToScratch8, &
    & VTKFile_WriteToScratch9, VTKFile_WriteToScratch10, &
    & VTKFile_WriteToScratch11, VTKFile_WriteToScratch12, &
#ifdef USE_Int64
    & VTKFile_WriteToScratch13, &
#endif
    & VTKFile_WriteToScratch14, &
    & VTKFile_WriteToScratch15, VTKFile_WriteToScratch16, &
    & VTKFile_WriteToScratch17, VTKFile_WriteToScratch18, &
#ifdef USE_Int64
    & VTKFile_WriteToScratch19, &
#endif
    & VTKFile_WriteToScratch20, &
    & VTKFile_WriteToScratch21, VTKFile_WriteToScratch22

  PROCEDURE, PASS( obj ) :: &
    & VTKFile_WriteToScratch1, VTKFile_WriteToScratch2, &
    & VTKFile_WriteToScratch3, VTKFile_WriteToScratch4, &
    & VTKFile_WriteToScratch5, VTKFile_WriteToScratch6, &
#ifdef USE_Int64
    & VTKFile_WriteToScratch7, &
#endif
    & VTKFile_WriteToScratch8, &
    & VTKFile_WriteToScratch9, VTKFile_WriteToScratch10, &
    & VTKFile_WriteToScratch11, VTKFile_WriteToScratch12, &
#ifdef USE_Int64
    & VTKFile_WriteToScratch13, &
#endif
    & VTKFile_WriteToScratch14, &
    & VTKFile_WriteToScratch15, VTKFile_WriteToScratch16, &
    & VTKFile_WriteToScratch17, VTKFile_WriteToScratch18, &
#ifdef USE_Int64
    & VTKFile_WriteToScratch19, &
#endif
    & VTKFile_WriteToScratch20, &
    & VTKFile_WriteToScratch21, VTKFile_WriteToScratch22


  GENERIC, PUBLIC :: WriteDataArray => &
    & VTKFile_WriteDataArray_Appended, &
    & VTKFile_WriteDataArray_Rank1_Real32, &
    & VTKFile_WriteDataArray_Rank1_Real64, &
#ifdef USE_Int64
    & VTKFile_WriteDataArray_Rank1_Int64, &
#endif
    & VTKFile_WriteDataArray_Rank1_Int32, &
    & VTKFile_WriteDataArray_Rank1_Int16, &
    & VTKFile_WriteDataArray_Rank1_Int8

  PROCEDURE, PASS( obj ) :: &
    & VTKFile_WriteDataArray_Appended, &
    & VTKFile_WriteDataArray_Rank1_Real32, &
    & VTKFile_WriteDataArray_Rank1_Real64, &
#ifdef USE_Int64
    & VTKFile_WriteDataArray_Rank1_Int64, &
#endif
    & VTKFile_WriteDataArray_Rank1_Int32, &
    & VTKFile_WriteDataArray_Rank1_Int16, &
    & VTKFile_WriteDataArray_Rank1_Int8

  ! PROCEDURE, PUBLIC, PASS( obj ) :: WriteConnectivity
  ! PROCEDURE, PUBLIC, PASS( obj ) :: WritePiece
  ! PROCEDURE, PUBLIC, PASS( obj ) :: WriteFieldData
  ! PROCEDURE, PUBLIC, PASS( obj ) :: WriteGeo
END TYPE VTKFile_

PUBLIC :: VTKFile_

!----------------------------------------------------------------------------
!                                                           VTKFilePointer_
!----------------------------------------------------------------------------

TYPE :: VTKFilePointer_
  CLASS( VTKFile_ ), POINTER :: ptr => NULL()
END TYPE VTKFilePointer_

PUBLIC :: VTKFilePointer_

!----------------------------------------------------------------------------
!                                                VTKFile@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 Sept 2021
! summary: This function returns an instance of VTKFile

INTERFACE
MODULE FUNCTION VTKFile1(  filename, mode, DataFormat, &
  & DataStructureType, its, iter, WholeExtent ) &
  & RESULT( Ans )
  CHARACTER( LEN = * ), INTENT( IN ) :: filename
  CHARACTER( LEN = * ), INTENT( IN ) :: mode
  INTEGER( I4B ), INTENT( IN ) :: DataFormat
    !! ASCII, BINARY, APPENEDED
  INTEGER( I4B ), INTENT( IN ) :: DataStructureType
    !! VTK_IMAGEDATA, VTK_STRUCTUREDGRID, ...
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: its
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: iter
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: WholeExtent( 6 )
  TYPE( VTKFile_ ) :: ans
END FUNCTION VTKFile1
END INTERFACE

INTERFACE VTKFile
  MODULE PROCEDURE VTKFile1
END INTERFACE VTKFile

PUBLIC :: VTKFile

!----------------------------------------------------------------------------
!                                        VTKFile_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 Sept 2021
! summary: This function returns an instance of VTKFile

INTERFACE
MODULE FUNCTION VTKFile_Pointer1(  filename, mode, DataFormat, &
  & DataStructureType, its, iter, WholeExtent ) &
  & RESULT( Ans )
  CHARACTER( LEN = * ), INTENT( IN ) :: filename
  CHARACTER( LEN = * ), INTENT( IN ) :: mode
  INTEGER( I4B ), INTENT( IN ) :: DataFormat
    !! ASCII, BINARY, APPENEDED
  INTEGER( I4B ), INTENT( IN ) :: DataStructureType
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: its
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: iter
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: WholeExtent( 6 )
  CLASS( VTKFile_ ), POINTER :: ans
END FUNCTION VTKFile_Pointer1
END INTERFACE

INTERFACE VTKFile_Pointer
  MODULE PROCEDURE VTKFile_Pointer1
END INTERFACE VTKFile_Pointer

PUBLIC :: VTKFile_Pointer

!----------------------------------------------------------------------------
!                                           AddSurrogate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 Sept 2021
! summary: Add surrogate to the module exception handler

INTERFACE
MODULE SUBROUTINE VTKFile_AddSurrogate( obj, UserObj )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( ExceptionHandler_ ), INTENT( IN ) :: UserObj
END SUBROUTINE VTKFile_AddSurrogate
END INTERFACE

!----------------------------------------------------------------------------
!                                         DeallocateData@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 Sept 2021
! summary: Deallocates the content of VTKFile

INTERFACE
MODULE SUBROUTINE VTKFile_DeallocateData( obj, delete )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: delete
END SUBROUTINE VTKFile_DeallocateData
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 Sept 2021
! summary: Finalizer for VTKFile

INTERFACE
MODULE SUBROUTINE VTKFile_Final( obj )
  TYPE( VTKFile_ ), INTENT( INOUT ) :: obj
END SUBROUTINE VTKFile_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                            UpdateOffset@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL SUBROUTINE VTKFile_UpdateOffset( obj, nByte )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nByte
END SUBROUTINE VTKFile_UpdateOffset
END INTERFACE

!----------------------------------------------------------------------------
!                                                    WriteRootTag@IOMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteRootTag( obj )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
END SUBROUTINE VTKFile_WriteRootTag
END INTERFACE

!----------------------------------------------------------------------------
!                                          WriteDataStructureTag@IOMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataStructureTag( obj )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
END SUBROUTINE VTKFile_WriteDataStructureTag
END INTERFACE

!----------------------------------------------------------------------------
!                                                    WriteStartTag@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 Sept 2021
! summary: Write the start tag

INTERFACE
MODULE SUBROUTINE VTKFile_WriteStartTag( obj, name, attrNames, attrValues )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  TYPE( String ), OPTIONAL, INTENT( IN ) :: attrNames( : )
  TYPE( String ), OPTIONAL, INTENT( IN ) :: attrValues( : )
END SUBROUTINE VTKFile_WriteStartTag
END INTERFACE

!----------------------------------------------------------------------------
!                                                    WriteEndTag@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 Sept 2021
! summary: Write the End tag

INTERFACE
MODULE SUBROUTINE VTKFile_WriteEndTag( obj, name )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
END SUBROUTINE VTKFile_WriteEndTag
END INTERFACE

!----------------------------------------------------------------------------
!                                            WriteSelfClosingTag@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 Sept 2021
! summary: Write the self closing tag

INTERFACE
MODULE SUBROUTINE VTKFile_WriteSelfClosingTag( obj, name, attrNames, &
  & attrValues )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  TYPE( String ), OPTIONAL, INTENT( IN ) :: attrNames( : )
  TYPE( String ), OPTIONAL, INTENT( IN ) :: attrValues( : )
END SUBROUTINE VTKFile_WriteSelfClosingTag
END INTERFACE

!----------------------------------------------------------------------------
!                                                        WriteTag@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 Sept 2021
! summary:

INTERFACE
MODULE SUBROUTINE VTKFile_WriteTag( obj, name, attrNames, &
  & attrValues, content )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  TYPE( String ), OPTIONAL, INTENT( IN ) :: attrNames( : )
  TYPE( String ), OPTIONAL, INTENT( IN ) :: attrValues( : )
  TYPE( String ), OPTIONAL, INTENT( IN ) :: content
END SUBROUTINE VTKFile_WriteTag
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Close@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 Sept 2021
! summary: Close the file

INTERFACE
MODULE SUBROUTINE VTKFile_Close( obj )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
END SUBROUTINE VTKFile_Close
END INTERFACE

!----------------------------------------------------------------------------
!                                                WriteDataArrayTag@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 Sept 2021
! summary:

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArrayTag( obj, dataType, name, &
  & numberOfComponents, content, isTuples, isOffset )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: dataType
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( I4B ), INTENT( IN ) :: numberOfComponents
  TYPE( String ), OPTIONAL, INTENT( IN ) :: content
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isOffset
END SUBROUTINE VTKFile_WriteDataArrayTag
END INTERFACE

!----------------------------------------------------------------------------
!                                       WriteDataArrayLocationTag@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 Sept 2021
! summary:

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArrayLocationTag( obj, location, action )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: location
  TYPE( String ), INTENT( IN ) :: action
END SUBROUTINE VTKFile_WriteDataArrayLocationTag
END INTERFACE

!----------------------------------------------------------------------------
!                                           WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Rank1_Real32( obj, name, x, &
  & isTuples )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  REAL( Real32 ), INTENT( IN ) :: x( 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
END SUBROUTINE VTKFile_WriteDataArray_Rank1_Real32
END INTERFACE

!----------------------------------------------------------------------------
!                                           WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Rank1_Real64( obj, name, x, &
  & isTuples )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  REAL( Real64 ), INTENT( IN ) :: x( 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
END SUBROUTINE VTKFile_WriteDataArray_Rank1_Real64
END INTERFACE

!----------------------------------------------------------------------------
!                                           WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Rank1_Int8( obj, name, x, isTuples )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int8 ), INTENT( IN ) :: x( 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
END SUBROUTINE VTKFile_WriteDataArray_Rank1_Int8
END INTERFACE

!----------------------------------------------------------------------------
!                                           WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Rank1_Int16( obj, name, x, isTuples)
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int16 ), INTENT( IN ) :: x( 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
END SUBROUTINE VTKFile_WriteDataArray_Rank1_Int16
END INTERFACE

!----------------------------------------------------------------------------
!                                           WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Rank1_Int32(obj, name, x, isTuples)
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int32 ), INTENT( IN ) :: x( 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
END SUBROUTINE VTKFile_WriteDataArray_Rank1_Int32
END INTERFACE

!----------------------------------------------------------------------------
!                                           WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

#ifdef USE_Int64
INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Rank1_Int64(obj, name, x, isTuples)
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int64 ), INTENT( IN ) :: x( 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
END SUBROUTINE VTKFile_WriteDataArray_Rank1_Int64
END INTERFACE
#endif

!----------------------------------------------------------------------------
!                                    WriteDataArray@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Appended( obj )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
END SUBROUTINE VTKFile_WriteDataArray_Appended
END INTERFACE

!----------------------------------------------------------------------------
!                                   WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch1( obj, x )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  CLASS( * ), INTENT( IN ) :: x( 1: )
END SUBROUTINE VTKFile_WriteToScratch1
END INTERFACE

!----------------------------------------------------------------------------
!                                   WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch2( obj, x )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  CLASS( * ), INTENT( IN ) :: x( 1:, 1: )
END SUBROUTINE VTKFile_WriteToScratch2
END INTERFACE

!----------------------------------------------------------------------------
!                                   WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch3( obj, x )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  CLASS( * ), INTENT( IN ) :: x( 1:, 1:, 1: )
END SUBROUTINE VTKFile_WriteToScratch3
END INTERFACE

!----------------------------------------------------------------------------
!                                   WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch4( obj, x )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  CLASS( * ), INTENT( IN ) :: x( 1:, 1:, 1:, 1: )
END SUBROUTINE VTKFile_WriteToScratch4
END INTERFACE

!----------------------------------------------------------------------------
!                                   WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch5( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  REAL( Real64 ), INTENT( IN ) :: x( 1: )
  REAL( Real64 ), INTENT( IN ) :: y( 1: )
  REAL( Real64 ), INTENT( IN ) :: z( 1: )
END SUBROUTINE VTKFile_WriteToScratch5
END INTERFACE

!----------------------------------------------------------------------------
!                                   WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch6( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  REAL( Real32 ), INTENT( IN ) :: x( 1: )
  REAL( Real32 ), INTENT( IN ) :: y( 1: )
  REAL( Real32 ), INTENT( IN ) :: z( 1: )
END SUBROUTINE VTKFile_WriteToScratch6
END INTERFACE

!----------------------------------------------------------------------------
!                                   WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

#ifdef USE_Int64
INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch7( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  INTEGER( Int64 ), INTENT( IN ) :: x( 1: )
  INTEGER( Int64 ), INTENT( IN ) :: y( 1: )
  INTEGER( Int64 ), INTENT( IN ) :: z( 1: )
END SUBROUTINE VTKFile_WriteToScratch7
END INTERFACE
#endif

!----------------------------------------------------------------------------
!                                   WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch8( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  INTEGER( Int32 ), INTENT( IN ) :: x( 1: )
  INTEGER( Int32 ), INTENT( IN ) :: y( 1: )
  INTEGER( Int32 ), INTENT( IN ) :: z( 1: )
END SUBROUTINE VTKFile_WriteToScratch8
END INTERFACE

!----------------------------------------------------------------------------
!                                   WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch9( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  INTEGER( Int16 ), INTENT( IN ) :: x( 1: )
  INTEGER( Int16 ), INTENT( IN ) :: y( 1: )
  INTEGER( Int16 ), INTENT( IN ) :: z( 1: )
END SUBROUTINE VTKFile_WriteToScratch9
END INTERFACE

!----------------------------------------------------------------------------
!                                   WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch10( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  INTEGER( Int8 ), INTENT( IN ) :: x( 1: )
  INTEGER( Int8 ), INTENT( IN ) :: y( 1: )
  INTEGER( Int8 ), INTENT( IN ) :: z( 1: )
END SUBROUTINE VTKFile_WriteToScratch10
END INTERFACE

!----------------------------------------------------------------------------
!                                   WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch11( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  REAL( Real64 ), INTENT( IN ) :: x( 1:, 1: )
  REAL( Real64 ), INTENT( IN ) :: y( 1:, 1: )
  REAL( Real64 ), INTENT( IN ) :: z( 1:, 1: )
END SUBROUTINE VTKFile_WriteToScratch11
END INTERFACE

!----------------------------------------------------------------------------
!                                   WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch12( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  REAL( Real32 ), INTENT( IN ) :: x( 1:, 1: )
  REAL( Real32 ), INTENT( IN ) :: y( 1:, 1: )
  REAL( Real32 ), INTENT( IN ) :: z( 1:, 1: )
END SUBROUTINE VTKFile_WriteToScratch12
END INTERFACE

!----------------------------------------------------------------------------
!                                   WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

#ifdef USE_Int64
INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch13( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  INTEGER( Int64 ), INTENT( IN ) :: x( 1:, 1: )
  INTEGER( Int64 ), INTENT( IN ) :: y( 1:, 1: )
  INTEGER( Int64 ), INTENT( IN ) :: z( 1:, 1: )
END SUBROUTINE VTKFile_WriteToScratch13
END INTERFACE
#endif

!----------------------------------------------------------------------------
!                                   WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch14( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  INTEGER( Int32 ), INTENT( IN ) :: x( 1:, 1: )
  INTEGER( Int32 ), INTENT( IN ) :: y( 1:, 1: )
  INTEGER( Int32 ), INTENT( IN ) :: z( 1:, 1: )
END SUBROUTINE VTKFile_WriteToScratch14
END INTERFACE

!----------------------------------------------------------------------------
!                                   WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch15( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  INTEGER( Int16 ), INTENT( IN ) :: x( 1:, 1: )
  INTEGER( Int16 ), INTENT( IN ) :: y( 1:, 1: )
  INTEGER( Int16 ), INTENT( IN ) :: z( 1:, 1: )
END SUBROUTINE VTKFile_WriteToScratch15
END INTERFACE

!----------------------------------------------------------------------------
!                                   WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch16( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  INTEGER( Int8 ), INTENT( IN ) :: x( 1:, 1: )
  INTEGER( Int8 ), INTENT( IN ) :: y( 1:, 1: )
  INTEGER( Int8 ), INTENT( IN ) :: z( 1:, 1: )
END SUBROUTINE VTKFile_WriteToScratch16
END INTERFACE

!----------------------------------------------------------------------------
!                                   WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch17( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  REAL( Real64 ), INTENT( IN ) :: x( 1:, 1:, 1: )
  REAL( Real64 ), INTENT( IN ) :: y( 1:, 1:, 1: )
  REAL( Real64 ), INTENT( IN ) :: z( 1:, 1:, 1: )
END SUBROUTINE VTKFile_WriteToScratch17
END INTERFACE

!----------------------------------------------------------------------------
!                                   WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch18( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  REAL( Real32 ), INTENT( IN ) :: x( 1:, 1:, 1: )
  REAL( Real32 ), INTENT( IN ) :: y( 1:, 1:, 1: )
  REAL( Real32 ), INTENT( IN ) :: z( 1:, 1:, 1: )
END SUBROUTINE VTKFile_WriteToScratch18
END INTERFACE

!----------------------------------------------------------------------------
!                                   WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

#ifdef USE_Int64
INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch19( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  INTEGER( Int64 ), INTENT( IN ) :: x( 1:, 1:, 1: )
  INTEGER( Int64 ), INTENT( IN ) :: y( 1:, 1:, 1: )
  INTEGER( Int64 ), INTENT( IN ) :: z( 1:, 1:, 1: )
END SUBROUTINE VTKFile_WriteToScratch19
END INTERFACE
#endif

!----------------------------------------------------------------------------
!                                   WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch20( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  INTEGER( Int32 ), INTENT( IN ) :: x( 1:, 1:, 1: )
  INTEGER( Int32 ), INTENT( IN ) :: y( 1:, 1:, 1: )
  INTEGER( Int32 ), INTENT( IN ) :: z( 1:, 1:, 1: )
END SUBROUTINE VTKFile_WriteToScratch20
END INTERFACE

!----------------------------------------------------------------------------
!                                   WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch21( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  INTEGER( Int16 ), INTENT( IN ) :: x( 1:, 1:, 1: )
  INTEGER( Int16 ), INTENT( IN ) :: y( 1:, 1:, 1: )
  INTEGER( Int16 ), INTENT( IN ) :: z( 1:, 1:, 1: )
END SUBROUTINE VTKFile_WriteToScratch21
END INTERFACE

!----------------------------------------------------------------------------
!                                   WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch22( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  INTEGER( Int8 ), INTENT( IN ) :: x( 1:, 1:, 1: )
  INTEGER( Int8 ), INTENT( IN ) :: y( 1:, 1:, 1: )
  INTEGER( Int8 ), INTENT( IN ) :: z( 1:, 1:, 1: )
END SUBROUTINE VTKFile_WriteToScratch22
END INTERFACE

!----------------------------------------------------------------------------
!                                       OpenScratchFile@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_OpenScratchFile( obj )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
END SUBROUTINE VTKFile_OpenScratchFile
END INTERFACE

!----------------------------------------------------------------------------
!                                       CloseScratchFile@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_CloseScratchFile( obj )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
END SUBROUTINE VTKFile_CloseScratchFile
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE VTKFile_Class