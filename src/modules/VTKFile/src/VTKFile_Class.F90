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
USE String_Class, ONLY:String
USE VTKDataArrayEncoder
USE XMLFile_Class
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
IMPLICIT NONE
PRIVATE
!!
PUBLIC :: encodeVTKDataArray
CHARACTER( LEN = * ), PARAMETER :: modName="VTKFile_Class"
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
  & "PUnstructuredGrid"  &
  & ]
INTEGER( I4B ), PARAMETER, PUBLIC :: VTK_ASCII = 1
INTEGER( I4B ), PARAMETER, PUBLIC :: VTK_BINARY = 2
INTEGER( I4B ), PARAMETER, PUBLIC :: VTK_APPENDED = 3
INTEGER( I4B ), PARAMETER, PUBLIC :: VTK_RAW_APPENDED = 3
INTEGER( I4B ), PARAMETER, PUBLIC :: VTK_BINARY_APPENDED = 4
CHARACTER( LEN = * ), PARAMETER, DIMENSION( 3 ) :: DataFormatName = &
  & [ &
  & "ascii   ", &
  & "binary  ", &
  & "appended" &
  & ]

!----------------------------------------------------------------------------
!                                                                   VTKFile_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 7 July 2022
! summary: VTKFile

TYPE, EXTENDS( XMLFile_ ) :: VTKFile_
  PRIVATE
  LOGICAL( LGT ) :: isStructured = .FALSE.
    !! Is dataset structured
  INTEGER( I4B ) :: dataStructureType = 0
    !! 1 : VTK_ImageData
    !! 2 : VTK_RectilinearGrid
    !! 3 : VTK_StructuredGrid
    !! 4 : VTK_PolyData
    !! 5 : VTK_UnstructuredGrid
    !! 6 : PARALLEL_VTK_ImageData
    !! 7 : PARALLEL_VTK_RectilinearGrid
    !! 8 : PARALLEL_VTK_StructuredGrid
    !! 9 : PARALLEL_VTK_PolyData
    !! 10: PARALLEL_VTK_UnstructuredGrid
  CHARACTER( LEN = MAX_LEN_DATA_STRUCTURENAME ) :: dataStructureName
    !! ImageData,
    !! RectilinearGrid,
    !! StructuredGrid,
    !! PolyData,
    !! UnstructuredGrid
    !! PImageData
    !! PRectilinearGrid
    !! PStructuredGrid
    !! PPolyData
    !! PUnstructuredGrid
  INTEGER( I4B ) :: dataFormat = 0
    !! VTK_ASCII
    !! VTK_BINARY
    !! VTK_APPENDED
    !! VTK_RAW_APPENDED
    !! VTK_BINARY_APPENDED
  INTEGER( I4B ) :: wholeExtent( 6 ) = 0
    !! Whole extent
  INTEGER( I4B ) :: origin( 3 ) = 0
    !! x0, y0, z0, Origin needed for ImageData
  INTEGER( I4B ) :: spacing( 3 ) = 1
    !! dx, dy, dz needed for ImageData
  INTEGER( I4B ) :: indent = 0
    !! Indent
  INTEGER( I4B ) :: offset = 0
    !! offset for appended mode
  TYPE( String ) :: encoding4Appended
    !! appended data encoding: "raw" or "base64".
  INTEGER( I4B ) :: scratch=0
    !! Used for scratch file
  LOGICAL( LGT ) :: isVolatile = .FALSE.
  TYPE( String ) :: volatileBuffer
  CONTAINS
  PRIVATE
  !!
  !! @ConstructorMethods
  !!
  PROCEDURE, PUBLIC, PASS( obj ) :: InitiateVTKFile
  PROCEDURE, PUBLIC, PASS( obj ) :: addSurrogate => VTKFile_addSurrogate
  PROCEDURE, PUBLIC, PASS( obj ) :: Deallocate => VTKFile_Deallocate
  FINAL :: VTKFile_Final
  PROCEDURE, PUBLIC, PASS( obj ) :: Close => VTKFile_Close
  PROCEDURE, PASS( obj ) :: UpdateOffset => VTKFile_UpdateOffset
  PROCEDURE, PASS( obj ) :: OpenScratchFile => VTKFile_OpenScratchFile
  PROCEDURE, PASS( obj ) :: CloseScratchFile => VTKFile_CloseScratchFile
  !!
  !! @TagsMethods
  !!
  PROCEDURE, PUBLIC, PASS( obj ) :: WriteRootTag => VTKFile_WriteRootTag
  PROCEDURE, PUBLIC, PASS( obj ) :: WriteDataStructureTag => &
    & VTKFile_WriteDataStructureTag
  PROCEDURE, PUBLIC, PASS( obj ) :: WriteStartTag => VTKFile_WriteStartTag
  PROCEDURE, PUBLIC, PASS( obj ) :: WriteEndTag => VTKFile_WriteEndTag
  PROCEDURE, PUBLIC, PASS( obj ) :: WriteSelfClosingTag => &
    & VTKFile_WriteSelfClosingTag
  PROCEDURE, PUBLIC, PASS( obj ) :: WriteTag => VTKFile_WriteTag
  !!
  !! @VertMethods
  !!
  PROCEDURE, PUBLIC, PASS( obj ) :: WriteVerts => VTKFile_WriteVerts
  !!
  !! @CellMethods
  !!
  PROCEDURE, PUBLIC, PASS( obj ) :: WriteCells => VTKFile_WriteCells
  !!
  !! @DataArrayMethods
  !!
  PROCEDURE, PUBLIC, PASS( obj ) :: WriteDataArrayTag => &
    & VTKFile_WriteDataArrayTag
  PROCEDURE, PUBLIC, PASS( obj ) :: WriteDataArrayLocationTag => &
    & VTKFile_WriteDataArrayLocationTag
  PROCEDURE, PASS( obj ) :: &
    & VTKFile_WriteDataArrayLocationTag, &
    & VTKFile_WriteDataArray_Rank1_Real32, &
    & VTKFile_WriteDataArray_Rank1_Real64, &
    & VTKFile_WriteDataArray_Rank1_Int64, &
    & VTKFile_WriteDataArray_Rank1_Int32, &
    & VTKFile_WriteDataArray_Rank1_Int16, &
    & VTKFile_WriteDataArray_Rank1_Int8, &
    & VTKFile_WriteDataArray_Rank2_Real32, &
    & VTKFile_WriteDataArray_Rank2_Real64, &
    & VTKFile_WriteDataArray_Rank2_Int64, &
    & VTKFile_WriteDataArray_Rank2_Int32, &
    & VTKFile_WriteDataArray_Rank2_Int16, &
    & VTKFile_WriteDataArray_Rank2_Int8, &
    & VTKFile_WriteDataArray_Rank3_Real32, &
    & VTKFile_WriteDataArray_Rank3_Real64, &
    & VTKFile_WriteDataArray_Rank3_Int64, &
    & VTKFile_WriteDataArray_Rank3_Int32, &
    & VTKFile_WriteDataArray_Rank3_Int16, &
    & VTKFile_WriteDataArray_Rank3_Int8, &
    & VTKFile_WriteDataArray_Rank4_Real32, &
    & VTKFile_WriteDataArray_Rank4_Real64, &
    & VTKFile_WriteDataArray_Rank4_Int64, &
    & VTKFile_WriteDataArray_Rank4_Int32, &
    & VTKFile_WriteDataArray_Rank4_Int16, &
    & VTKFile_WriteDataArray_Rank4_Int8, &
    & VTKFile_WriteDataArray_XYZ_Rank1_Real32, &
    & VTKFile_WriteDataArray_XYZ_Rank1_Real64, &
    & VTKFile_WriteDataArray_XYZ_Rank1_Int64, &
    & VTKFile_WriteDataArray_XYZ_Rank1_Int32, &
    & VTKFile_WriteDataArray_XYZ_Rank1_Int16, &
    & VTKFile_WriteDataArray_XYZ_Rank1_Int8, &
    & VTKFile_WriteDataArray_XYZ_Rank2_Real32, &
    & VTKFile_WriteDataArray_XYZ_Rank2_Real64, &
    & VTKFile_WriteDataArray_XYZ_Rank2_Int64, &
    & VTKFile_WriteDataArray_XYZ_Rank2_Int32, &
    & VTKFile_WriteDataArray_XYZ_Rank2_Int16, &
    & VTKFile_WriteDataArray_XYZ_Rank2_Int8, &
    & VTKFile_WriteDataArray_XYZ_Rank3_Real32, &
    & VTKFile_WriteDataArray_XYZ_Rank3_Real64, &
    & VTKFile_WriteDataArray_XYZ_Rank3_Int64, &
    & VTKFile_WriteDataArray_XYZ_Rank3_Int32, &
    & VTKFile_WriteDataArray_XYZ_Rank3_Int16, &
    & VTKFile_WriteDataArray_XYZ_Rank3_Int8
  GENERIC, PUBLIC :: WriteDataArray => &
    & VTKFile_WriteDataArrayLocationTag, &
    & VTKFile_WriteDataArray_Rank1_Real32, &
    & VTKFile_WriteDataArray_Rank1_Real64, &
    & VTKFile_WriteDataArray_Rank1_Int64, &
    & VTKFile_WriteDataArray_Rank1_Int32, &
    & VTKFile_WriteDataArray_Rank1_Int16, &
    & VTKFile_WriteDataArray_Rank1_Int8, &
    & VTKFile_WriteDataArray_Rank2_Real32, &
    & VTKFile_WriteDataArray_Rank2_Real64, &
    & VTKFile_WriteDataArray_Rank2_Int64, &
    & VTKFile_WriteDataArray_Rank2_Int32, &
    & VTKFile_WriteDataArray_Rank2_Int16, &
    & VTKFile_WriteDataArray_Rank2_Int8, &
    & VTKFile_WriteDataArray_Rank3_Real32, &
    & VTKFile_WriteDataArray_Rank3_Real64, &
    & VTKFile_WriteDataArray_Rank3_Int64, &
    & VTKFile_WriteDataArray_Rank3_Int32, &
    & VTKFile_WriteDataArray_Rank3_Int16, &
    & VTKFile_WriteDataArray_Rank3_Int8, &
    & VTKFile_WriteDataArray_Rank4_Real32, &
    & VTKFile_WriteDataArray_Rank4_Real64, &
    & VTKFile_WriteDataArray_Rank4_Int64, &
    & VTKFile_WriteDataArray_Rank4_Int32, &
    & VTKFile_WriteDataArray_Rank4_Int16, &
    & VTKFile_WriteDataArray_Rank4_Int8, &
    & VTKFile_WriteDataArray_XYZ_Rank1_Real32, &
    & VTKFile_WriteDataArray_XYZ_Rank1_Real64, &
    & VTKFile_WriteDataArray_XYZ_Rank1_Int64, &
    & VTKFile_WriteDataArray_XYZ_Rank1_Int32, &
    & VTKFile_WriteDataArray_XYZ_Rank1_Int16, &
    & VTKFile_WriteDataArray_XYZ_Rank1_Int8, &
    & VTKFile_WriteDataArray_XYZ_Rank2_Real32, &
    & VTKFile_WriteDataArray_XYZ_Rank2_Real64, &
    & VTKFile_WriteDataArray_XYZ_Rank2_Int64, &
    & VTKFile_WriteDataArray_XYZ_Rank2_Int32, &
    & VTKFile_WriteDataArray_XYZ_Rank2_Int16, &
    & VTKFile_WriteDataArray_XYZ_Rank2_Int8, &
    & VTKFile_WriteDataArray_XYZ_Rank3_Real32, &
    & VTKFile_WriteDataArray_XYZ_Rank3_Real64, &
    & VTKFile_WriteDataArray_XYZ_Rank3_Int64, &
    & VTKFile_WriteDataArray_XYZ_Rank3_Int32, &
    & VTKFile_WriteDataArray_XYZ_Rank3_Int16, &
    & VTKFile_WriteDataArray_XYZ_Rank3_Int8
  !!
  !! @DataArrayAppendedMethods
  !!
  PROCEDURE, PASS( obj ) :: &
    & VTKFile_WriteDataArray_Appended, &
    & VTKFile_WriteToScratch1, &
    & VTKFile_WriteToScratch2, &
    & VTKFile_WriteToScratch3, &
    & VTKFile_WriteToScratch4, &
    & VTKFile_WriteToScratch5, &
    & VTKFile_WriteToScratch6, &
    & VTKFile_WriteToScratch7, &
    & VTKFile_WriteToScratch8, &
    & VTKFile_WriteToScratch9, &
    & VTKFile_WriteToScratch10, &
    & VTKFile_WriteToScratch11, &
    & VTKFile_WriteToScratch12, &
    & VTKFile_WriteToScratch13, &
    & VTKFile_WriteToScratch14, &
    & VTKFile_WriteToScratch15, &
    & VTKFile_WriteToScratch16, &
    & VTKFile_WriteToScratch17, &
    & VTKFile_WriteToScratch18, &
    & VTKFile_WriteToScratch19, &
    & VTKFile_WriteToScratch20, &
    & VTKFile_WriteToScratch21, &
    & VTKFile_WriteToScratch22
  !!
  GENERIC, PUBLIC :: WriteDataArray => VTKFile_WriteDataArray_Appended
  !!
  GENERIC :: WriteToScratch => &
    & VTKFile_WriteToScratch1, &
    & VTKFile_WriteToScratch2, &
    & VTKFile_WriteToScratch3, &
    & VTKFile_WriteToScratch4, &
    & VTKFile_WriteToScratch5, &
    & VTKFile_WriteToScratch6, &
    & VTKFile_WriteToScratch7, &
    & VTKFile_WriteToScratch8, &
    & VTKFile_WriteToScratch9, &
    & VTKFile_WriteToScratch10, &
    & VTKFile_WriteToScratch11, &
    & VTKFile_WriteToScratch12, &
    & VTKFile_WriteToScratch13, &
    & VTKFile_WriteToScratch14, &
    & VTKFile_WriteToScratch15, &
    & VTKFile_WriteToScratch16, &
    & VTKFile_WriteToScratch17, &
    & VTKFile_WriteToScratch18, &
    & VTKFile_WriteToScratch19, &
    & VTKFile_WriteToScratch20, &
    & VTKFile_WriteToScratch21, &
    & VTKFile_WriteToScratch22
  !!
  !!@FieldDataMethods
  !!
  GENERIC, PUBLIC :: WriteFieldData => &
    & VTKFile_WriteFieldData_1, &
    & VTKFile_WriteFieldData_2
  !!
  PROCEDURE, PASS( obj ) :: &
    & VTKFile_WriteFieldData_1, &
    & VTKFile_WriteFieldData_2
  !!
  !! @PointsMethods
  !!
  GENERIC, PUBLIC :: WritePoints => &
    & VTKFile_WritePoints_1, &
    & VTKFile_WritePoints_2, &
    & VTKFile_WritePoints_3, &
    & VTKFile_WritePoints_4, &
    & VTKFile_WritePoints_5, &
    & VTKFile_WritePoints_6
  !!
  PROCEDURE, PASS( obj ) :: &
    & VTKFile_WritePoints_1, &
    & VTKFile_WritePoints_2, &
    & VTKFile_WritePoints_3, &
    & VTKFile_WritePoints_4, &
    & VTKFile_WritePoints_5, &
    & VTKFile_WritePoints_6
  !!
  !! @PieceMethods
  !!
  GENERIC, PUBLIC :: WritePiece => &
    & VTKFile_WritePiece_1, &
    & VTKFile_WritePiece_2, &
    & VTKFile_WritePiece_3, &
    & VTKFile_WritePiece_4
  !!
  PROCEDURE, PASS( obj ) :: &
    & VTKFile_WritePiece_1, &
    & VTKFile_WritePiece_2, &
    & VTKFile_WritePiece_3, &
    & VTKFile_WritePiece_4
  !!
  ! PROCEDURE, PUBLIC, PASS( obj ) :: WriteConnectivity
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
!
!----------------------------------------------------------------------------

#include "./ConstructorMethods.inc"
#include "./TagMethods.inc"
#include "./VertMethods.inc"
#include "./CellMethods.inc"
#include "./DataArrayMethods.inc"
#include "./DataArrayAppendedMethods.inc"
#include "./FieldDataMethods.inc"
#include "./PointsMethods.inc"
#include "./PieceMethods.inc"

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE VTKFile_Class