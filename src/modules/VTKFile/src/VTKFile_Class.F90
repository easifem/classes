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
USE Globaldata
USE BaseType
USE String_Class, ONLY: String
USE VTKdataArrayEncoder
USE XMLFile_Class
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE
PRIVATE

PUBLIC :: encodeVTKdataArray

CHARACTER(*), PARAMETER :: modName = "VTKFile_Class"
INTEGER(I4B), PARAMETER :: MAX_LEN_data_STRUCTURENAME = 256
INTEGER(I4B), PARAMETER, PUBLIC :: VTK_Imagedata = 1
INTEGER(I4B), PARAMETER, PUBLIC :: VTK_RectilinearGrid = 2
INTEGER(I4B), PARAMETER, PUBLIC :: VTK_StructuredGrid = 3
INTEGER(I4B), PARAMETER, PUBLIC :: VTK_Polydata = 4
INTEGER(I4B), PARAMETER, PUBLIC :: VTK_UnstructuredGrid = 5
INTEGER(I4B), PARAMETER, PUBLIC :: PARALLEL_VTK_Imagedata = 6
INTEGER(I4B), PARAMETER, PUBLIC :: PARALLEL_VTK_RectilinearGrid = 7
INTEGER(I4B), PARAMETER, PUBLIC :: PARALLEL_VTK_StructuredGrid = 8
INTEGER(I4B), PARAMETER, PUBLIC :: PARALLEL_VTK_Polydata = 9
INTEGER(I4B), PARAMETER, PUBLIC :: PARALLEL_VTK_UnstructuredGrid = 10
CHARACTER(*), PARAMETER, DIMENSION(10) :: dataStructureName = &
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
INTEGER(I4B), PARAMETER, PUBLIC :: VTK_ASCII = 1
INTEGER(I4B), PARAMETER, PUBLIC :: VTK_BINARY = 2
INTEGER(I4B), PARAMETER, PUBLIC :: VTK_APPENDED = 3
INTEGER(I4B), PARAMETER, PUBLIC :: VTK_RAW_APPENDED = 3
INTEGER(I4B), PARAMETER, PUBLIC :: VTK_BINARY_APPENDED = 4
CHARACTER(*), PARAMETER, DIMENSION(3) :: dataFormatName = &
  & [ &
  & "ascii   ", &
  & "binary  ", &
  & "appended" &
  & ]
PUBLIC :: VTKFile_
PUBLIC :: VTKFilePointer_

!----------------------------------------------------------------------------
!                                                                   VTKFile_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 7 July 2022
! summary: VTKFile

TYPE, EXTENDS(XMLFile_) :: VTKFile_
  PRIVATE
  LOGICAL(LGT) :: isStructured = .FALSE.
    !! Is dataset structured
  INTEGER(I4B) :: dataStructureType = 0
    !! 1 : VTK_Imagedata
    !! 2 : VTK_RectilinearGrid
    !! 3 : VTK_StructuredGrid
    !! 4 : VTK_Polydata
    !! 5 : VTK_UnstructuredGrid
    !! 6 : PARALLEL_VTK_Imagedata
    !! 7 : PARALLEL_VTK_RectilinearGrid
    !! 8 : PARALLEL_VTK_StructuredGrid
    !! 9 : PARALLEL_VTK_Polydata
    !! 10: PARALLEL_VTK_UnstructuredGrid
  CHARACTER(MAX_LEN_data_STRUCTURENAME) :: dataStructureName
    !! Imagedata,
    !! RectilinearGrid,
    !! StructuredGrid,
    !! Polydata,
    !! UnstructuredGrid
    !! PImagedata
    !! PRectilinearGrid
    !! PStructuredGrid
    !! PPolydata
    !! PUnstructuredGrid
  INTEGER(I4B) :: dataFormat = 0
    !! VTK_ASCII
    !! VTK_BINARY
    !! VTK_APPENDED
    !! VTK_RAW_APPENDED
    !! VTK_BINARY_APPENDED
  INTEGER(I4B) :: wholeExtent(6) = 0
    !! Whole extent
  INTEGER(I4B) :: origin(3) = 0
    !! x0, y0, z0, Origin needed for Imagedata
  INTEGER(I4B) :: SPACING(3) = 1
    !! dx, dy, dz needed for Imagedata
  INTEGER(I4B) :: indent = 0
    !! Indent
  INTEGER(I4B) :: offset = 0
    !! offset for appended mode
  TYPE(String) :: encoding4Appended
    !! appended data encoding: "raw" or "base64".
  INTEGER(I4B) :: scratch = 0
    !! Used for scratch file
  LOGICAL(LGT) :: isVolatile = .FALSE.
  TYPE(String) :: volatileBuffer
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateVTKFile
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => VTKFile_Deallocate
  FINAL :: VTKFile_Final
  PROCEDURE, PUBLIC, PASS(obj) :: CLOSE => VTKFile_Close
  PROCEDURE, PASS(obj) :: UpdateOffset => VTKFile_UpdateOffset
  PROCEDURE, PASS(obj) :: OpenScratchFile => VTKFile_OpenScratchFile
  PROCEDURE, PASS(obj) :: CloseScratchFile => VTKFile_CloseScratchFile

  ! IO:
  ! @TagsMethods
  PROCEDURE, PUBLIC, PASS(obj) :: WriteRootTag => VTKFile_WriteRootTag
  PROCEDURE, PUBLIC, PASS(obj) :: WritedataStructureTag => &
    & VTKFile_WritedataStructureTag
  PROCEDURE, PUBLIC, PASS(obj) :: WriteStartTag => VTKFile_WriteStartTag
  PROCEDURE, PUBLIC, PASS(obj) :: WriteEndTag => VTKFile_WriteEndTag
  PROCEDURE, PUBLIC, PASS(obj) :: WriteSelfClosingTag => &
    & VTKFile_WriteSelfClosingTag
  PROCEDURE, PUBLIC, PASS(obj) :: WriteTag => VTKFile_WriteTag

  ! IO:
  ! @VertMethods
  PROCEDURE, PUBLIC, PASS(obj) :: WriteVerts => VTKFile_WriteVerts

  ! IO:
  ! @CellMethods
  PROCEDURE, PUBLIC, PASS(obj) :: WriteCells => VTKFile_WriteCells

  ! IO:
  ! @dataArrayMethods
  PROCEDURE, PUBLIC, PASS(obj) :: WritedataArrayTag => &
    & VTKFile_WritedataArrayTag
  PROCEDURE, PUBLIC, PASS(obj) :: WritedataArrayLocationTag => &
    & VTKFile_WritedataArrayLocationTag
  PROCEDURE, PASS(obj) :: &
    & VTKFile_WritedataArrayLocationTag, &
    & VTKFile_WritedataArray_Rank1_Real32, &
    & VTKFile_WritedataArray_Rank1_Real64, &
    & VTKFile_WritedataArray_Rank1_Int64, &
    & VTKFile_WritedataArray_Rank1_Int32, &
    & VTKFile_WritedataArray_Rank1_Int16, &
    & VTKFile_WritedataArray_Rank1_Int8, &
    & VTKFile_WritedataArray_Rank2_Real32, &
    & VTKFile_WritedataArray_Rank2_Real64, &
    & VTKFile_WritedataArray_Rank2_Int64, &
    & VTKFile_WritedataArray_Rank2_Int32, &
    & VTKFile_WritedataArray_Rank2_Int16, &
    & VTKFile_WritedataArray_Rank2_Int8, &
    & VTKFile_WritedataArray_Rank3_Real32, &
    & VTKFile_WritedataArray_Rank3_Real64, &
    & VTKFile_WritedataArray_Rank3_Int64, &
    & VTKFile_WritedataArray_Rank3_Int32, &
    & VTKFile_WritedataArray_Rank3_Int16, &
    & VTKFile_WritedataArray_Rank3_Int8, &
    & VTKFile_WritedataArray_Rank4_Real32, &
    & VTKFile_WritedataArray_Rank4_Real64, &
    & VTKFile_WritedataArray_Rank4_Int64, &
    & VTKFile_WritedataArray_Rank4_Int32, &
    & VTKFile_WritedataArray_Rank4_Int16, &
    & VTKFile_WritedataArray_Rank4_Int8, &
    & VTKFile_WritedataArray_XYZ_Rank1_Real32, &
    & VTKFile_WritedataArray_XYZ_Rank1_Real64, &
    & VTKFile_WritedataArray_XYZ_Rank1_Int64, &
    & VTKFile_WritedataArray_XYZ_Rank1_Int32, &
    & VTKFile_WritedataArray_XYZ_Rank1_Int16, &
    & VTKFile_WritedataArray_XYZ_Rank1_Int8, &
    & VTKFile_WritedataArray_XYZ_Rank2_Real32, &
    & VTKFile_WritedataArray_XYZ_Rank2_Real64, &
    & VTKFile_WritedataArray_XYZ_Rank2_Int64, &
    & VTKFile_WritedataArray_XYZ_Rank2_Int32, &
    & VTKFile_WritedataArray_XYZ_Rank2_Int16, &
    & VTKFile_WritedataArray_XYZ_Rank2_Int8, &
    & VTKFile_WritedataArray_XYZ_Rank3_Real32, &
    & VTKFile_WritedataArray_XYZ_Rank3_Real64, &
    & VTKFile_WritedataArray_XYZ_Rank3_Int64, &
    & VTKFile_WritedataArray_XYZ_Rank3_Int32, &
    & VTKFile_WritedataArray_XYZ_Rank3_Int16, &
    & VTKFile_WritedataArray_XYZ_Rank3_Int8
  GENERIC, PUBLIC :: WritedataArray => &
    & VTKFile_WritedataArrayLocationTag, &
    & VTKFile_WritedataArray_Rank1_Real32, &
    & VTKFile_WritedataArray_Rank1_Real64, &
    & VTKFile_WritedataArray_Rank1_Int64, &
    & VTKFile_WritedataArray_Rank1_Int32, &
    & VTKFile_WritedataArray_Rank1_Int16, &
    & VTKFile_WritedataArray_Rank1_Int8, &
    & VTKFile_WritedataArray_Rank2_Real32, &
    & VTKFile_WritedataArray_Rank2_Real64, &
    & VTKFile_WritedataArray_Rank2_Int64, &
    & VTKFile_WritedataArray_Rank2_Int32, &
    & VTKFile_WritedataArray_Rank2_Int16, &
    & VTKFile_WritedataArray_Rank2_Int8, &
    & VTKFile_WritedataArray_Rank3_Real32, &
    & VTKFile_WritedataArray_Rank3_Real64, &
    & VTKFile_WritedataArray_Rank3_Int64, &
    & VTKFile_WritedataArray_Rank3_Int32, &
    & VTKFile_WritedataArray_Rank3_Int16, &
    & VTKFile_WritedataArray_Rank3_Int8, &
    & VTKFile_WritedataArray_Rank4_Real32, &
    & VTKFile_WritedataArray_Rank4_Real64, &
    & VTKFile_WritedataArray_Rank4_Int64, &
    & VTKFile_WritedataArray_Rank4_Int32, &
    & VTKFile_WritedataArray_Rank4_Int16, &
    & VTKFile_WritedataArray_Rank4_Int8, &
    & VTKFile_WritedataArray_XYZ_Rank1_Real32, &
    & VTKFile_WritedataArray_XYZ_Rank1_Real64, &
    & VTKFile_WritedataArray_XYZ_Rank1_Int64, &
    & VTKFile_WritedataArray_XYZ_Rank1_Int32, &
    & VTKFile_WritedataArray_XYZ_Rank1_Int16, &
    & VTKFile_WritedataArray_XYZ_Rank1_Int8, &
    & VTKFile_WritedataArray_XYZ_Rank2_Real32, &
    & VTKFile_WritedataArray_XYZ_Rank2_Real64, &
    & VTKFile_WritedataArray_XYZ_Rank2_Int64, &
    & VTKFile_WritedataArray_XYZ_Rank2_Int32, &
    & VTKFile_WritedataArray_XYZ_Rank2_Int16, &
    & VTKFile_WritedataArray_XYZ_Rank2_Int8, &
    & VTKFile_WritedataArray_XYZ_Rank3_Real32, &
    & VTKFile_WritedataArray_XYZ_Rank3_Real64, &
    & VTKFile_WritedataArray_XYZ_Rank3_Int64, &
    & VTKFile_WritedataArray_XYZ_Rank3_Int32, &
    & VTKFile_WritedataArray_XYZ_Rank3_Int16, &
    & VTKFile_WritedataArray_XYZ_Rank3_Int8

  ! IO:
  ! @dataArrayAppendedMethods
  PROCEDURE, PASS(obj) :: &
    & VTKFile_WritedataArray_Appended, &
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

  GENERIC, PUBLIC :: WritedataArray => VTKFile_WritedataArray_Appended

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

  ! IO:
  !@FielddataMethods
  GENERIC, PUBLIC :: WriteFielddata => VTKFile_WriteFielddata_1, &
    & VTKFile_WriteFielddata_2

  PROCEDURE, PASS(obj) :: VTKFile_WriteFielddata_1, &
    & VTKFile_WriteFielddata_2

  ! IO:
  ! @PointsMethods
  GENERIC, PUBLIC :: WritePoints => &
    & VTKFile_WritePoints_1, &
    & VTKFile_WritePoints_2, &
    & VTKFile_WritePoints_3, &
    & VTKFile_WritePoints_4, &
    & VTKFile_WritePoints_5, &
    & VTKFile_WritePoints_6

  PROCEDURE, PASS(obj) :: &
    & VTKFile_WritePoints_1, &
    & VTKFile_WritePoints_2, &
    & VTKFile_WritePoints_3, &
    & VTKFile_WritePoints_4, &
    & VTKFile_WritePoints_5, &
    & VTKFile_WritePoints_6

  ! IO:
  ! @PieceMethods
  GENERIC, PUBLIC :: WritePiece => &
    & VTKFile_WritePiece_1, &
    & VTKFile_WritePiece_2, &
    & VTKFile_WritePiece_3, &
    & VTKFile_WritePiece_4

  PROCEDURE, PASS(obj) :: &
    & VTKFile_WritePiece_1, &
    & VTKFile_WritePiece_2, &
    & VTKFile_WritePiece_3, &
    & VTKFile_WritePiece_4

  ! PROCEDURE, PUBLIC, PASS( obj ) :: WriteConnectivity
  ! PROCEDURE, PUBLIC, PASS( obj ) :: WriteGeo
END TYPE VTKFile_

!----------------------------------------------------------------------------
!                                                           VTKFilePointer_
!----------------------------------------------------------------------------

TYPE :: VTKFilePointer_
  CLASS(VTKFile_), POINTER :: ptr => NULL()
END TYPE VTKFilePointer_

!----------------------------------------------------------------------------
!                                                VTKFile@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 Sept 2021
! summary: This function returns an instance of VTKFile
!
!# Introduction
! This function returns an instance if VTKFile. After calling this function
! This routine initiates the XMLFile, and opens it.
! It also write header file and dataStructure tag to the file.
!
!## Arguments:
!
! *filename*: is the name of the file
!
! *mode* : can be `READ`, `WRITE`, `NEW`, `OVERWRITE`. It is like action.
!
! *dataFormat* : defines the format of dataArray in VTKFile, it can be one fo
! the following:
!
! - `VTK_ASCII`
! - `VTK_BINARY`
! - `VTK_APPENED`
! - `VTK_RAW_APPENED`,
! - `VTK_BINARY_APPENDED`.
! - `VTK_APPENDED` and `VTK_RAW_APPENDED` are the same, where appended data
! is in raw format
! - `VTK_BINARY_APPENDED`, on the other hand, appends data in binary form
!
! *dataStructureType* : The type of vtk file. It can have following values
!
! - Imagedata,
! - RectilinearGrid,
! - StructuredGrid,
! - Polydata,
! - UnstructuredGrid
! - PImagedata
! - PRectilinearGrid
! - PStructuredGrid
! - PPolydata
! - PUnstructuredGrid
!
! *WholeExtent* : is [x1, x2, y1, y2, z1, z2], it is required for structured
! data set.
!
! *isVolatile* : It is True if volatileBuffer is used to keep the xml file
!
! *meshdataFormat* It can be Float32, Float64

INTERFACE
  MODULE SUBROUTINE InitiateVTKFile(obj, filename, mode, dataFormat, &
    & dataStructureType, WholeExtent, isVolatile, meshdataFormat, &
    & Spacing, Origin)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: filename
    !! Name of the file, where xml data will be printed
    CHARACTER(*), OPTIONAL, INTENT(IN) :: mode
    !! READ, WRITE, NEW, REPLACE
    !! DEFAULT IS NEW
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dataFormat
    !! VTK_ASCII, VTK_APPENEDED, VTK_BINARY
    !! DEFAULT is VTK_BINARY_APPENDED
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dataStructureType
    !! VTK_IMAGEdata, VTK_STRUCTUREDGRID, etc.
    !! Default is VTK_UnstructuredGrid
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: WholeExtent(:)
    !! Required for structured data set
    !! [x1, x2, y1, y2, z1, z2]
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isVolatile
    !! True if volatileBuffer is used to keep the xml file
    CHARACTER(*), OPTIONAL, INTENT(IN) :: meshdataFormat
    !! Float32, Float64, etc
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: SPACING(:)
    !! dx, dy, dz, needed in case of Imagedata
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: Origin(:)
    !! x0, y0, z0, needed in case of Imagedata
  END SUBROUTINE InitiateVTKFile
END INTERFACE

!----------------------------------------------------------------------------
!                                         Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 Sept 2021
! summary: Deallocates the content of VTKFile and close or delete it.

INTERFACE
  MODULE SUBROUTINE VTKFile_Deallocate(obj, delete)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: delete
  END SUBROUTINE VTKFile_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 Sept 2021
! summary: Finalizer for VTKFile

INTERFACE
  MODULE SUBROUTINE VTKFile_Final(obj)
    TYPE(VTKFile_), INTENT(INOUT) :: obj
  END SUBROUTINE VTKFile_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                            UpdateOffset@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 7 July 2022
! summary: Update the offset in VTKFile
!
!# Introduction
!
! If the dataformat is VTK_APPENDED then update the offset by number of
! bytes `nByte`

INTERFACE
  MODULE ELEMENTAL SUBROUTINE VTKFile_UpdateOffset(obj, nByte)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nByte
  END SUBROUTINE VTKFile_UpdateOffset
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Close@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 Sept 2021
! summary: Close the file

INTERFACE
  MODULE SUBROUTINE VTKFile_Close(obj)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
  END SUBROUTINE VTKFile_Close
END INTERFACE

!----------------------------------------------------------------------------
!                                         OpenScratchFile@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 7 July 2022
! summary: Open the scratch file if data format is `VTK_APPENDED`

INTERFACE
  MODULE SUBROUTINE VTKFile_OpenScratchFile(obj)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
  END SUBROUTINE VTKFile_OpenScratchFile
END INTERFACE

!----------------------------------------------------------------------------
!                                       CloseScratchFile@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 7 July 2022
! summary: Close the scratch file

INTERFACE
  MODULE SUBROUTINE VTKFile_CloseScratchFile(obj)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
  END SUBROUTINE VTKFile_CloseScratchFile
END INTERFACE

!----------------------------------------------------------------------------
!                                                   WriteRootTag@TagsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 7 July 2022
! summary: Write the root tag
!
!# Introduction
!
! This routine write the root tag. This routine is called when we initiate
! the VTKFile. following information is written. Also indent is set to 2.
!
!```xml
! <?xml version="1.0" encoding="UTF-8"?>
! <VTKFile type="'//trim(obj%dataStructureName)// &
! & '" version="1.0" byte_order="LittleEndian">
!```

INTERFACE
  MODULE SUBROUTINE VTKFile_WriteRootTag(obj)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
  END SUBROUTINE VTKFile_WriteRootTag
END INTERFACE

!----------------------------------------------------------------------------
!                                           WritedataStructureTag@TagsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 7 July 2022
! summary: Write the data structure tags

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataStructureTag(obj, meshdataFormat)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    CHARACTER(*), OPTIONAL, INTENT(IN) :: meshdataFormat
    !! Float32, Float64
    !! It should be present for parallel data structure
    !! PARALLEL_VTK_RECTILINEARGRID, PARALLEL_VTK_STRUCTUREDGRID,
    !! PARALLEL_VTK_UNSTRUCTUREDGRID
  END SUBROUTINE VTKFile_WritedataStructureTag
END INTERFACE

!----------------------------------------------------------------------------
!                                                  WriteStartTag@TagsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 Sept 2021
! summary: Write the start tag

INTERFACE
  MODULE SUBROUTINE VTKFile_WriteStartTag(obj, name, attrNames, attrValues)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    TYPE(String), OPTIONAL, INTENT(IN) :: attrNames(:)
    TYPE(String), OPTIONAL, INTENT(IN) :: attrValues(:)
  END SUBROUTINE VTKFile_WriteStartTag
END INTERFACE

!----------------------------------------------------------------------------
!                                                    WriteEndTag@TagsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 Sept 2021
! summary: Write the End tag

INTERFACE
  MODULE SUBROUTINE VTKFile_WriteEndTag(obj, name)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
  END SUBROUTINE VTKFile_WriteEndTag
END INTERFACE

!----------------------------------------------------------------------------
!                                            WriteSelfClosingTag@TagsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 Sept 2021
! summary: Write the self closing tag

INTERFACE
  MODULE SUBROUTINE VTKFile_WriteSelfClosingTag(obj, name, attrNames, &
    & attrValues)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    TYPE(String), OPTIONAL, INTENT(IN) :: attrNames(:)
    TYPE(String), OPTIONAL, INTENT(IN) :: attrValues(:)
  END SUBROUTINE VTKFile_WriteSelfClosingTag
END INTERFACE

!----------------------------------------------------------------------------
!                                                       WriteTag@TagsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 Sept 2021
! summary: Write Tags and contents

INTERFACE
  MODULE SUBROUTINE VTKFile_WriteTag(obj, name, attrNames, &
    & attrValues, content)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    TYPE(String), OPTIONAL, INTENT(IN) :: attrNames(:)
    TYPE(String), OPTIONAL, INTENT(IN) :: attrValues(:)
    TYPE(String), OPTIONAL, INTENT(IN) :: content
  END SUBROUTINE VTKFile_WriteTag
END INTERFACE

!----------------------------------------------------------------------------
!                                                WriteVerts@WriteVertMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 sept 2021
! summary: Write Vertices

INTERFACE
  MODULE SUBROUTINE VTKFile_WriteVerts(obj, connectivity, offsets)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: connectivity(:)
    INTEGER(I4B), INTENT(IN) :: offsets(:)
  END SUBROUTINE VTKFile_WriteVerts
END INTERFACE

!----------------------------------------------------------------------------
!                                                WriteCells@CellMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 sept 2021
! summary: Write Cells

INTERFACE
  MODULE SUBROUTINE VTKFile_WriteCells(obj, connectivity, offsets, types)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: connectivity(:)
    INTEGER(I4B), INTENT(IN) :: offsets(:)
    INTEGER(INT8), INTENT(IN) :: types(:)
  END SUBROUTINE VTKFile_WriteCells
END INTERFACE

!----------------------------------------------------------------------------
!                                 WritedataArrayLocationTag@dataArrayMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 Sept 2021
! summary: WritedataArrayLocation CELL or NODE
!
!# Introduction
!
! Write Pointdata or Celldata open/close tag.
!
!@note
! This routine must be called before saving the data related to geometric
! mesh, this function initializes the saving of data variables indicating
! the *location* (node or cell centered) of variables that will be saved.
!@endnote
!
!@note
! A single file can contain both cell and node centered variables.
!@endnote
!

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArrayLocationTag(obj, location, action)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: location
    !! "CELL" and "NODE"
    TYPE(String), INTENT(IN) :: action
    !! "OPEN" or "CLOSE"
  END SUBROUTINE VTKFile_WritedataArrayLocationTag
END INTERFACE

!----------------------------------------------------------------------------
!                                         WritedataArrayTag@dataArrayMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 Sept 2021
! summary: WritedataArrayTag

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArrayTag(obj, dataType, name, &
    & numberOfComponents, content, isTuples, isOffset)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: dataType
    TYPE(String), INTENT(IN) :: name
    INTEGER(I4B), INTENT(IN) :: numberOfComponents
    TYPE(String), OPTIONAL, INTENT(IN) :: content
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isOffset
  END SUBROUTINE VTKFile_WritedataArrayTag
END INTERFACE

!----------------------------------------------------------------------------
!                                           WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: This routine writes Rank1 array of Real32
!
!# Introduction
!
! This routine writes Rank 1 of Real32. This routine calls `WritedataArrayTag`

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_Rank1_Real32(obj, name, x, &
    & isTuples, numberOfComponents)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    REAL(REAL32), INTENT(IN) :: x(1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberOfComponents
    !! number of components in the data, default is 1
  END SUBROUTINE VTKFile_WritedataArray_Rank1_Real32
END INTERFACE

!----------------------------------------------------------------------------
!                                             WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: This routine writes Rank1 array of Real64

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_Rank1_Real64(obj, name, x, &
    & isTuples, numberOfComponents)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    REAL(REAL64), INTENT(IN) :: x(1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberOfComponents
  END SUBROUTINE VTKFile_WritedataArray_Rank1_Real64
END INTERFACE

!----------------------------------------------------------------------------
!                                             WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: This routine writes Rank1 array of Int8

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_Rank1_Int8(obj, name, x, &
    & isTuples, numberOfComponents)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    INTEGER(INT8), INTENT(IN) :: x(1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberOfComponents
  END SUBROUTINE VTKFile_WritedataArray_Rank1_Int8
END INTERFACE

!----------------------------------------------------------------------------
!                                            WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: This routine writes Rank1 array of Int16

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_Rank1_Int16(obj, name, x, &
    & isTuples, numberOfComponents)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    INTEGER(INT16), INTENT(IN) :: x(1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberOfComponents
  END SUBROUTINE VTKFile_WritedataArray_Rank1_Int16
END INTERFACE

!----------------------------------------------------------------------------
!                                             WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_Rank1_Int32(obj, name, x, &
    & isTuples, numberOfComponents)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    INTEGER(INT32), INTENT(IN) :: x(1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberOfComponents
  END SUBROUTINE VTKFile_WritedataArray_Rank1_Int32
END INTERFACE

!----------------------------------------------------------------------------
!                                            WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_Rank1_Int64(obj, name, x, &
    & isTuples, numberOfComponents)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    INTEGER(INT64), INTENT(IN) :: x(1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberOfComponents
  END SUBROUTINE VTKFile_WritedataArray_Rank1_Int64
END INTERFACE

!----------------------------------------------------------------------------
!                                             WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_Rank2_Real32(obj, name, x, &
    & isTuples, numberOfComponents)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    REAL(REAL32), INTENT(IN) :: x(1:, 1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberOfComponents
  END SUBROUTINE VTKFile_WritedataArray_Rank2_Real32
END INTERFACE

!----------------------------------------------------------------------------
!                                             WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_Rank2_Real64(obj, name, x, &
    & isTuples, numberOfComponents)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    REAL(REAL64), INTENT(IN) :: x(1:, 1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberOfComponents
  END SUBROUTINE VTKFile_WritedataArray_Rank2_Real64
END INTERFACE

!----------------------------------------------------------------------------
!                                             WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_Rank2_Int8(obj, name, x, &
    & isTuples, numberOfComponents)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    INTEGER(INT8), INTENT(IN) :: x(1:, 1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberOfComponents
  END SUBROUTINE VTKFile_WritedataArray_Rank2_Int8
END INTERFACE

!----------------------------------------------------------------------------
!                                            WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_Rank2_Int16(obj, name, x, &
    & isTuples, numberOfComponents)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    INTEGER(INT16), INTENT(IN) :: x(1:, 1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberOfComponents
  END SUBROUTINE VTKFile_WritedataArray_Rank2_Int16
END INTERFACE

!----------------------------------------------------------------------------
!                                             WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_Rank2_Int32(obj, name, x, &
    & isTuples, numberOfComponents)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    INTEGER(INT32), INTENT(IN) :: x(1:, 1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberOfComponents
  END SUBROUTINE VTKFile_WritedataArray_Rank2_Int32
END INTERFACE

!----------------------------------------------------------------------------
!                                            WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_Rank2_Int64(obj, name, x, &
    & isTuples, numberOfComponents)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    INTEGER(INT64), INTENT(IN) :: x(1:, 1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberOfComponents
  END SUBROUTINE VTKFile_WritedataArray_Rank2_Int64
END INTERFACE

!----------------------------------------------------------------------------
!                                             WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_Rank3_Real32(obj, name, x, &
    & isTuples, numberOfComponents)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    REAL(REAL32), INTENT(IN) :: x(1:, 1:, 1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberOfComponents
  END SUBROUTINE VTKFile_WritedataArray_Rank3_Real32
END INTERFACE

!----------------------------------------------------------------------------
!                                             WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_Rank3_Real64(obj, name, x, &
    & isTuples, numberOfComponents)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    REAL(REAL64), INTENT(IN) :: x(1:, 1:, 1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberOfComponents
  END SUBROUTINE VTKFile_WritedataArray_Rank3_Real64
END INTERFACE

!----------------------------------------------------------------------------
!                                             WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_Rank3_Int8(obj, name, x, &
    & isTuples, numberOfComponents)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    INTEGER(INT8), INTENT(IN) :: x(1:, 1:, 1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberOfComponents
  END SUBROUTINE VTKFile_WritedataArray_Rank3_Int8
END INTERFACE

!----------------------------------------------------------------------------
!                                            WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_Rank3_Int16(obj, name, x, &
    & isTuples, numberOfComponents)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    INTEGER(INT16), INTENT(IN) :: x(1:, 1:, 1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberOfComponents
  END SUBROUTINE VTKFile_WritedataArray_Rank3_Int16
END INTERFACE

!----------------------------------------------------------------------------
!                                             WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_Rank3_Int32(obj, name, x, &
    & isTuples, numberOfComponents)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    INTEGER(INT32), INTENT(IN) :: x(1:, 1:, 1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberOfComponents
  END SUBROUTINE VTKFile_WritedataArray_Rank3_Int32
END INTERFACE

!----------------------------------------------------------------------------
!                                            WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_Rank3_Int64(obj, name, x, &
    & isTuples, numberOfComponents)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    INTEGER(INT64), INTENT(IN) :: x(1:, 1:, 1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberOfComponents
  END SUBROUTINE VTKFile_WritedataArray_Rank3_Int64
END INTERFACE

!----------------------------------------------------------------------------
!                                             WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_Rank4_Real32(obj, name, x, &
    & isTuples, numberOfComponents)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    REAL(REAL32), INTENT(IN) :: x(1:, 1:, 1:, 1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberOfComponents
  END SUBROUTINE VTKFile_WritedataArray_Rank4_Real32
END INTERFACE

!----------------------------------------------------------------------------
!                                             WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_Rank4_Real64(obj, name, x, &
    & isTuples, numberOfComponents)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    REAL(REAL64), INTENT(IN) :: x(1:, 1:, 1:, 1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberOfComponents
  END SUBROUTINE VTKFile_WritedataArray_Rank4_Real64
END INTERFACE

!----------------------------------------------------------------------------
!                                             WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_Rank4_Int8(obj, name, x, &
    & isTuples, numberOfComponents)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    INTEGER(INT8), INTENT(IN) :: x(1:, 1:, 1:, 1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberOfComponents
  END SUBROUTINE VTKFile_WritedataArray_Rank4_Int8
END INTERFACE

!----------------------------------------------------------------------------
!                                            WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_Rank4_Int16(obj, name, x, &
    & isTuples, numberOfComponents)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    INTEGER(INT16), INTENT(IN) :: x(1:, 1:, 1:, 1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberOfComponents
  END SUBROUTINE VTKFile_WritedataArray_Rank4_Int16
END INTERFACE

!----------------------------------------------------------------------------
!                                             WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_Rank4_Int32(obj, name, x, &
    & isTuples, numberOfComponents)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    INTEGER(INT32), INTENT(IN) :: x(1:, 1:, 1:, 1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberOfComponents
  END SUBROUTINE VTKFile_WritedataArray_Rank4_Int32
END INTERFACE

!----------------------------------------------------------------------------
!                                            WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_Rank4_Int64(obj, name, x, &
    & isTuples, numberOfComponents)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    INTEGER(INT64), INTENT(IN) :: x(1:, 1:, 1:, 1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberOfComponents
  END SUBROUTINE VTKFile_WritedataArray_Rank4_Int64
END INTERFACE

!----------------------------------------------------------------------------
!                                            WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_XYZ_Rank1_Real32(obj, name, x, &
    & y, z, isTuples)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    REAL(REAL32), INTENT(IN) :: x(1:)
    REAL(REAL32), INTENT(IN) :: y(1:)
    REAL(REAL32), INTENT(IN) :: z(1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
  END SUBROUTINE VTKFile_WritedataArray_XYZ_Rank1_Real32
END INTERFACE

!----------------------------------------------------------------------------
!                                            WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_XYZ_Rank1_Real64(obj, name, x, &
    & y, z, isTuples)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    REAL(REAL64), INTENT(IN) :: x(1:)
    REAL(REAL64), INTENT(IN) :: y(1:)
    REAL(REAL64), INTENT(IN) :: z(1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
  END SUBROUTINE VTKFile_WritedataArray_XYZ_Rank1_Real64
END INTERFACE

!----------------------------------------------------------------------------
!                                            WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_XYZ_Rank1_Int8(obj, name, x, &
    & y, z, isTuples)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    INTEGER(INT8), INTENT(IN) :: x(1:)
    INTEGER(INT8), INTENT(IN) :: y(1:)
    INTEGER(INT8), INTENT(IN) :: z(1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
  END SUBROUTINE VTKFile_WritedataArray_XYZ_Rank1_Int8
END INTERFACE

!----------------------------------------------------------------------------
!                                            WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_XYZ_Rank1_Int16(obj, name, x, &
    & y, z, isTuples)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    INTEGER(INT16), INTENT(IN) :: x(1:)
    INTEGER(INT16), INTENT(IN) :: y(1:)
    INTEGER(INT16), INTENT(IN) :: z(1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
  END SUBROUTINE VTKFile_WritedataArray_XYZ_Rank1_Int16
END INTERFACE

!----------------------------------------------------------------------------
!                                            WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_XYZ_Rank1_Int32(obj, name, x, &
    & y, z, isTuples)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    INTEGER(INT32), INTENT(IN) :: x(1:)
    INTEGER(INT32), INTENT(IN) :: y(1:)
    INTEGER(INT32), INTENT(IN) :: z(1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
  END SUBROUTINE VTKFile_WritedataArray_XYZ_Rank1_Int32
END INTERFACE

!----------------------------------------------------------------------------
!                                            WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_XYZ_Rank1_Int64(obj, name, x, &
    & y, z, isTuples)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    INTEGER(INT64), INTENT(IN) :: x(1:)
    INTEGER(INT64), INTENT(IN) :: y(1:)
    INTEGER(INT64), INTENT(IN) :: z(1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
  END SUBROUTINE VTKFile_WritedataArray_XYZ_Rank1_Int64
END INTERFACE

!----------------------------------------------------------------------------
!                                            WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_XYZ_Rank2_Real32(obj, name, x, &
    & y, z, isTuples)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    REAL(REAL32), INTENT(IN) :: x(1:, 1:)
    REAL(REAL32), INTENT(IN) :: y(1:, 1:)
    REAL(REAL32), INTENT(IN) :: z(1:, 1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
  END SUBROUTINE VTKFile_WritedataArray_XYZ_Rank2_Real32
END INTERFACE

!----------------------------------------------------------------------------
!                                            WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_XYZ_Rank2_Real64(obj, name, x, &
    & y, z, isTuples)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    REAL(REAL64), INTENT(IN) :: x(1:, 1:)
    REAL(REAL64), INTENT(IN) :: y(1:, 1:)
    REAL(REAL64), INTENT(IN) :: z(1:, 1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
  END SUBROUTINE VTKFile_WritedataArray_XYZ_Rank2_Real64
END INTERFACE

!----------------------------------------------------------------------------
!                                            WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_XYZ_Rank2_Int8(obj, name, x, &
    & y, z, isTuples)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    INTEGER(INT8), INTENT(IN) :: x(1:, 1:)
    INTEGER(INT8), INTENT(IN) :: y(1:, 1:)
    INTEGER(INT8), INTENT(IN) :: z(1:, 1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
  END SUBROUTINE VTKFile_WritedataArray_XYZ_Rank2_Int8
END INTERFACE

!----------------------------------------------------------------------------
!                                            WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_XYZ_Rank2_Int16(obj, name, x, &
    & y, z, isTuples)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    INTEGER(INT16), INTENT(IN) :: x(1:, 1:)
    INTEGER(INT16), INTENT(IN) :: y(1:, 1:)
    INTEGER(INT16), INTENT(IN) :: z(1:, 1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
  END SUBROUTINE VTKFile_WritedataArray_XYZ_Rank2_Int16
END INTERFACE

!----------------------------------------------------------------------------
!                                            WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_XYZ_Rank2_Int32(obj, name, x, &
    & y, z, isTuples)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    INTEGER(INT32), INTENT(IN) :: x(1:, 1:)
    INTEGER(INT32), INTENT(IN) :: y(1:, 1:)
    INTEGER(INT32), INTENT(IN) :: z(1:, 1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
  END SUBROUTINE VTKFile_WritedataArray_XYZ_Rank2_Int32
END INTERFACE

!----------------------------------------------------------------------------
!                                            WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_XYZ_Rank2_Int64(obj, name, x, &
    & y, z, isTuples)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    INTEGER(INT64), INTENT(IN) :: x(1:, 1:)
    INTEGER(INT64), INTENT(IN) :: y(1:, 1:)
    INTEGER(INT64), INTENT(IN) :: z(1:, 1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
  END SUBROUTINE VTKFile_WritedataArray_XYZ_Rank2_Int64
END INTERFACE

!----------------------------------------------------------------------------
!                                            WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_XYZ_Rank3_Real32(obj, name, x, &
    & y, z, isTuples)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    REAL(REAL32), INTENT(IN) :: x(1:, 1:, 1:)
    REAL(REAL32), INTENT(IN) :: y(1:, 1:, 1:)
    REAL(REAL32), INTENT(IN) :: z(1:, 1:, 1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
  END SUBROUTINE VTKFile_WritedataArray_XYZ_Rank3_Real32
END INTERFACE

!----------------------------------------------------------------------------
!                                            WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_XYZ_Rank3_Real64(obj, name, x, &
    & y, z, isTuples)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    REAL(REAL64), INTENT(IN) :: x(1:, 1:, 1:)
    REAL(REAL64), INTENT(IN) :: y(1:, 1:, 1:)
    REAL(REAL64), INTENT(IN) :: z(1:, 1:, 1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
  END SUBROUTINE VTKFile_WritedataArray_XYZ_Rank3_Real64
END INTERFACE

!----------------------------------------------------------------------------
!                                            WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_XYZ_Rank3_Int8(obj, name, x, &
    & y, z, isTuples)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    INTEGER(INT8), INTENT(IN) :: x(1:, 1:, 1:)
    INTEGER(INT8), INTENT(IN) :: y(1:, 1:, 1:)
    INTEGER(INT8), INTENT(IN) :: z(1:, 1:, 1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
  END SUBROUTINE VTKFile_WritedataArray_XYZ_Rank3_Int8
END INTERFACE

!----------------------------------------------------------------------------
!                                            WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_XYZ_Rank3_Int16(obj, name, x, &
    & y, z, isTuples)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    INTEGER(INT16), INTENT(IN) :: x(1:, 1:, 1:)
    INTEGER(INT16), INTENT(IN) :: y(1:, 1:, 1:)
    INTEGER(INT16), INTENT(IN) :: z(1:, 1:, 1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
  END SUBROUTINE VTKFile_WritedataArray_XYZ_Rank3_Int16
END INTERFACE

!----------------------------------------------------------------------------
!                                            WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_XYZ_Rank3_Int32(obj, name, x, &
    & y, z, isTuples)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    INTEGER(INT32), INTENT(IN) :: x(1:, 1:, 1:)
    INTEGER(INT32), INTENT(IN) :: y(1:, 1:, 1:)
    INTEGER(INT32), INTENT(IN) :: z(1:, 1:, 1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
  END SUBROUTINE VTKFile_WritedataArray_XYZ_Rank3_Int32
END INTERFACE

!----------------------------------------------------------------------------
!                                            WritedataArray@dataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_XYZ_Rank3_Int64(obj, name, x, &
    & y, z, isTuples)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    INTEGER(INT64), INTENT(IN) :: x(1:, 1:, 1:)
    INTEGER(INT64), INTENT(IN) :: y(1:, 1:, 1:)
    INTEGER(INT64), INTENT(IN) :: z(1:, 1:, 1:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTuples
  END SUBROUTINE VTKFile_WritedataArray_XYZ_Rank3_Int64
END INTERFACE

!----------------------------------------------------------------------------
!                                    WritedataArray@dataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WritedataArray_Appended(obj)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
  END SUBROUTINE VTKFile_WritedataArray_Appended
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@dataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WriteToScratch1(obj, x)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    CLASS(*), INTENT(IN) :: x(1:)
  END SUBROUTINE VTKFile_WriteToScratch1
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@dataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WriteToScratch2(obj, x)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    CLASS(*), INTENT(IN) :: x(1:, 1:)
  END SUBROUTINE VTKFile_WriteToScratch2
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@dataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WriteToScratch3(obj, x)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    CLASS(*), INTENT(IN) :: x(1:, 1:, 1:)
  END SUBROUTINE VTKFile_WriteToScratch3
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@dataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WriteToScratch4(obj, x)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    CLASS(*), INTENT(IN) :: x(1:, 1:, 1:, 1:)
  END SUBROUTINE VTKFile_WriteToScratch4
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@dataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WriteToScratch5(obj, x, y, z)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    REAL(REAL64), INTENT(IN) :: x(1:)
    REAL(REAL64), INTENT(IN) :: y(1:)
    REAL(REAL64), INTENT(IN) :: z(1:)
  END SUBROUTINE VTKFile_WriteToScratch5
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@dataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WriteToScratch6(obj, x, y, z)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    REAL(REAL32), INTENT(IN) :: x(1:)
    REAL(REAL32), INTENT(IN) :: y(1:)
    REAL(REAL32), INTENT(IN) :: z(1:)
  END SUBROUTINE VTKFile_WriteToScratch6
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@dataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WriteToScratch7(obj, x, y, z)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    INTEGER(INT64), INTENT(IN) :: x(1:)
    INTEGER(INT64), INTENT(IN) :: y(1:)
    INTEGER(INT64), INTENT(IN) :: z(1:)
  END SUBROUTINE VTKFile_WriteToScratch7
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@dataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WriteToScratch8(obj, x, y, z)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    INTEGER(INT32), INTENT(IN) :: x(1:)
    INTEGER(INT32), INTENT(IN) :: y(1:)
    INTEGER(INT32), INTENT(IN) :: z(1:)
  END SUBROUTINE VTKFile_WriteToScratch8
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@dataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WriteToScratch9(obj, x, y, z)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    INTEGER(INT16), INTENT(IN) :: x(1:)
    INTEGER(INT16), INTENT(IN) :: y(1:)
    INTEGER(INT16), INTENT(IN) :: z(1:)
  END SUBROUTINE VTKFile_WriteToScratch9
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@dataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WriteToScratch10(obj, x, y, z)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    INTEGER(INT8), INTENT(IN) :: x(1:)
    INTEGER(INT8), INTENT(IN) :: y(1:)
    INTEGER(INT8), INTENT(IN) :: z(1:)
  END SUBROUTINE VTKFile_WriteToScratch10
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@dataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WriteToScratch11(obj, x, y, z)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    REAL(REAL64), INTENT(IN) :: x(1:, 1:)
    REAL(REAL64), INTENT(IN) :: y(1:, 1:)
    REAL(REAL64), INTENT(IN) :: z(1:, 1:)
  END SUBROUTINE VTKFile_WriteToScratch11
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@dataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WriteToScratch12(obj, x, y, z)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    REAL(REAL32), INTENT(IN) :: x(1:, 1:)
    REAL(REAL32), INTENT(IN) :: y(1:, 1:)
    REAL(REAL32), INTENT(IN) :: z(1:, 1:)
  END SUBROUTINE VTKFile_WriteToScratch12
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@dataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WriteToScratch13(obj, x, y, z)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    INTEGER(INT64), INTENT(IN) :: x(1:, 1:)
    INTEGER(INT64), INTENT(IN) :: y(1:, 1:)
    INTEGER(INT64), INTENT(IN) :: z(1:, 1:)
  END SUBROUTINE VTKFile_WriteToScratch13
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@dataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WriteToScratch14(obj, x, y, z)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    INTEGER(INT32), INTENT(IN) :: x(1:, 1:)
    INTEGER(INT32), INTENT(IN) :: y(1:, 1:)
    INTEGER(INT32), INTENT(IN) :: z(1:, 1:)
  END SUBROUTINE VTKFile_WriteToScratch14
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@dataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WriteToScratch15(obj, x, y, z)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    INTEGER(INT16), INTENT(IN) :: x(1:, 1:)
    INTEGER(INT16), INTENT(IN) :: y(1:, 1:)
    INTEGER(INT16), INTENT(IN) :: z(1:, 1:)
  END SUBROUTINE VTKFile_WriteToScratch15
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@dataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WriteToScratch16(obj, x, y, z)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    INTEGER(INT8), INTENT(IN) :: x(1:, 1:)
    INTEGER(INT8), INTENT(IN) :: y(1:, 1:)
    INTEGER(INT8), INTENT(IN) :: z(1:, 1:)
  END SUBROUTINE VTKFile_WriteToScratch16
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@dataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WriteToScratch17(obj, x, y, z)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    REAL(REAL64), INTENT(IN) :: x(1:, 1:, 1:)
    REAL(REAL64), INTENT(IN) :: y(1:, 1:, 1:)
    REAL(REAL64), INTENT(IN) :: z(1:, 1:, 1:)
  END SUBROUTINE VTKFile_WriteToScratch17
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@dataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WriteToScratch18(obj, x, y, z)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    REAL(REAL32), INTENT(IN) :: x(1:, 1:, 1:)
    REAL(REAL32), INTENT(IN) :: y(1:, 1:, 1:)
    REAL(REAL32), INTENT(IN) :: z(1:, 1:, 1:)
  END SUBROUTINE VTKFile_WriteToScratch18
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@dataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WriteToScratch19(obj, x, y, z)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    INTEGER(INT64), INTENT(IN) :: x(1:, 1:, 1:)
    INTEGER(INT64), INTENT(IN) :: y(1:, 1:, 1:)
    INTEGER(INT64), INTENT(IN) :: z(1:, 1:, 1:)
  END SUBROUTINE VTKFile_WriteToScratch19
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@dataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WriteToScratch20(obj, x, y, z)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    INTEGER(INT32), INTENT(IN) :: x(1:, 1:, 1:)
    INTEGER(INT32), INTENT(IN) :: y(1:, 1:, 1:)
    INTEGER(INT32), INTENT(IN) :: z(1:, 1:, 1:)
  END SUBROUTINE VTKFile_WriteToScratch20
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@dataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WriteToScratch21(obj, x, y, z)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    INTEGER(INT16), INTENT(IN) :: x(1:, 1:, 1:)
    INTEGER(INT16), INTENT(IN) :: y(1:, 1:, 1:)
    INTEGER(INT16), INTENT(IN) :: z(1:, 1:, 1:)
  END SUBROUTINE VTKFile_WriteToScratch21
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@dataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE VTKFile_WriteToScratch22(obj, x, y, z)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    INTEGER(INT8), INTENT(IN) :: x(1:, 1:, 1:)
    INTEGER(INT8), INTENT(IN) :: y(1:, 1:, 1:)
    INTEGER(INT8), INTENT(IN) :: z(1:, 1:, 1:)
  END SUBROUTINE VTKFile_WriteToScratch22
END INTERFACE

!----------------------------------------------------------------------------
!                                          WriteFielddata@FielddataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: Write Field data

INTERFACE
  MODULE SUBROUTINE VTKFile_WriteFielddata_1(obj, name, x)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    CLASS(*), INTENT(IN) :: x
  END SUBROUTINE VTKFile_WriteFielddata_1
END INTERFACE

!----------------------------------------------------------------------------
!                                          WriteFielddata@FielddataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: Write Field data

INTERFACE
  MODULE SUBROUTINE VTKFile_WriteFielddata_2(obj, action)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: action
    !! Open, Close
  END SUBROUTINE VTKFile_WriteFielddata_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                   WritePiece@PieceMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: WritePiece

INTERFACE
  MODULE SUBROUTINE VTKFile_WritePiece_1(obj, extent, srcFileName)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: extent(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: srcFileName
  END SUBROUTINE VTKFile_WritePiece_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                   WritePiece@PieceMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: Write Piece

INTERFACE
  MODULE SUBROUTINE VTKFile_WritePiece_2(obj, nPoints, nCells)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nPoints
    INTEGER(I4B), INTENT(IN) :: nCells
  END SUBROUTINE VTKFile_WritePiece_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                   WritePiece@PieceMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: Write Piece

INTERFACE
  MODULE SUBROUTINE VTKFile_WritePiece_3(obj, nPoints, nVerts, nLines, &
    & nStrips, nPolys)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nPoints
    INTEGER(I4B), INTENT(IN) :: nVerts
    INTEGER(I4B), INTENT(IN) :: nLines
    INTEGER(I4B), INTENT(IN) :: nStrips
    INTEGER(I4B), INTENT(IN) :: nPolys
  END SUBROUTINE VTKFile_WritePiece_3
END INTERFACE

!----------------------------------------------------------------------------
!                                                   WritePiece@PieceMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: Write piece

INTERFACE
  MODULE SUBROUTINE VTKFile_WritePiece_4(obj)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
  END SUBROUTINE VTKFile_WritePiece_4
END INTERFACE

!----------------------------------------------------------------------------
!                                                WritePoints@PointsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: Write Points

INTERFACE
  MODULE SUBROUTINE VTKFile_WritePoints_1(obj, x, y, z)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    REAL(REAL64), INTENT(IN) :: x(:)
    REAL(REAL64), INTENT(IN) :: y(:)
    REAL(REAL64), INTENT(IN) :: z(:)
  END SUBROUTINE VTKFile_WritePoints_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                WritePoints@PointsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: Write points

INTERFACE
  MODULE SUBROUTINE VTKFile_WritePoints_2(obj, x, y, z)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    REAL(REAL32), INTENT(IN) :: x(:)
    REAL(REAL32), INTENT(IN) :: y(:)
    REAL(REAL32), INTENT(IN) :: z(:)
  END SUBROUTINE VTKFile_WritePoints_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                WritePoints@PointsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: Write points

INTERFACE
  MODULE SUBROUTINE VTKFile_WritePoints_3(obj, x)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    REAL(REAL64), INTENT(IN) :: x(1:, 1:)
  END SUBROUTINE VTKFile_WritePoints_3
END INTERFACE

!----------------------------------------------------------------------------
!                                                WritePoints@PointsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: Write points

INTERFACE
  MODULE SUBROUTINE VTKFile_WritePoints_4(obj, x)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    REAL(REAL32), INTENT(IN) :: x(1:, 1:)
  END SUBROUTINE VTKFile_WritePoints_4
END INTERFACE

!----------------------------------------------------------------------------
!                                                WritePoints@PointsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: Write points

INTERFACE
  MODULE SUBROUTINE VTKFile_WritePoints_5(obj, x, y, z)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    REAL(REAL64), INTENT(IN) :: x(:, :, :)
    REAL(REAL64), INTENT(IN) :: y(:, :, :)
    REAL(REAL64), INTENT(IN) :: z(:, :, :)
  END SUBROUTINE VTKFile_WritePoints_5
END INTERFACE

!----------------------------------------------------------------------------
!                                                WritePoints@PointsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: Write points

INTERFACE
  MODULE SUBROUTINE VTKFile_WritePoints_6(obj, x, y, z)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    REAL(REAL32), INTENT(IN) :: x(:, :, :)
    REAL(REAL32), INTENT(IN) :: y(:, :, :)
    REAL(REAL32), INTENT(IN) :: z(:, :, :)
  END SUBROUTINE VTKFile_WritePoints_6
END INTERFACE

!----------------------------------------------------------------------------
!                                                WritePoints@PointsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: Write points

INTERFACE
  MODULE SUBROUTINE VTKFile_WritePoints_7(obj, x)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    REAL(REAL64), INTENT(IN) :: x(:, :, :, :)
  END SUBROUTINE VTKFile_WritePoints_7
END INTERFACE

!----------------------------------------------------------------------------
!                                                WritePoints@PointsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: Write points

INTERFACE
  MODULE SUBROUTINE VTKFile_WritePoints_8(obj, x)
    CLASS(VTKFile_), INTENT(INOUT) :: obj
    REAL(REAL32), INTENT(IN) :: x(:, :, :, :)
  END SUBROUTINE VTKFile_WritePoints_8
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE VTKFile_Class
