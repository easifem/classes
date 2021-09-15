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
  & "ImageData               ", &
  & "RectilinearGrid         ", &
  & "StructuredGrid          ", &
  & "PolyData                ", &
  & "UnstructuredGrid        ", &
  & "PImageData       ", &
  & "PRectilinearGrid ", &
  & "PStructuredGrid  ", &
  & "PPolyData        ", &
  & "ParallelUnstructuredGrid" &
  & ]

INTEGER( I4B ), PARAMETER, PUBLIC :: VTK_ASCII = 1
INTEGER( I4B ), PARAMETER, PUBLIC :: VTK_BINARY = 2
INTEGER( I4B ), PARAMETER, PUBLIC :: VTK_APPENDED = 3

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
  CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS( obj ) :: addSurrogate => VTKFile_addSurrogate
  PROCEDURE, PUBLIC, PASS( obj ) :: DeallocateData => VTKFile_DeallocateData
  FINAL :: VTKFile_Final
  PROCEDURE, PUBLIC, PASS( obj ) :: WriteRootTag => VTKFile_WriteRootTag
  PROCEDURE, PUBLIC, PASS( obj ) :: WriteDataStructureTag => VTKFile_WriteDataStructureTag


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

END MODULE VTKFile_Class