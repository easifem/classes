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
! It also write header file and DataStructure tag to the file.
!
!## Arguments:
!
! *filename*: is the name of the file
!
! *mode* : can be `READ`, `WRITE`, `NEW`, `OVERWRITE`. It is like action.
!
! *DataFormat* : defines the format of DataArray in VTKFile, it can be one fo
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
! *DataStructureType* : The type of vtk file. It can have following values
!
! - ImageData,
! - RectilinearGrid,
! - StructuredGrid,
! - PolyData,
! - UnstructuredGrid
! - PImageData
! - PRectilinearGrid
! - PStructuredGrid
! - PPolyData
! - PUnstructuredGrid
!
! *WholeExtent* : is [x1, x2, y1, y2, z1, z2], it is required for structured
! data set.
!
! *isVolatile* : It is True if volatileBuffer is used to keep the xml file
!
! *meshDataFormat* It can be Float32, Float64

INTERFACE
MODULE SUBROUTINE InitiateVTKFile( obj, filename, mode, DataFormat, &
  & DataStructureType, WholeExtent, isVolatile, meshDataFormat, &
  & Spacing, Origin )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: filename
    !! Name of the file, where xml data will be printed
  CHARACTER( LEN = * ), INTENT( IN ) :: mode
    !! READ, WRITE, NEW, REPLACE
  INTEGER( I4B ), INTENT( IN ) :: DataFormat
    !! VTK_ASCII, VTK_APPENEDED, VTK_BINARY
  INTEGER( I4B ), INTENT( IN ) :: DataStructureType
    !! VTK_IMAGEDATA, VTK_STRUCTUREDGRID, etc.
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: WholeExtent( : )
    !! Required for structured data set
    !! [x1, x2, y1, y2, z1, z2]
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isVolatile
    !! True if volatileBuffer is used to keep the xml file
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: meshDataFormat
    !! Float32, Float64, etc
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: Spacing( : )
    !! dx, dy, dz, needed in case of ImageData
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: Origin( : )
    !! x0, y0, z0, needed in case of ImageData
END SUBROUTINE InitiateVTKFile
END INTERFACE

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
!                                         Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 Sept 2021
! summary: Deallocates the content of VTKFile and close or delete it.

INTERFACE
MODULE SUBROUTINE VTKFile_Deallocate( obj, delete )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: delete
END SUBROUTINE VTKFile_Deallocate
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

!> authors: Vikas Sharma, Ph. D.
! date: 7 July 2022
! summary: Update the offset in VTKFile
!
!# Introduction
!
! If the Dataformat is VTK_APPENDED then update the offset by number of
! bytes `nByte`

INTERFACE
MODULE ELEMENTAL SUBROUTINE VTKFile_UpdateOffset( obj, nByte )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nByte
END SUBROUTINE VTKFile_UpdateOffset
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Close@ConstructorMethods
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
!                                         OpenScratchFile@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 7 July 2022
! summary: Open the scratch file if data format is `VTK_APPENDED`

INTERFACE
MODULE SUBROUTINE VTKFile_OpenScratchFile( obj )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
END SUBROUTINE VTKFile_OpenScratchFile
END INTERFACE

!----------------------------------------------------------------------------
!                                       CloseScratchFile@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 7 July 2022
! summary: Close the scratch file

INTERFACE
MODULE SUBROUTINE VTKFile_CloseScratchFile( obj )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
END SUBROUTINE VTKFile_CloseScratchFile
END INTERFACE
