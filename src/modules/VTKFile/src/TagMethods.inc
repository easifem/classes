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
! <VTKFile type="'//trim(obj%DataStructureName)// &
! & '" version="1.0" byte_order="LittleEndian">
!```

INTERFACE
MODULE SUBROUTINE VTKFile_WriteRootTag( obj )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
END SUBROUTINE VTKFile_WriteRootTag
END INTERFACE

!----------------------------------------------------------------------------
!                                           WriteDataStructureTag@TagsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 7 July 2022
! summary: Write the data structure tags

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataStructureTag( obj, meshDataFormat )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: meshDataFormat
    !! Float32, Float64
    !! It should be present for parallel data structure
    !! PARALLEL_VTK_RECTILINEARGRID, PARALLEL_VTK_STRUCTUREDGRID,
    !! PARALLEL_VTK_UNSTRUCTUREDGRID
END SUBROUTINE VTKFile_WriteDataStructureTag
END INTERFACE

!----------------------------------------------------------------------------
!                                                  WriteStartTag@TagsMethods
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
!                                                    WriteEndTag@TagsMethods
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
!                                            WriteSelfClosingTag@TagsMethods
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
!                                                       WriteTag@TagsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 Sept 2021
! summary: Write Tags and contents

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
