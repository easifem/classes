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

!> authors: Vikas Sharma, Ph. D.
! date: 	9 June 2021
! summary: This module contains routines to handle $MeshFormat section of msh4 file type
!
!### Introduction
! 	Gmsh's msh4 file format contains mesh format in $MeshFormat section. The class mshFormat_ handles this section. An example is given below.
!
! ```
! $MeshFormat
! 4.1 0 8
! $EndMeshFormat
! ```
!
! Here, 4.1 is the majorVERSION.minorVERSION, 0 denotes ASCII, 8 denotes the data size.

MODULE mshFormat_Class
USE BaseType
USE GlobalData
USE FortranFile_Class
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                                 mshFormat_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 June 2021
! summary: 	`mshFormat_` data type handles the mesh format of msh4 file format

TYPE :: mshFormat_
  PRIVATE
  REAL( DFP ) :: Version = 0.0_DFP
  INTEGER( I4B ) :: MajorVersion = 4_I4B
  INTEGER( I4B ) :: MinorVersion = 1_I4B
  INTEGER( I4B ) :: FileType = 0, DataSize = 0
  LOGICAL( LGT ) :: isASCII = .FALSE.
  CHARACTER( LEN = 10 ) :: MeshFormat = ""
  CONTAINS
    PRIVATE
    PROCEDURE, PUBLIC, PASS( Obj ) :: Display => fmt_Display
      !! Display the content
    PROCEDURE, PUBLIC, PASS( Obj ) :: getVersion => fmt_getVersion
      !! Returns the version
    PROCEDURE, PUBLIC, PASS( Obj ) :: getMajorVersion => fmt_getMajorVersion
      !! Returns the major version
    PROCEDURE, PUBLIC, PASS( Obj ) :: getMinorVersion => fmt_getMinorVersion
      !! Returns the minor version
    PROCEDURE, PUBLIC, PASS( Obj ) :: getFileType => fmt_getFileType
      !! Returns the file type
    PROCEDURE, PUBLIC, PASS( Obj ) :: getDataSize => fmt_getDataSize
      !! Returns the Datasize
    PROCEDURE, PUBLIC, PASS( Obj ) :: getMeshFormat => fmt_getMeshFormat
      !! Return the Mesh format
    PROCEDURE, PUBLIC, PASS( obj ) :: ReadFromFile => fmt_ReadFromFile
      !! Read format from a file
    PROCEDURE, PUBLIC, PASS( obj ) :: WriteToFile => fmt_WriteToFile
      !! Write content to a file
    PROCEDURE, PUBLIC, PASS( obj ) :: GotoTag => fmt_GotoTag
      !! Goto a tag
    PROCEDURE, PUBLIC, PASS( obj ) :: Finalize => fmt_Finalize
      !! Finalize
END TYPE mshFormat_

PUBLIC :: mshFormat_
TYPE( mshFormat_ ), PUBLIC, PARAMETER :: TypemshFormat = mshFormat_( )

!----------------------------------------------------------------------------
!                                                           mshFormatPointer_
!----------------------------------------------------------------------------

TYPE :: mshFormatPointer_
  CLASS( mshFormat_ ), POINTER :: Ptr => NULL()
END TYPE mshFormatPointer_

PUBLIC :: mshFormatPointer_

!----------------------------------------------------------------------------
!                                                          Display@mshFormat
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 June 2021
! summary: This subroutine display the content of [[mshFormat_]]

INTERFACE
MODULE SUBROUTINE fmt_Display( obj, Msg, UnitNo )
  CLASS( mshFormat_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
END SUBROUTINE fmt_Display
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE fmt_Display
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                                 getVersion
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 June 2021
! summary: 	This routine returns the version

INTERFACE
MODULE PURE FUNCTION fmt_getVersion( obj ) RESULT( Ans )
  CLASS( mshFormat_ ), INTENT( IN ) :: obj
  REAL( DFP ) :: Ans
END FUNCTION fmt_getVersion
END INTERFACE

!----------------------------------------------------------------------------
!                                                           getMajorVersion
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 June 2021
! summary: 	This routine returns the Major Version

INTERFACE
MODULE PURE FUNCTION fmt_getMajorVersion( obj ) RESULT( Ans )
  CLASS( mshFormat_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: Ans
END FUNCTION fmt_getMajorVersion
END INTERFACE

!----------------------------------------------------------------------------
!                                                           getMinorVersion
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 June 2021
! summary: 	This routine returns the Minor major

INTERFACE
MODULE PURE FUNCTION fmt_getMinorVersion( obj ) RESULT( Ans )
  CLASS( mshFormat_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: Ans
END FUNCTION fmt_getMinorVersion
END INTERFACE

!----------------------------------------------------------------------------
!                                                              getFileType
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 June 2021
! summary: 	This routine returns the file type

INTERFACE
MODULE PURE FUNCTION fmt_getFileType( obj ) RESULT( Ans )
  CLASS( mshFormat_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = 6 ) :: Ans
END FUNCTION fmt_getFileType
END INTERFACE

!----------------------------------------------------------------------------
!                                                                getDataSize
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 June 2021
! summary: 	This routine returns the data size in Bytes

INTERFACE
MODULE PURE FUNCTION fmt_getDataSize( obj ) RESULT( Ans )
  CLASS( mshFormat_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: Ans
END FUNCTION fmt_getDataSize
END INTERFACE

!----------------------------------------------------------------------------
!                                                             getMeshFormat
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 June 2021
! summary: 	This routine returns the mesh format

INTERFACE
MODULE PURE FUNCTION fmt_getMeshFormat( obj ) RESULT( Ans )
  CLASS( mshFormat_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = 10 ) :: Ans
END FUNCTION fmt_getMeshFormat
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ReadFromFile@mshFormat
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 June 2021
! summary: 	Routine for reading mesh format from the msh file

INTERFACE
MODULE SUBROUTINE fmt_ReadFromFile( obj, mshFile, ierr )
  CLASS( mshFormat_ ), INTENT( INOUT) :: obj
  TYPE( FortranFile_ ), INTENT( INOUT) :: mshFile
  LOGICAL( LGT ), INTENT( INOUT ) :: ierr
END SUBROUTINE fmt_ReadFromFile
END INTERFACE

!----------------------------------------------------------------------------
!                                                      WriteToFile@mshFormat
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 June 2021
! summary: This subroutine writes mesh format to a .msh file

INTERFACE
MODULE SUBROUTINE fmt_WriteToFile( obj, mshFile, Str, EndStr )
  CLASS( mshFormat_ ), INTENT( INOUT ) :: obj
  TYPE( FortranFile_ ), INTENT( INOUT ) :: mshFile
  CHARACTER( LEN = * ), INTENT( IN ), OPTIONAL :: Str, EndStr
END SUBROUTINE fmt_WriteToFile
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GotoTag@mshFormat
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 June 2021
! summary: This subroutine search the mesh format tag in the mesh file

INTERFACE
MODULE SUBROUTINE fmt_GotoTag( obj, mshFile, ierr )
  CLASS( mshFormat_ ), INTENT( IN ) :: obj
  TYPE( FortranFile_ ), INTENT( INOUT ) :: mshFile
  LOGICAL( LGT ), INTENT( INOUT ) :: ierr
END SUBROUTINE fmt_GotoTag
END INTERFACE

!----------------------------------------------------------------------------
!                                                    DeallocateData@mshFormat
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 June 2021
! summary: 	This subroutine clears the content of [[mshFormat_]]

INTERFACE
MODULE SUBROUTINE fmt_Finalize( obj )
  CLASS( mshFormat_ ), INTENT( INOUT) :: obj
END SUBROUTINE fmt_Finalize
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE fmt_Finalize
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE mshFormat_Class