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

MODULE XMLFile_Class
USE GlobalData
USE BaseType
USE AbstractFile_Class
USE XMLTag_Class
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
IMPLICIT NONE
PRIVATE
!>
CHARACTER( LEN = * ), PARAMETER :: modName="XMLFILE_CLASS"
TYPE( ExceptionHandler_ ) :: e
PUBLIC :: xmlTag_

!----------------------------------------------------------------------------
!                                                              XMLFile_
!----------------------------------------------------------------------------

TYPE, EXTENDS( AbstractFile_ ) :: XMLFile_
  PRIVATE
  LOGICAL( LGT ), PUBLIC :: isInitiated = .FALSE.
    !! Logical indicating if file was initialized
  INTEGER( I4B ), PUBLIC :: unitNo=-1
    !! The unit number assigned to the file
  REAL( DFP ) :: version=1.0_DFP
    !! The XML version
  CHARACTER( LEN=32 ) :: encoding='UTF-8'
    !! The XML file encoding
  TYPE( String ) :: style_sheet
  LOGICAL( LGT ) :: standalone=.FALSE.
    !! The root XML element of the file
  LOGICAL( LGT ) :: newstat=.FALSE.
    !! The 'new' status of a file
  LOGICAL( LGT ) :: overwriteStat=.FALSE.
    !! When .TRUE., file data can be overwritten
  TYPE( String ) :: fullname
    !! full name of the file
  TYPE( XMLTag_ ), PUBLIC, POINTER :: root => NULL()
  CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS( obj ) :: addSurrogate => xmlFile_addSurrogate
  PROCEDURE, PUBLIC, PASS( obj ) :: Initiate => xmlFile_Initiate
  PROCEDURE, PUBLIC, PASS( obj ) :: DeallocateData => xmlFile_DeallocateData
  FINAL :: xmlFile_Final
  PROCEDURE, PUBLIC, PASS( obj ) :: Open => xmlFile_Open
  PROCEDURE, PUBLIC, PASS( obj ) :: Close => xmlFile_Close
  PROCEDURE, PUBLIC, PASS( obj ) :: Delete => xmlFile_Delete
  PROCEDURE, PUBLIC, PASS( obj ) :: Import => xmlFile_Import
  PROCEDURE, PUBLIC, PASS( obj ) :: Export => xmlFile_Export
  PROCEDURE, PUBLIC, PASS( Obj ) :: setNewStat => xmlFile_setNewStat
  PROCEDURE, PUBLIC, PASS( Obj ) :: isNew => xmlFile_isNew
  PROCEDURE, PUBLIC, PASS( Obj ) :: setOverwriteStat => xmlFile_setOverwriteStat
  PROCEDURE, PUBLIC, PASS( obj ) :: isOverwrite => xmlFile_isOverwrite
  PROCEDURE, PUBLIC, PASS( Obj ) :: isFormatted => xmlFile_isFormatted
  PROCEDURE, PUBLIC, PASS( obj ) :: parseXMLDeclaration => &
    & xmlFile_parseXMLDeclaration
  PROCEDURE, PUBLIC, PASS( obj ) :: BuildCache => xmlFile_BuildCache
  PROCEDURE, PUBLIC, PASS( obj ) :: Display => xmlFile_Display
END TYPE XMLFile_

PUBLIC :: XMLFile_

!----------------------------------------------------------------------------
!                                                        XMLFilePointer_
!----------------------------------------------------------------------------

TYPE :: XMLFilePointer_
  CLASS( XMLFile_ ), POINTER :: ptr => NULL()
END TYPE XMLFilePointer_

PUBLIC :: XMLFilePointer_

!----------------------------------------------------------------------------
!                                           addSurrogate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE xmlFile_addSurrogate( obj, userObj )
  CLASS( xmlFile_ ), INTENT( INOUT ) :: obj
  TYPE( ExceptionHandler_ ), INTENT( IN ) :: userObj
END SUBROUTINE xmlFile_addSurrogate
END INTERFACE

!----------------------------------------------------------------------------
!                                              Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 9 Sept 2021
! summary: This routine initiates xmlfile
!
!### Introduction
!
! This routine initiates the xmlFile_.
! - filename is full name of the file
! - mode can be READ, WRITE, NEW, OVERWRITE

INTERFACE
MODULE SUBROUTINE xmlFile_Initiate( obj, filename, mode )
  CLASS( xmlFile_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: filename
  CHARACTER( LEN = * ), INTENT( IN ) :: mode
    !! READ, WRITE, NEW, OVERWRITE
END SUBROUTINE xmlFile_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                        DeallocateData@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE xmlFile_DeallocateData( obj, delete )
  CLASS( xmlFile_ ), INTENT( INOUT ) :: obj
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: delete
END SUBROUTINE xmlFile_DeallocateData
END INTERFACE

PUBLIC :: xmlFile_DeallocateData

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE xmlFile_Final( obj )
  TYPE( xmlFile_ ), INTENT( INOUT ) :: obj
END SUBROUTINE xmlFile_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Open@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE xmlFile_Open( obj )
  CLASS( xmlFile_ ), INTENT( INOUT ) :: obj
END SUBROUTINE xmlFile_Open
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Close@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE xmlFile_Close( obj )
  CLASS( xmlFile_ ), INTENT( INOUT ) :: obj
END SUBROUTINE xmlFile_Close
END INTERFACE

PUBLIC :: XMLFile_Close

!----------------------------------------------------------------------------
!                                                   Delete@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE xmlFile_Delete( obj )
  CLASS( xmlFile_ ), INTENT( INOUT ) :: obj
END SUBROUTINE xmlFile_Delete
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE xmlFile_Export( obj, filename )
  CLASS( xmlFile_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: filename
END SUBROUTINE xmlFile_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE xmlFile_Import( obj, filename )
  CLASS( xmlFile_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: filename
END SUBROUTINE xmlFile_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                     setNewStat@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 June 2021
! summary: Sets whether or not the xml file is new.

INTERFACE
MODULE SUBROUTINE xmlFile_setNewStat( obj, bool )
  CLASS( XMLFile_ ), INTENT( INOUT ) :: obj
  LOGICAL( LGT ), INTENT( IN ) :: bool
END SUBROUTINE xmlFile_setNewStat
END INTERFACE

!----------------------------------------------------------------------------
!                                              setOverwriteStat@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 June 2021
! summary: 	Sets the value for the status of whether or not the file will be overwritable.

INTERFACE
MODULE SUBROUTINE xmlFile_setOverwriteStat( obj, bool )
  CLASS( XMLFile_ ), INTENT( INOUT ) :: obj
  LOGICAL( LGT ), INTENT( IN ) :: bool
END SUBROUTINE xmlFile_setOverwriteStat
END INTERFACE

!----------------------------------------------------------------------------
!                                                          isNew@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 June 2021
! summary: Returns whether or not the HDF5 file is new.

INTERFACE
MODULE PURE FUNCTION xmlFile_isNew( obj ) RESULT( Ans )
  CLASS( XMLFile_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION xmlFile_isNew
END INTERFACE

!----------------------------------------------------------------------------
!                                                    isOverwrite@getMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION xmlFile_isOverwrite( obj ) RESULT( Ans )
  CLASS( XMLFile_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION xmlFile_isOverwrite
END INTERFACE

!----------------------------------------------------------------------------
!                                                           isFormatted
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION xmlFile_isFormatted( obj ) RESULT( ans )
  CLASS( XMLFile_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION xmlFile_isFormatted
END INTERFACE

!----------------------------------------------------------------------------
!                                              ParseXMLDeclaration@IOMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE xmlFile_parseXMLDeclaration( obj )
  CLASS( XMLFile_ ), INTENT( INOUT ) :: obj
END SUBROUTINE xmlFile_parseXMLDeclaration
END INTERFACE

!----------------------------------------------------------------------------
!                                                      buildCache@IOMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE xmlFile_BuildCache( obj, nchars, fileCache )
  CLASS( XMLFile_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( OUT ) :: nchars
  CHARACTER( LEN=1 ), ALLOCATABLE, INTENT( INOUT ) :: fileCache(:)
END SUBROUTINE xmlFile_BuildCache
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Display@IOMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE xmlFile_Display( obj, msg, unitNo )
  CLASS( XMLFile_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitNo
END SUBROUTINE xmlFile_Display
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE XMLFile_Class