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

MODULE XMLTag_Class
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName="XMLTAG_CLASS"

!----------------------------------------------------------------------------
!                                                                  XMLTag_
!----------------------------------------------------------------------------

TYPE :: XMLTag_
  PRIVATE
  TYPE( String ), PUBLIC :: name
    !!  name of tag
  TYPE( String ), PUBLIC :: content
    !! Content
  TYPE( String ), ALLOCATABLE :: attrNames( : )
    !! attribute names
  TYPE( String ), ALLOCATABLE :: attrValues( : )
    !! attribute values
  INTEGER( I4B ) :: tAttributes = 0
    !! Total number of attributes
  TYPE( XMLTag_ ), POINTER :: parent => NULL()
    !! parent tag
  TYPE( XMLTag_ ), POINTER :: children( : ) => NULL()
    !! children tag
  CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS( obj ) :: Initiate => xmlTag_Initiate
END TYPE XMLTag_

PUBLIC :: XMLTag_

TYPE :: XMLTagPointer_
  CLASS( XMLTag_ ), POINTER :: ptr => NULL()
END TYPE XMLTagPointer_

PUBLIC :: XMLTagPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Sept 2021
! summary: Get the name of an XML tag
!
!### Introduction
! Get the name of an XML tag.
! This routine is taken from
! [https://github.com/CASL/Futility/blob/master/src/FileType_XML.f90](https://github.com/CASL/Futility/blob/master/src/FileType_XML.f90)
!

INTERFACE
MODULE SUBROUTINE getTagName(fullTag,ierr,sname)
  CHARACTER( LEN=1 ), INTENT( IN ) :: fullTag(:)
    !! fulltag the full tag string
  INTEGER( I4B ), INTENT( INOUT ) :: ierr
    !! ierr return error code
    !!  0: Success
    !! -1: Bad value for fulltag
    !! -2: illegal first character for tag name
    !! -3: illegal character in tag name
    !! -4: tag name starts with "xml"
  TYPE( String ), INTENT( INOUT ) :: sname
    !! sname a string with the tag name
END SUBROUTINE getTagName
END INTERFACE

END MODULE XMLTag_Class