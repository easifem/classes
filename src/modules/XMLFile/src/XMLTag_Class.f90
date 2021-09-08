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
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName="XMLTAG_CLASS"
TYPE( ExceptionHandler_ ) :: e
  !! name of the module
CHARACTER(LEN=1),PARAMETER :: CHAR_SPACE=' '
  !! Character constant for a single space
CHARACTER(LEN=1),PARAMETER :: CHAR_CR=CHAR(13)
  !! Character constant for a carraige return
CHARACTER(LEN=1),PARAMETER :: CHAR_LF=CHAR(10)
  !! Character constant for a line feed
CHARACTER(LEN=1),PARAMETER :: CHAR_TAB=CHAR(9)
  !! Character constant for a tab
INTEGER(I4B) :: BAD_TAG=-1
  !! Module local constant for indicating a bad tag
INTEGER(I4B) :: START_TAG=1
  !! Module local constant for indicating the start of a tag
INTEGER(I4B) :: END_TAG=2
  !! Module local constant for indicating the end of a tag
INTEGER(I4B) :: EMPTY_ELEMENT_TAG=3
  !! Module local constant for indicating an empty tag
INTEGER(I4B) :: COMMENT_TAG=4
  !! Module local constant for indicating comment tag
INTEGER(I4B) :: PROCESSING_INST_TAG=5
  !! Module local constant for indicating ??? tag
INTEGER(I4B) :: DECLARATION_TAG=6
  !! Module local constant for indicating a declaration tag
  !! the declaration tag, is usually the first tag in the file.

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
  PROCEDURE, PUBLIC, PASS( obj ) :: addSurrogate => xmlTag_addSurrogate
  PROCEDURE, PUBLIC, PASS( obj ) :: Initiate => xmlTag_Initiate
END TYPE XMLTag_

PUBLIC :: XMLTag_

TYPE :: XMLTagPointer_
  CLASS( XMLTag_ ), POINTER :: ptr => NULL()
END TYPE XMLTagPointer_

PUBLIC :: XMLTagPointer_

!----------------------------------------------------------------------------
!                                                      addSurrogate@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 Sept 2021
! summary: This routine adds surrogate to the module
!
!### Introduction
! This routine adds surrogate to the module

INTERFACE
MODULE SUBROUTINE xmlTag_addSurrogate( obj, UserObj )
  CLASS( XMLTag_ ), INTENT( INOUT ) :: obj
  TYPE( ExceptionHandler_ ), INTENT( IN ) :: UserObj
END SUBROUTINE xmlTag_addSurrogate
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Sept 2021
! summary: Get the name of an XML tag
!
!### Introduction
! Get the name of an XML tag.
! This routine is modified from the following library
! [https://github.com/CASL/Futility/blob/master/src/FileType_XML.f90](https://github.com/CASL/Futility/blob/master/src/FileType_XML.f90)
!

INTERFACE
MODULE SUBROUTINE getTagName(chars, tagname, ierr)
  CHARACTER( LEN=1 ), INTENT( IN ) :: chars(:)
    !! chars the full tag string
  TYPE( String ), INTENT( INOUT ) :: tagname
    !! String with Tag name
  INTEGER( I4B ), INTENT( INOUT ) :: ierr
    !! ierr return error code
    !!  0: Success
    !! -1: Bad value for chars
    !! -2: illegal first character for tag name
    !! -3: illegal character in tag name
    !! -4: tag name starts with "xml"
END SUBROUTINE getTagName
END INTERFACE

!----------------------------------------------------------------------------
!                                              ConvertCharArrayToStr@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 Sept 2021
! summary: 	Converts a character array to a string
!

INTERFACE
MODULE PURE SUBROUTINE ConvertCharArrayToStr(chars, strobj)
  CHARACTER(LEN=1),INTENT(IN) :: chars( : )
  TYPE(String),INTENT(INOUT) :: strobj
END SUBROUTINE ConvertCharArrayToStr
END INTERFACE

!----------------------------------------------------------------------------
!                                                 parseTagAttributes@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 Sept 2021
! summary: For a given XML element extract the tag information
!
!### Introduction
!
! This routine extracts the tag information from a given XML element
!
! - ierr is the error code, which has following meaning:
!   - 0: no errors
!   -1: the element string does not have start/end tags
!   -2: "==" was encountered
!   -3: bad attribute name
!   -4: bad attribute value

INTERFACE
MODULE SUBROUTINE parseTagAttributes( chars, tAttributes, attrNames, &
  & attrValues, ierr )
  CHARACTER( LEN=* ), INTENT( IN ) ::  chars
    !! the element string with start tag
  INTEGER( I4B ), INTENT( OUT ) ::  tAttributes
    !! number of attributes in the tag
  TYPE( String ), ALLOCATABLE, INTENT( INOUT ) :: attrNames(:)
    !! names of the attributes
  TYPE( String ), ALLOCATABLE, INTENT( INOUT ) :: attrValues(:)
    !! values of the attributes
  INTEGER( I4B ), INTENT(OUT) :: ierr
    !! error code
END SUBROUTINE parseTagAttributes
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 Sept 2021
! summary: 	Determines the number of child elements fora  given XML element
!
!### Introduction
!
! This routine determines the number of child elements for a  given XML
! element

INTERFACE
MODULE PURE SUBROUTINE getChildTagInfo( tagStart, tagEnd, iTag, tChild, &
  & childTags, ierr )
  INTEGER( I4B ), INTENT( IN ) :: tagStart
    !! The index of the first tag
  INTEGER( I4B ), INTENT( IN ) :: tagEnd
    !! The index of the last tag
  INTEGER( I4B ), INTENT( IN ) :: iTag( :, : )
    !! the XML tags for a given element
  INTEGER( I4B ), INTENT( OUT ) :: tChild
    !! The number of child elements
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: childTags( :, : )
    !! The start/end indices of the tags of any children
  INTEGER( I4B ), INTENT( OUT ) :: ierr
END SUBROUTINE getChildTagInfo
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 Sept 2021
! summary: This routine initializes an instance of XMLTag_ class
!
!### Introduction
! This routine initializes an instance of XMLTag_ class


INTERFACE
MODULE RECURSIVE SUBROUTINE xmlTag_Initiate( obj, cachedFile, iTag, lines, &
  & tagStart, tagEnd )
  CLASS( XMLTag_ ), TARGET, INTENT( INOUT ) :: obj
    !! xmlTag object
  CHARACTER( LEN = 1 ), INTENT( IN ) :: cachedFile( : )
    !! cached file, which will be used to construct the obj
  INTEGER( I4B ), INTENT( IN ) :: iTag(:,:)
    !! The starting and ending position of each tag
  INTEGER( I4B ), INTENT( IN ) :: lines(:)
    !! The position of line endings
  INTEGER( I4B ), INTENT( IN ) :: tagStart
    !! Starting position of tag
  INTEGER( I4B ), INTENT( IN ) :: tagEnd
    !! Ending position of tag
END SUBROUTINE xmlTag_Initiate
END INTERFACE

END MODULE XMLTag_Class