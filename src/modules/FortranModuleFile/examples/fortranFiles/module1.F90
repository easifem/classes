! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-01
! status: stable
! extpkgs: none
! tags:
!   - file
!   - docs
! summary: This module handles the fortran module source files defined
!          in easifemClass
!
!# FortranModuleFile
!
! This module extends the [TxtFile_](../TxtFile) class to
! [FortranModuleFile_](./FortranModuleFile_), which can read the
! easifem's  module files. It can also create markdown documentation
! from the source files.

MODULE FortranModuleFile_Class
USE TxtFile_Class, ONLY: TxtFile_
USE String_Class, ONLY: String
USE GlobalData, ONLY: I4B, DFP, LGT
USE MarkdownData_Class, ONLY: MarkdownData_
IMPLICIT NONE

PRIVATE
PUBLIC :: FortranModuleFile_

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "FortranModuleFile_Class()"
#endif

!----------------------------------------------------------------------------
!                                                             UserTypeEntry_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-02
! summary: data type for keeping a single entry of the field
!
!# UserTypeEntry_
!
! `UserTypeEntry_` handle a single entry defined in the UserType.
! it can be used to handle documentation of fields as well as methods

TYPE :: UserTypeEntry_
  TYPE(String) :: name
  !! name of the field
  TYPE(String) :: doc
  !! documentation of field
END TYPE UserTypeEntry_

!----------------------------------------------------------------------------
!                                                      UserTypeEntryPointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-02
! summary: data type which contains pointer to UserTypeEntry_

TYPE :: UserTypeEntryPointer_
  TYPE(UserTypeEntry_), POINTER :: ptr => NULL()
END TYPE UserTypeEntryPointer_

!----------------------------------------------------------------------------
!                                                              UserTypeData_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-02
! summary: User type for storing user data types
!
!# UserTypeData_
!
! `UserTypeData_` contains data for handling documentation of easifem
! data types

TYPE :: UserTypeData_
  LOGICAL(LGT) :: isChild = .FALSE.
  !! it is true when user type is a child class, i.e., extends is present
  LOGICAL(LGT) :: isAbstract = .FALSE.
  !! it is true when user type is an abstract class
  TYPE(String) :: name
  !! name of data type
  TYPE(MarkdownData_) :: md
  !! Information of module
  TYPE(UserTypeEntryPointer_), ALLOCATABLE :: fields(:)
  !! fields name for user data type
END TYPE UserTypeData_

!----------------------------------------------------------------------------
!                                                       UserTypeDataPointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-02
! summary: data type to contain a pointer to [UserTypeData_](./UserTypeData_)

TYPE :: UserTypeDataPointer_
  TYPE(UserTypeData_), POINTER :: ptr => NULL()
END TYPE UserTypeDataPointer_

!----------------------------------------------------------------------------
!                                                         FortranModuleFile_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-01
! summary: This class contains data of a fortran module file

TYPE, EXTENDS(TxtFile_) :: FortranModuleFile_
  TYPE(String) :: moduleName
  !! name of the module
  TYPE(String) :: moduleDir
  !! directory name where module markdown files will be created
  TYPE(String) :: moduleUsed
  !! lists of use modules used in the module, separated by CHAR_LF
  TYPE(MarkdownData_) :: md
  !! Information of module in mardown format
  TYPE(UserTypeDataPointer_), ALLOCATABLE :: userTypes(:)
  !! list of user defined data types in the module

CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: ReadModuleFile => obj_ReadModuleFile
  !! parse the module source file
  PROCEDURE, PUBLIC, PASS(obj) :: ReadPreComments => obj_ReadPreComments
  !! parse the pre comments which starts after !>
  PROCEDURE, PUBLIC, PASS(obj) :: SearchKeywordInCommentAtStart => &
    obj_SearchKeywordInCommentAtStart
  !! search for a keyword at the beginning of comments
  PROCEDURE, PUBLIC, PASS(obj) :: SearchKeywordInSourceAtStart => &
    obj_SearchKeywordInSourceAtStart
  !! search for a keyword at the beginning of source code
  PROCEDURE, PUBLIC, PASS(obj) :: SearchUserTypes => obj_SearchUserTypes
  !! Search user types in the fortran source files
  PROCEDURE, PUBLIC, PASS(obj) :: ReadModuleName => obj_ReadModuleName
  !! read the module name from the source file
  PROCEDURE, PUBLIC, PASS(obj) :: ReadUseStatements => obj_ReadUseStatements
  !! read use statements in the module file
  PROCEDURE, PUBLIC, PASS(obj) :: ReadUserTypes => obj_ReadUserTypes
  !! read user types from the module file
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalUserTypes => obj_GetTotalUserTypes
  !! get total user types
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalFieldsInUserType => &
    obj_GetTotalFieldsInUserType
  !! get total fields in the user types
  PROCEDURE, PUBLIC, PASS(obj) :: ReadFortranLine => obj_ReadFortranLine
  !! Read a fortran line, check if line ends with &, if yes read next line &
  !! too
END TYPE FortranModuleFile_

!----------------------------------------------------------------------------
!                                       ReadModuleFile@ReadModuleFileMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-01
! summary: Parse the module source file
!
!# Introduction
! This method parse the module source file. It reads the source file
! and prepare the data structure for FortranModuleFile.

INTERFACE
  MODULE SUBROUTINE obj_ReadModuleFile(obj)
    CLASS(FortranModuleFile_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_ReadModuleFile
END INTERFACE

!----------------------------------------------------------------------------
!                                     ReadModuleName@ReadModuleFileMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-02
! summary: read module name from the source file

INTERFACE
  MODULE SUBROUTINE obj_ReadModuleName(obj, lineLoc, numLineRead, isFound)
    CLASS(FortranModuleFile_), INTENT(INOUT) :: obj
  !! Fortran module file
    INTEGER(I4B), INTENT(INOUT) :: lineLoc
  !! location of current line which is being read
    INTEGER(I4B), INTENT(OUT) :: numLineRead
  !! Total number of lines read
    LOGICAL(LGT), INTENT(OUT) :: isFound
  !! it is set to when keyword is  found
  END SUBROUTINE obj_ReadModuleName
END INTERFACE

!----------------------------------------------------------------------------
!                                     ReadPreComments@ReadPreCommentsMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-01
! summary: Read module information
!
!# ReadPreComments
!
! This method reads the following information given before
! module statement
!
!```txt
!> author: Vikas Sharma, Ph. D.
! date: 2026-02-01
! status: stable
! extpkgs: none
! tags:
!   - file
!   - docs
! summary: This module handles the fortran module source files defined
!          in easifemClass
!```

INTERFACE
  MODULE SUBROUTINE obj_ReadPreComments(obj, lineLoc, numLineRead)
    CLASS(FortranModuleFile_), INTENT(INOUT) :: obj
  !! Fortran module file
    INTEGER(I4B), INTENT(INOUT) :: lineLoc
  !! location of current line which is being read
    INTEGER(I4B), INTENT(OUT) :: numLineRead
  !! Total number of lines read
  END SUBROUTINE obj_ReadPreComments
END INTERFACE

!----------------------------------------------------------------------------
!                                SearchKeywordInCommentAtStart@SearchMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-02
! summary: This method searches keywords at start of comment lines

INTERFACE
  MODULE SUBROUTINE obj_SearchKeywordInCommentAtStart( &
    obj, aline, keyword, lineLoc, numLineRead, isFound)
    CLASS(FortranModuleFile_), INTENT(INOUT) :: obj
  !! Fortran module file
    TYPE(String), INTENT(INOUT) :: aline
  !! the line which contains keyword will ne returned
    CHARACTER(*), INTENT(IN) :: keyword
  !! keyword to be searched
    INTEGER(I4B), INTENT(INOUT) :: lineLoc
  !! location of current line which is being read
    INTEGER(I4B), INTENT(OUT) :: numLineRead
  !! Total number of lines read
    LOGICAL(LGT), INTENT(OUT) :: isFound
  !! it true when keyword is found
  END SUBROUTINE obj_SearchKeywordInCommentAtStart
END INTERFACE

!----------------------------------------------------------------------------
!                                 SearchKeywordInSourceAtStart@SearchMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-02
! summary: This method searches a keyword at the beginning of source

INTERFACE
  MODULE SUBROUTINE obj_SearchKeywordInSourceAtStart( &
    obj, aline, keyword, lineLoc, numLineRead, isFound, readSingleLine)
    CLASS(FortranModuleFile_), INTENT(INOUT) :: obj
  !! Fortran module file
    TYPE(String), INTENT(INOUT) :: aline
  !! the line which contains keyword will ne returned
    CHARACTER(*), INTENT(IN) :: keyword
  !! keyword to be searched
    INTEGER(I4B), INTENT(INOUT) :: lineLoc
  !! location of current line which is being read
    INTEGER(I4B), INTENT(OUT) :: numLineRead
  !! Total number of lines read
    LOGICAL(LGT), INTENT(OUT) :: isFound
  !! it true when keyword is found
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: readSingleLine
  !! if present and true then this method reads only a single line
  END SUBROUTINE obj_SearchKeywordInSourceAtStart
END INTERFACE

!----------------------------------------------------------------------------
!                                              SearchUserTypes@SearchMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-02
! summary: This method searches user types in the fortran module

INTERFACE
  MODULE SUBROUTINE obj_SearchUserTypes( &
    obj, aline, lineLoc, numLineRead, isFound, caseType, readSingleLine)
    CLASS(FortranModuleFile_), INTENT(INOUT) :: obj
    !! Fortran module file
    TYPE(String), INTENT(INOUT) :: aline
    !! the line which contains keyword will ne returned
    INTEGER(I4B), INTENT(INOUT) :: lineLoc
    !! location of current line which is being read
    INTEGER(I4B), INTENT(OUT) :: numLineRead
    !! Total number of lines read
    LOGICAL(LGT), INTENT(OUT) :: isFound
    !! it true when keyword is found
    INTEGER(I4B), INTENT(OUT) :: caseType
    !! user type case, following options are considered
    !! caseType 1, TYPE :: name
    !! caseType 2, TYPE, EXTENDS(name) :: name
    !! caseType 3, TYPE, ABSTRACT :: name
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: readSingleLine
    !! if present and true then this method reads only a single line
  END SUBROUTINE obj_SearchUserTypes
END INTERFACE

!----------------------------------------------------------------------------
!                                  ReadUseStatement@ReadUseStatementsMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-02
! summary: read the use statements in the module

INTERFACE
  MODULE SUBROUTINE obj_ReadUseStatements(obj, lineLoc, numLineRead)
    CLASS(FortranModuleFile_), INTENT(INOUT) :: obj
  !! Fortran module file
    INTEGER(I4B), INTENT(INOUT) :: lineLoc
  !! location of current line which is being read
    INTEGER(I4B), INTENT(OUT) :: numLineRead
  !! Total number of lines read
  END SUBROUTINE obj_ReadUseStatements
END INTERFACE

!----------------------------------------------------------------------------
!                                         ReadUserTypes@ReadUserTypesMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-02
! summary: read user types from the module files

INTERFACE
  MODULE SUBROUTINE obj_ReadUserTypes(obj, lineLoc, numLineRead)
    CLASS(FortranModuleFile_), INTENT(INOUT) :: obj
  !! Fortran module file
    INTEGER(I4B), INTENT(INOUT) :: lineLoc
  !! location of current line which is being read
    INTEGER(I4B), INTENT(OUT) :: numLineRead
  !! Total number of lines read
  END SUBROUTINE obj_ReadUserTypes
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetTotalUserTypes@UserTypeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-02
! summary: Get total number of user types

INTERFACE
  MODULE FUNCTION obj_GetTotalUserTypes(obj) RESULT(ans)
    CLASS(FortranModuleFile_), INTENT(INOUT) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalUserTypes
END INTERFACE

!----------------------------------------------------------------------------
!                                   GetTotalFieldsInUserTypes@UserTypeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-03
! summary: Get total number of fields in a usertype

INTERFACE
  MODULE FUNCTION obj_GetTotalFieldsInUserType(obj) RESULT(ans)
    CLASS(FortranModuleFile_), INTENT(INOUT) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalFieldsInUserType
END INTERFACE

!----------------------------------------------------------------------------
!                                            ReadFortranLine@ReadLineMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-03
! summary: Read a fortran line, if the line ends with &, then read next too
!
!# ReadFortranLine
!
! This method reads a fortran line correctly. If the fortran line is
! split using &, then it will read the next line and join them

INTERFACE
  MODULE SUBROUTINE obj_ReadFortranLine(obj, aline, lineLoc, numLineRead, &
                                        isFound, readSingleLine)
    CLASS(FortranModuleFile_), INTENT(INOUT) :: obj
    !! fortran module file object
    TYPE(String), INTENT(OUT) :: aline
    !! a fortran line
    INTEGER(I4B), INTENT(INOUT) :: lineLoc
    !! location in the line before and after the reading
    INTEGER(I4B), INTENT(OUT) :: numLineRead
    !! number of lines read by this routine
    LOGICAL(LGT), INTENT(OUT) :: isFound
    !! Set to true if the line is read properly
    LOGICAL(LGT), INTENT(IN) :: readSingleLine
    !! if readSingleLine is true then we read only a single
    !! if that line is comment, it will return isFound = false.
  END SUBROUTINE obj_ReadFortranLine
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FortranModuleFile_Class

