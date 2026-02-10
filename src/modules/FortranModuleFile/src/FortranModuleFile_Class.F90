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
CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: Display => UserTypeEntry_Display
  !! display the content of UserTypeData_
END TYPE UserTypeEntry_

!----------------------------------------------------------------------------
!                                                      UserTypeEntryPointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-02
! summary: This data type contains pointer to UserTypeEntry_

TYPE :: UserTypeEntryPointer_
  TYPE(UserTypeEntry_), POINTER :: ptr => NULL()
  !! pointer to UserTypeEntry
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
  TYPE(String) :: headerLine
  !! type definition declaration line.
  !! for example "type :: UserTypeData_"
  TYPE(String) :: name
  !! name of user defined datatype
  !! for example "UserTypeData_"
  TYPE(MarkdownData_) :: md
  !! Information of module
  TYPE(UserTypeEntryPointer_), ALLOCATABLE :: fields(:)
  !! fields defined inside a user defined datatype
  TYPE(UserTypeEntryPointer_), ALLOCATABLE :: methods(:)
  !! methods defined inside a user defined datatype

CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: Display => UserTypeData_Display
  !! display the content of UserTypeData_
  PROCEDURE, PUBLIC, PASS(obj) :: GenerateMarkdownDocs => &
    UserTypeData_GenerateMarkdownDocs
  !! Generate markdown documentation files
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
!
!# FortranModuleFile_
!
! This class contains data of a fortran module file. This class can
! read a fortran module and create markdown files for documentation.

TYPE, EXTENDS(TxtFile_) :: FortranModuleFile_
  TYPE(String) :: moduleName
  !! name of the module
  TYPE(String) :: moduleDir
  !! directory name where module markdown files will be created
  TYPE(MarkdownData_) :: md
  !! Information of module in mardown format
  TYPE(String) :: moduleUsed
  !! lists of use modules used in the module, separated by CHAR_LF
  TYPE(UserTypeDataPointer_), ALLOCATABLE :: userTypes(:)
  !! list of user defined data types in the module
  ! TYPE(ProcedureDataPointer_), ALLOCATABLE :: userTypes(:)
  ! !! list of user defined data types in the module

CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: ReadModuleFile => obj_ReadModuleFile
  !! parse the module source file
  PROCEDURE, PUBLIC, PASS(obj) :: ReadMarkdownData => obj_ReadMarkdownData
  !! parse the pre comments which starts after !>
  PROCEDURE, PUBLIC, PASS(obj) :: SearchKeywordInCommentAtStart => &
    obj_SearchKeywordInCommentAtStart
  !! search for a keyword at the beginning of comments
  PROCEDURE, PUBLIC, PASS(obj) :: SearchKeywordInCommentAtStartBack => &
    obj_SearchKeywordInCommentAtStartBack
  !! search for a keyword at the beginning of comments in back mode
  PROCEDURE, PUBLIC, PASS(obj) :: SearchKeywordInSourceAtStart => &
    obj_SearchKeywordInSourceAtStart
  !! search for a keyword at the beginning of source code
  PROCEDURE, PUBLIC, PASS(obj) :: SearchUserTypes => obj_SearchUserTypes
  !! Search user types in the fortran source files
  PROCEDURE, PUBLIC, PASS(obj) :: SearchUserTypesBack => &
    obj_SearchUserTypesBack
  !! Search user types in the fortran source files in backward mode
  PROCEDURE, PUBLIC, PASS(obj) :: SkipBlankLinesBack => &
    obj_SkipBlankLinesBack
  !! Skip the blanklines and reach to a nonblank line in backward mode
  PROCEDURE, PUBLIC, PASS(obj) :: ReadModuleName => obj_ReadModuleName
  !! read the module name from the source file
  PROCEDURE, PUBLIC, PASS(obj) :: ReadModuleDir => obj_ReadModuleDir
  !! Get moduleDir from moduleName
  PROCEDURE, PUBLIC, PASS(obj) :: ReadUseStatements => obj_ReadUseStatements
  !! read use statements in the module file
  PROCEDURE, PUBLIC, PASS(obj) :: ReadUserTypes => obj_ReadUserTypes
  !! read user types from the module file
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalUserTypes => obj_GetTotalUserTypes
  !! get total user types
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalFieldsInUserType => &
    obj_GetTotalFieldsInUserType
  !! get total fields in the user types
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalMethodsInUserType => &
    obj_GetTotalMethodsInUserType
  !! get total Methods in user defined datatypes
  PROCEDURE, PUBLIC, PASS(obj) :: ReadFortranLine => obj_ReadFortranLine
  !! Read a fortran line, check if line ends with &,
  !! if true read next line too
  PROCEDURE, PUBLIC, PASS(obj) :: ReadDocCommentLine => &
    obj_ReadDocCommentLine
  !! Read a doc comment, usually starts with !!,
  !! keep on appending the docs until a non docstring is found
  PROCEDURE, PUBLIC, PASS(obj) :: ReadFieldInUserType => &
    obj_ReadFieldInUserType
  !! Read a user type entry within the usertype block
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content of fortran module file
  PROCEDURE, PUBLIC, PASS(obj) :: GenerateMarkdownDocs => &
    obj_GenerateMarkdownDocs
  !! Generate Markdown documentation for data stored in fortran module files
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
!                                       ReadModuleName@ReadModuleFileMethods
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
!                                       ReadModuleDir@ReadModuleFileMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-02
! summary: read module dir from the moduleName
!
!# ReadModuleDir
!
! Read moduleDir from moduleName. Following rules are used
!
! - `foo_Class` will result in foo
! - `foo_Method` will result in foo
! - `foo_Methods` will result in foo
! - `fooUtility` will result in `fooUtility`

INTERFACE
  MODULE SUBROUTINE obj_ReadModuleDir(obj)
    CLASS(FortranModuleFile_), INTENT(INOUT) :: obj
    !! Fortran module file
  END SUBROUTINE obj_ReadModuleDir
END INTERFACE

!----------------------------------------------------------------------------
!                                   ReadMarkdownData@ReadMarkdownDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-01
! summary: Read the markdown data given before a code entity.
!
!# ReadPreComments
!
! This method reads the following information given before
! module statement
!
!```txt
!!> author: Vikas Sharma, Ph. D.
!! date: 2026-02-01
!! status: stable
!! extpkgs: none
!! tags:
!!   - file
!!   - docs
!! summary: This module handles the fortran module source files defined
!!          in easifemClass
!```

INTERFACE
  MODULE SUBROUTINE obj_ReadMarkdownData(obj, md, lineLoc, numLineRead)
    CLASS(FortranModuleFile_), INTENT(INOUT) :: obj
    !! Fortran module file
    TYPE(MarkdownData_), INTENT(INOUT) :: md
    !! markdown data, which will be read by this method
    INTEGER(I4B), INTENT(INOUT) :: lineLoc
    !! location of current line which is being read
    INTEGER(I4B), INTENT(OUT) :: numLineRead
    !! Total number of lines read
  END SUBROUTINE obj_ReadMarkdownData
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
!                            SearchKeywordInCommentAtStartBack@SearchMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-02
! summary: This method searches keywords at start of comment lines backward
!
!# SearchKeywordInCommentAtStartBack
!
! Search keywords in comment lines. The keyword is searched at the begining
! The search is made in backword, which means the returned lineLoc will be
! lesser than the input lineloc.

INTERFACE
  MODULE SUBROUTINE obj_SearchKeywordInCommentAtStartBack( &
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
  END SUBROUTINE obj_SearchKeywordInCommentAtStartBack
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
    obj, aline, lineLoc, numLineRead, isFound, caseType)
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
  END SUBROUTINE obj_SearchUserTypes
END INTERFACE

!----------------------------------------------------------------------------
!                                          SearchUserTypesBack@SearchMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-02
! summary: This method searches user types in the fortran module in back mode
!
!# SearchUserTypesBack
!
! This method searches for user types in the fortran module in backmode.
! This means that after calling this method lineLoc will decrease.
! The numLineRead denotes the number of lines read in backward order.

INTERFACE
  MODULE SUBROUTINE obj_SearchUserTypesBack( &
    obj, aline, lineLoc, numLineRead, isFound, caseType)
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
  END SUBROUTINE obj_SearchUserTypesBack
END INTERFACE

!----------------------------------------------------------------------------
!                                          SkipBlankLinesBack@SearchMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-02
! summary: This method skips the blanklines in backward mode
!
!# SkipBlankLinesBack
!
! This method skips the blanklines in backward mode.
! This means that after calling this method lineLoc will decrease.
! The numLineRead denotes the number of lines read in backward order.

INTERFACE
  MODULE SUBROUTINE obj_SkipBlankLinesBack( &
    obj, aline, lineLoc, numLineRead)
    CLASS(FortranModuleFile_), INTENT(INOUT) :: obj
    !! Fortran module file
    TYPE(String), INTENT(INOUT) :: aline
    !! the line which contains keyword will ne returned
    INTEGER(I4B), INTENT(INOUT) :: lineLoc
    !! location of current line which is being read
    INTEGER(I4B), INTENT(OUT) :: numLineRead
    !! Total number of lines read
  END SUBROUTINE obj_SkipBlankLinesBack
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
  MODULE FUNCTION obj_GetTotalUserTypes(obj, lineLoc) RESULT(ans)
    CLASS(FortranModuleFile_), INTENT(INOUT) :: obj
    !! Fortran module file
    INTEGER(I4B), INTENT(IN) :: lineLoc
    !! line location before calling this routine
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
!                                 GetTotalMethodsInUserTypes@UserTypeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-03
! summary: Get total number of Methods in a user defined data types

INTERFACE
  MODULE FUNCTION obj_GetTotalMethodsInUserType(obj) RESULT(ans)
    CLASS(FortranModuleFile_), INTENT(INOUT) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalMethodsInUserType
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
    !! if readSingleLine is true, then we read only a single line,
    !! if that line is comment, it will return isFound = false.
    !! Set readSingleLine to true if you do not want to skip comments
  END SUBROUTINE obj_ReadFortranLine
END INTERFACE

!----------------------------------------------------------------------------
!                                          ReadDocCommentLine@ReadLineMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-03
! summary: Read doc comment line, usually starts with !!, keep on appending
!
!# ReadDocCommentLine
!
! This method reads a doc string correctly. The doc line starts
! with docString, usually starts with !!.
! It will keep on reading and appending the docstrings until
! the line starts with the nondoc string, including a blankline

INTERFACE
  MODULE SUBROUTINE obj_ReadDocCommentLine( &
    obj, aline, lineLoc, numLineRead, isFound, docString)
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
    CHARACTER(*), INTENT(IN) :: docString
    !! docString
  END SUBROUTINE obj_ReadDocCommentLine
END INTERFACE

!----------------------------------------------------------------------------
!                                        ReadFieldInUserType@UserTypeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-03
! summary: Read field entry in usertype definition block
!
!# ReadFieldInUserType
!
! This method reads a field declared in the usertype definition.

INTERFACE
  MODULE SUBROUTINE obj_ReadFieldInUserType(obj, val, lineLoc, numLineRead, &
                                            isFound)
    CLASS(FortranModuleFile_), INTENT(INOUT) :: obj
    !! fortran module file object
    TYPE(UserTypeEntry_), INTENT(INOUT) :: val
    !! val will be formed by this routine
    INTEGER(I4B), INTENT(INOUT) :: lineLoc
    !! location in the line before and after the reading
    INTEGER(I4B), INTENT(OUT) :: numLineRead
    !! number of lines read by this routine
    LOGICAL(LGT), INTENT(OUT) :: isFound
    !! Set to true if the line is read properly
  END SUBROUTINE obj_ReadFieldInUserType
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Display@UserTypeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-03
! summary: Display the content of UserTypeData_
!
!# Display
!
! This method displays the content of UserTypeData_

INTERFACE
  MODULE SUBROUTINE UserTypeData_Display(obj, msg, unitNo)
    CLASS(UserTypeData_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE UserTypeData_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                Display@UserTypeEntryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-03
! summary: Display the content of UserTypeEntry_
!
!# Display
!
! This method displays the content of UserTypeEntry_

INTERFACE
  MODULE SUBROUTINE UserTypeEntry_Display(obj, msg, unitNo)
    CLASS(UserTypeEntry_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE UserTypeEntry_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-04
! summary: Display the content of fortran module file.
!
!# Display
!
! This method display the content of FortranModuleFile_.
!

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(FortranModuleFile_), INTENT(INOUT) :: obj
    !! Fortran module file
    CHARACTER(*), INTENT(IN) :: msg
    !! message
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: unitNo
    !! unit number, default is standard output
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                       GenerateMarkdownDocs@MarkdownMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-04
! summary: Generate markdown docs for data stored inside FortranModuleFile_
!
!# GenerateMarkdownDocs
!
! Generate markdown docs for data stored inside FortranModuleFile_.

INTERFACE
  MODULE SUBROUTINE obj_GenerateMarkdownDocs(obj)
    CLASS(FortranModuleFile_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_GenerateMarkdownDocs
END INTERFACE

!----------------------------------------------------------------------------
!                                      GenerateMarkdownDocs@UserTypesMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-04
! summary: Generate markdown docs for data stored inside UserTypeData_
!
!# GenerateMarkdownDocs
!
! Generate markdown docs for data stored inside UserTypeData_.

INTERFACE
  MODULE SUBROUTINE UserTypeData_GenerateMarkdownDocs(obj)
    CLASS(UserTypeData_), INTENT(INOUT) :: obj
  END SUBROUTINE UserTypeData_GenerateMarkdownDocs
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FortranModuleFile_Class

