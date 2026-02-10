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
!   - docs
! summary: This module is used for generating docs, it handles the data
!          stored inside the user defined data type.
!
!# UserTypeData
!
! This module defines a class called UserTypeData. This class is used in
! [FortranModuleFile_](../FortranModuleFile_), which can read the
! easifem's  module files and create markdown documentation
! from the source files.

MODULE UserTypeData_Class
USE TxtFile_Class, ONLY: TxtFile_
USE String_Class, ONLY: String
USE GlobalData, ONLY: I4B, DFP, LGT
USE MarkdownData_Class, ONLY: MarkdownData_
IMPLICIT NONE

PRIVATE
PUBLIC :: UserTypeData_
PUBLIC :: UserTypeEntry_
PUBLIC :: UserTypeEntryPointer_
PUBLIC :: UserTypeDataPointer_

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
  PRIVATE
  TYPE(String) :: name
  !! name of the field
  TYPE(String) :: doc
  !! documentation of field
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: SetName => UserTypeEntry_SetName
  !! Set the name in UserTypeEntry
  PROCEDURE, PUBLIC, PASS(obj) :: SetDoc => UserTypeEntry_SetDoc
  !! Set the doc in UserTypeEntry
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
  PRIVATE
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
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! display the content of UserTypeData_
  PROCEDURE, PUBLIC, PASS(obj) :: GenerateMarkdownDocs => &
    obj_GenerateMarkdownDocs
  !! Generate markdown documentation files
  PROCEDURE, PUBLIC, PASS(obj) :: GetName => obj_GetName
  !! Get the name of UserType
  PROCEDURE, PUBLIC, PASS(obj) :: SetHeaderLine => obj_SetHeaderLine
  !! Set the header line
  PROCEDURE, PUBLIC, PASS(obj) :: SetFieldPointer => obj_SetFieldPointer
  !! Set a specific pointer to fields by specifying index and value
  PROCEDURE, PUBLIC, PASS(obj) :: SetMethodPointer => obj_SetMethodPointer
  !! Set a specific pointer to Methods by specifying index and value
  PROCEDURE, PUBLIC, PASS(obj) :: SetIsChild => obj_SetIsChild
  !! Set the value of isChild
  PROCEDURE, PUBLIC, PASS(obj) :: SetIsAbstract => obj_SetIsAbstract
  !! Set the value of isAbstract
  PROCEDURE, PUBLIC, PASS(obj) :: SetName => obj_SetName
  !! Set the value of name
  PROCEDURE, PUBLIC, PASS(obj) :: SetMd => obj_SetMd
  !! Set the value of Md
  PROCEDURE, PUBLIC, PASS(obj) :: AllocateFields => obj_AllocateFields
  !! allocate the fields
  PROCEDURE, PUBLIC, PASS(obj) :: AllocateMethods => obj_AllocateMethods
  !! allocate the Methods
END TYPE UserTypeData_

!----------------------------------------------------------------------------
!                                                       UserTypeDataPointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-02
! summary: data type to contain a pointer to [UserTypeData_](./UserTypeData_)

TYPE :: UserTypeDataPointer_
  TYPE(UserTypeData_), POINTER :: ptr => NULL()
  !! pointer to UserTypeData
END TYPE UserTypeDataPointer_

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
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(UserTypeData_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
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
  MODULE SUBROUTINE obj_GenerateMarkdownDocs(obj)
    CLASS(UserTypeData_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_GenerateMarkdownDocs
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetName@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-10
! summary: Get name of UserType

INTERFACE
  MODULE FUNCTION obj_GetName(obj) RESULT(ans)
    CLASS(UserTypeData_), INTENT(IN) :: obj
    TYPE(String) :: ans
  END FUNCTION obj_GetName
END INTERFACE

!----------------------------------------------------------------------------
!                                                  SetHeaderLine@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-10
! summary: Set headerLine, after removing pre and post spaces in val

INTERFACE
  MODULE SUBROUTINE obj_SetHeaderLine(obj, val)
    CLASS(UserTypeData_), INTENT(INOUT) :: obj
    !! user defined data type
    TYPE(String), INTENT(IN) :: val
    !! value of headerLine in obj
  END SUBROUTINE obj_SetHeaderLine
END INTERFACE

!----------------------------------------------------------------------------
!                                                  SetFieldPointer@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-10
! summary: Set a pointer to fields
!
!# SetFieldPointer
!
! Set a pointer to `obj%fields(indx)`. Make sure that fields is already
! allocated, and indx is not out of bound.

INTERFACE
  MODULE SUBROUTINE obj_SetFieldPointer(obj, indx, val)
    CLASS(UserTypeData_), INTENT(INOUT) :: obj
    !! user defined data type
    INTEGER(I4B), INTENT(IN) :: indx
    !! indx is used in fields, fields(indx)=>val
    TYPE(UserTypeEntry_), TARGET, INTENT(IN) :: val
    !! target of fields(indx)
  END SUBROUTINE obj_SetFieldPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                SetMethodPointer@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-10
! summary: Set a pointer to methods
!
!# SetMethodPointer
!
! Set a pointer to `obj%methods(indx)`. Make sure that methods is already
! allocated, and indx is not out of bound.
!

INTERFACE
  MODULE SUBROUTINE obj_SetMethodPointer(obj, indx, val)
    CLASS(UserTypeData_), INTENT(INOUT) :: obj
    !! user defined data type
    INTEGER(I4B), INTENT(IN) :: indx
    !! indx is used in Methods, Methods(indx)=>val
    TYPE(UserTypeEntry_), TARGET, INTENT(IN) :: val
    !! target of Methods(indx)
  END SUBROUTINE obj_SetMethodPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                         SetName@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-10
! summary: Set the name in UserTypeEntry
!
!# SetName
!
! Set the name in UserTypeEntry, the val will be cleaned.

INTERFACE
  MODULE SUBROUTINE UserTypeEntry_SetName(obj, val)
    CLASS(UserTypeEntry_), INTENT(INOUT) :: obj
    !! user type entry
    TYPE(String), TARGET, INTENT(IN) :: val
    !! value of name
  END SUBROUTINE UserTypeEntry_SetName
END INTERFACE

!----------------------------------------------------------------------------
!                                                         SetDoc@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-10
! summary: Set the doc in UserTypeEntry
!
!# SetDoc
!
! Set the doc in UserTypeEntry, the val will be cleaned.

INTERFACE
  MODULE SUBROUTINE UserTypeEntry_SetDoc(obj, val)
    CLASS(UserTypeEntry_), INTENT(INOUT) :: obj
    !! user type entry
    TYPE(String), TARGET, INTENT(IN) :: val
    !! value of Doc
  END SUBROUTINE UserTypeEntry_SetDoc
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetIsChild@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-10
! summary: Set isChild field of UserTypeData
!
!# SetIsChild
!
! This method sets the isChild field of obj

INTERFACE
  MODULE SUBROUTINE obj_SetIsChild(obj, val)
    CLASS(UserTypeData_), INTENT(INOUT) :: obj
    !! user type entry
    LOGICAL(LGT), INTENT(IN) :: val
    !! value for isChild
  END SUBROUTINE obj_SetIsChild
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetIsAbstract@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-10
! summary: Set isAbstract field of UserTypeData
!
!# SetIsAbstract
!
! This method sets the isAbstract field of obj.

INTERFACE
  MODULE SUBROUTINE obj_SetIsAbstract(obj, val)
    CLASS(UserTypeData_), INTENT(INOUT) :: obj
    !! user type entry
    LOGICAL(LGT), INTENT(IN) :: val
    !! value for isAbstract
  END SUBROUTINE obj_SetIsAbstract
END INTERFACE

!----------------------------------------------------------------------------
!                                                         SetName@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-10
! summary: Set name field of UserTypeData
!
!# SetName
!
! This method sets the name field of obj

INTERFACE
  MODULE SUBROUTINE obj_SetName(obj, val)
    CLASS(UserTypeData_), INTENT(INOUT) :: obj
    !! user type entry
    TYPE(String), INTENT(IN) :: val
    !! value for name
  END SUBROUTINE obj_SetName
END INTERFACE

!----------------------------------------------------------------------------
!                                                           SetMd@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-10
! summary: Set md field of UserTypeData
!
!# SetMd
!
! This method sets the md field of obj

INTERFACE
  MODULE SUBROUTINE obj_SetMd(obj, val)
    CLASS(UserTypeData_), INTENT(INOUT) :: obj
    !! user type entry
    TYPE(MarkdownData_), INTENT(IN) :: val
    !! value for md
  END SUBROUTINE obj_SetMd
END INTERFACE

!----------------------------------------------------------------------------
!                                           AllocateFields@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-10
! summary: Allocates the fields
!
!# AllocateFields
!
! This method allocate the fields of obj. Make sure fields is not allocated
! before as reallocation is not allowed.

INTERFACE
  MODULE SUBROUTINE obj_AllocateFields(obj, tsize)
    CLASS(UserTypeData_), INTENT(INOUT) :: obj
    !! user defined data type
    INTEGER(I4B), INTENT(IN) :: tsize
    !! total number of fields
  END SUBROUTINE obj_AllocateFields
END INTERFACE

!----------------------------------------------------------------------------
!                                         AllocateMethods@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-10
! summary: Allocates the Methods
!
!# AllocateMethods
!
! This method allocates methods field of obj. Make sure methods is not
! allocated before, as reallocation is not allowed.

INTERFACE
  MODULE SUBROUTINE obj_AllocateMethods(obj, tsize)
    CLASS(UserTypeData_), INTENT(INOUT) :: obj
    !! user defined data type
    INTEGER(I4B), INTENT(IN) :: tsize
    !! total number of Methods
  END SUBROUTINE obj_AllocateMethods
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE UserTypeData_Class

