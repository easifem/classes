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
!          stored inside the procedure interface.
!
!# ProcedureData
!
! This module defines a class called ProcedureData. This class is used in
! [FortranModuleFile_](../FortranModuleFile_), which can read the
! easifem's  module files and create markdown documentation
! from the source files.

MODULE ProcedureData_Class
USE String_Class, ONLY: String
USE GlobalData, ONLY: I4B, DFP, LGT
USE MarkdownData_Class, ONLY: MarkdownData_
IMPLICIT NONE

PRIVATE
PUBLIC :: ProcedureData_
PUBLIC :: ProcedureEntry_
PUBLIC :: ProcedureEntryPointer_
PUBLIC :: ProcedureDataPointer_

!----------------------------------------------------------------------------
!                                                            ProcedureEntry_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-02
! summary: data type for keeping a line of declaration for arguments
!
!# ProcedureEntry_
!
! `ProcedureEntry_` handles a single line of declaration for arguments of
! the procedure.

TYPE :: ProcedureEntry_
  PRIVATE
  TYPE(String) :: name
  !! name of the argument
  TYPE(String) :: doc
  !! documentation of argument
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: SetName => ProcedureEntry_SetName
  !! Set the name in ProcedureEntry_
  PROCEDURE, PUBLIC, PASS(obj) :: SetDoc => ProcedureEntry_SetDoc
  !! Set the doc in ProcedureEntry_
END TYPE ProcedureEntry_

!----------------------------------------------------------------------------
!                                                      ProcedureEntryPointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-02
! summary: This data type contains pointer to ProcedureEntry_

TYPE :: ProcedureEntryPointer_
  TYPE(ProcedureEntry_), POINTER :: ptr => NULL()
  !! pointer to ProcedureEntry
END TYPE ProcedureEntryPointer_

!----------------------------------------------------------------------------
!                                                              ProcedureData_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-02
! summary: User type for storing the data of a procedure
!
!# ProcedureData_
!
! `ProcedureData_` contains data for handling documentation of easifem
! data types. It contains the data of procedure interface defined in the
! module.

TYPE :: ProcedureData_
  PRIVATE
  LOGICAL(LGT) :: isSubroutine = .FALSE.
  !! true if the procedure is subroutine
  LOGICAL(LGT) :: isFunction = .FALSE.
  !! true if the procedure is function
  LOGICAL(LGT) :: isAbstract = .FALSE.
  !! true if the procedure is an abstract interface
  LOGICAL(LGT) :: isModuleProcedure = .FALSE.
  !! true if the procedure is generic method with module procedures
  !! in it.
  LOGICAL(LGT) :: isGeneric = .FALSE.
  !! true if the procedure is a generic interface
  TYPE(String) :: code
  !! code block for procedure interface
  TYPE(String) :: name
  !! name of the procedure
  !! for example "obj_Initiate"
  TYPE(String) :: genericName
  !! generic name of the procedure
  TYPE(MarkdownData_) :: md
  !! Information of procedure which is given before the procedure
  !! in fortran comments and markdown format.
  TYPE(ProcedureEntryPointer_), ALLOCATABLE :: args(:)
  !! fields defined inside a user defined datatype

CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! display the content of UserTypeData_
  PROCEDURE, PUBLIC, PASS(obj) :: GenerateMarkdownDocs => &
    obj_GenerateMarkdownDocs
  !! Generate markdown documentation files
  PROCEDURE, PUBLIC, PASS(obj) :: GetName => obj_GetName
  !! Get the name of UserType
  PROCEDURE, PUBLIC, PASS(obj) :: SetArgPointer => obj_SetArgPointer
  !! Set a specific pointer to args by specifying index and value
  PROCEDURE, PUBLIC, PASS(obj) :: SetIsSubroutine => obj_SetIsSubroutine
  !! Set the value of isSubroutine
  PROCEDURE, PUBLIC, PASS(obj) :: SetIsFunction => obj_SetIsFunction
  !! Set the value of isFunction
  PROCEDURE, PUBLIC, PASS(obj) :: SetIsAbstract => obj_SetIsAbstract
  !! Set the value of isAbstract
  PROCEDURE, PUBLIC, PASS(obj) :: SetIsGeneric => obj_SetIsGeneric
  !! Set the value of isGeneric
  PROCEDURE, PUBLIC, PASS(obj) :: SetName => obj_SetName
  !! Set the value of name
  PROCEDURE, PUBLIC, PASS(obj) :: SetMd => obj_SetMd
  !! Set the value of Md
  PROCEDURE, PUBLIC, PASS(obj) :: AllocateArgs => obj_AllocateArgs
  !! allocate the fields
  PROCEDURE, PUBLIC, PASS(obj) :: ParseLine1 => obj_ParseLine1
  !! Read the procedure interface first line
  PROCEDURE, PUBLIC, PASS(obj) :: ParseLine2 => obj_ParseLine2
  !! Read the second line of procedure interface
  PROCEDURE, PUBLIC, PASS(obj) :: ParseLine3 => obj_ParseLine3
  !! Read the second last line of the procedure interface
  PROCEDURE, PUBLIC, PASS(obj) :: ParseLine4 => obj_ParseLine4
  !! Read the last line of the procedure interface
  PROCEDURE, PUBLIC, PASS(obj) :: GetIsModuleProcedure => &
    obj_GetIsModuleProcedure
  !! get the isModuleProcedure
END TYPE ProcedureData_

!----------------------------------------------------------------------------
!                                                       ProcedureDataPointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-02
! summary: data type to contain a pointer to ProcedureData_

TYPE :: ProcedureDataPointer_
  TYPE(ProcedureData_), POINTER :: ptr => NULL()
  !! pointer to ProcedureData
END TYPE ProcedureDataPointer_

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-03
! summary: Display the content of ProcedureData_
!
!# Display
!
! This method displays the content of ProcedureData_

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(ProcedureData_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                       GenerateMarkdownDocs@MarkdownMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-04
! summary: Generate markdown docs for data stored inside ProcedureData_
!
!# GenerateMarkdownDocs
!
! Generate markdown docs for data stored inside ProcedureData_.

INTERFACE
  MODULE SUBROUTINE obj_GenerateMarkdownDocs(obj)
    CLASS(ProcedureData_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_GenerateMarkdownDocs
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetName@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-10
! summary: Get name of Procedure

INTERFACE
  MODULE FUNCTION obj_GetName(obj) RESULT(ans)
    CLASS(ProcedureData_), INTENT(IN) :: obj
    TYPE(String) :: ans
  END FUNCTION obj_GetName
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetArgPointer@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-10
! summary: Set a pointer to Args
!
!# SetArgPointer
!
! Set a pointer to `obj%Args(indx)`. Make sure that Args is already
! allocated, and indx is not out of bound.

INTERFACE
  MODULE SUBROUTINE obj_SetArgPointer(obj, indx, val)
    CLASS(ProcedureData_), INTENT(INOUT) :: obj
    !! user defined data type
    INTEGER(I4B), INTENT(IN) :: indx
    !! indx is used in Args, Args(indx)=>val
    TYPE(ProcedureEntry_), TARGET, INTENT(IN) :: val
    !! target of Args(indx)
  END SUBROUTINE obj_SetArgPointer
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

INTERFACE
  MODULE SUBROUTINE obj_SetMethodPointer(obj, indx, val)
    CLASS(ProcedureData_), INTENT(INOUT) :: obj
    !! user defined data type
    INTEGER(I4B), INTENT(IN) :: indx
    !! indx is used in Methods, Methods(indx)=>val
    TYPE(ProcedureEntry_), TARGET, INTENT(IN) :: val
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
  MODULE SUBROUTINE ProcedureEntry_SetName(obj, val)
    CLASS(ProcedureEntry_), INTENT(INOUT) :: obj
    !! user type entry
    TYPE(String), TARGET, INTENT(IN) :: val
    !! value of name
  END SUBROUTINE ProcedureEntry_SetName
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
  MODULE SUBROUTINE ProcedureEntry_SetDoc(obj, val)
    CLASS(ProcedureEntry_), INTENT(INOUT) :: obj
    !! procedure entry data
    TYPE(String), TARGET, INTENT(IN) :: val
    !! value of Doc
  END SUBROUTINE ProcedureEntry_SetDoc
END INTERFACE

!----------------------------------------------------------------------------
!                                                 SetIsSubroutine@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-10
! summary: Set isSubroutine field of ProcedureData_
!
!# SetIsSubroutine
!
! This method sets the isSubroutine field of obj.

INTERFACE
  MODULE SUBROUTINE obj_SetIsSubroutine(obj, val)
    CLASS(ProcedureData_), INTENT(INOUT) :: obj
    !! procedure data
    LOGICAL(LGT), INTENT(IN) :: val
    !! value for isSubroutine
  END SUBROUTINE obj_SetIsSubroutine
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetIsFunction@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-10
! summary: Set isFunction field of ProcedureData_
!
!# SetIsFunction
!
! This method sets the isFunction field of obj.

INTERFACE
  MODULE SUBROUTINE obj_SetIsFunction(obj, val)
    CLASS(ProcedureData_), INTENT(INOUT) :: obj
    !! procedure data
    LOGICAL(LGT), INTENT(IN) :: val
    !! value for isFunction
  END SUBROUTINE obj_SetIsFunction
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetIsAbstract@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-10
! summary: Set isAbstract field of ProcedureData_
!
!# SetIsAbstract
!
! This method sets the isAbstract field of obj.

INTERFACE
  MODULE SUBROUTINE obj_SetIsAbstract(obj, val)
    CLASS(ProcedureData_), INTENT(INOUT) :: obj
    !! procedure data
    LOGICAL(LGT), INTENT(IN) :: val
    !! value for isAbstract
  END SUBROUTINE obj_SetIsAbstract
END INTERFACE

!----------------------------------------------------------------------------
!                                                    SetIsGeneric@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-10
! summary: Set isGeneric field of ProcedureData_
!
!# SetIsGeneric
!
! This method sets the isGeneric field of obj.

INTERFACE
  MODULE SUBROUTINE obj_SetIsGeneric(obj, val)
    CLASS(ProcedureData_), INTENT(INOUT) :: obj
    !! procedure data
    LOGICAL(LGT), INTENT(IN) :: val
    !! value for isGeneric
  END SUBROUTINE obj_SetIsGeneric
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
    CLASS(ProcedureData_), INTENT(INOUT) :: obj
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
! summary: Set md field of ProcedureData
!
!# SetMd
!
! This method sets the md field of obj

INTERFACE
  MODULE SUBROUTINE obj_SetMd(obj, val)
    CLASS(ProcedureData_), INTENT(INOUT) :: obj
    !! user type entry
    TYPE(MarkdownData_), INTENT(IN) :: val
    !! value for md
  END SUBROUTINE obj_SetMd
END INTERFACE

!----------------------------------------------------------------------------
!                                            AllocateArgs@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-10
! summary: Allocates the args
!
!# AllocateArgs
!
! This method allocate the args of obj. Make sure args is not allocated
! before as reallocation is not allowed.

INTERFACE
  MODULE SUBROUTINE obj_AllocateArgs(obj, tsize)
    CLASS(ProcedureData_), INTENT(INOUT) :: obj
    !! user defined data type
    INTEGER(I4B), INTENT(IN) :: tsize
    !! total number of args
  END SUBROUTINE obj_AllocateArgs
END INTERFACE

!----------------------------------------------------------------------------
!                                               ParseLine1@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-11
! summary: Parse the first line of procedure interface
!
!# ParseLine1
!
! Process the first line of procedure interface. it reads the following
! lines.
!
! - template 1
!
!```fortran
!INTERFACE
!```
!
! In this case isGeneric is false, isAbstract is false.
!
!```fortran
!INTERFACE Initiate
!```
!
! In this case isGeneric is true, isAbstract is false.
!
!```fortran
!ABSTRACT INTERFACE
!```
!
! In this case isGeneric is false, isAbstract is true.

INTERFACE
  MODULE SUBROUTINE obj_ParseLine1(obj, aline)
    CLASS(ProcedureData_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: aline
  END SUBROUTINE obj_ParseLine1
END INTERFACE

!----------------------------------------------------------------------------
!                                               ParseLine2@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-11
! summary: Parse the second line of procedure interface
!
!# ParseLine2
!
! Process the second line of procedure interface. it reads the following
! lines.
!
! - template 1
!
!```fortran
!MODULE PROCEDURE METHOD_NAME
!```
!
! In this case isGeneric is true, isAbstract is false.
!
! - template 2
!
!```fortran
!MODULE SUBROUTINE METHOD_NAME
!```
!
! In this case isSubroutine is true. isGeneric can be true of false
! depending on the first line.
!
! - template 3
!
!```fortran
!MODULE FUNCTION METHOD_NAME
!```
!
! In this case isFunction is true. isGeneric can be true of false
! depending on the first line.
!
! - template 4
!
!```fortran
!SUBROUTINE METHOD_NAME
!```
!
! In this case isSubroutine is true. isAbstract is also true.
!
! - template 5
!
!```fortran
!FUNCTION METHOD_NAME
!```
!
! In this case isFunction is true. isAbstract is also true.

INTERFACE
  MODULE SUBROUTINE obj_ParseLine2(obj, aline)
    CLASS(ProcedureData_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: aline
  END SUBROUTINE obj_ParseLine2
END INTERFACE

!----------------------------------------------------------------------------
!                                               ParseLine3@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-11
! summary: Parse the second last line of procedure interface
!
!# ParseLine3
!
! Process the second last line of procedure interface. it reads the following
! lines.
!
! - template 1
!
!```fortran
!END SUBROUTINE
!```
!
! - template 2
!
!```fortran
!END FUNCTION
!```

INTERFACE
  MODULE SUBROUTINE obj_ParseLine3(obj, aline)
    CLASS(ProcedureData_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: aline
  END SUBROUTINE obj_ParseLine3
END INTERFACE

!----------------------------------------------------------------------------
!                                               ParseLine4@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-11
! summary: Parse the last line of procedure interface
!
!# ParseLine4
!
! Process the last line of procedure interface. it reads the following
! lines.
!
!```fortran
!END INTERFACE
!```

INTERFACE
  MODULE SUBROUTINE obj_ParseLine4(obj, aline)
    CLASS(ProcedureData_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: aline
  END SUBROUTINE obj_ParseLine4
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetIsModuleProcedure@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-11
! summary: Get isModuleProcedure
!
!# GetIsModuleProcedure
!
! Get isModuleProcedure from obj

INTERFACE
  MODULE FUNCTION obj_GetIsModuleProcedure(obj) RESULT(ans)
    CLASS(ProcedureData_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_GetIsModuleProcedure
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ProcedureData_Class

