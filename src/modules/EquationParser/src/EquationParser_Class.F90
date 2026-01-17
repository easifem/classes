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

! This code is modified form of the original code located at
! https://github.com/jacopo-chevallard/FortranParser/
!
! Copyright (c) 2000-2008, Roland Schmehl. All rights reserved.
!
! This software is distributable under the BSD license. See the terms of the
! BSD license in the documentation provided with this software.
!
! Fortran 2008 function parser
!
! This is an OOP Fortran 2008 version of the original fparser by
! Roland Schmehl. This simple class
! wrapping of the original fparser has been developed by Jacopo Chevallard,
! and it is available on
! the GitHub repository https://github.com/jacopo-chevallard/FortranParser.
!
! For comments and bug reports, please open an issue on
! https://github.com/jacopo-chevallard/FortranParser/issues
!
! This function parser module is intended for applications
! where a set of mathematical
! fortran-style expressions is specified at runtime and is then
! evaluated for a large
! number of variable values. This is done by compiling the set of
! function strings
! into byte code, which is interpreted efficiently for the various
! variable values.
!
! The source code of the original fparser is
! available from http://fparser.sourceforge.net
!
! Please send comments, corrections or questions
! realtive to the original fparser to its author:
! Roland Schmehl <roland.schmehl@alumni.uni-karlsruhe.de>
!
! The function parser concept is based on a C++ class library written by
! Juha Nieminen <warp@iki.fi> available from
! http://warp.povusers.org/FunctionParser/
!
! The original code is modified to fit into EASIFEM library structure
! and coding standards. the modification is peformed so that
! the code can be used from userfunction class.
! if you are looking for equation parser only please refer to the
! original repository.
! we are thankful to the original author Jacopo Chevallard for creating
! and sharing the code under open source license.

MODULE EquationParser_Class
USE GlobalData, ONLY: DFP, I4B, LGT
USE BaseType, ONLY: TypeUserFunctionOpt
USE ExceptionHandler_Class, ONLY: e
USE TxtFile_Class, ONLY: TxtFile_
USE tomlf, ONLY: toml_table

IMPLICIT NONE

PRIVATE

!! We need modName in checking syntax error even in non debug mode
CHARACTER(*), PARAMETER :: modName = "EquationParser"

PUBLIC :: EquationParser
PUBLIC :: EquationParser_Pointer
PUBLIC :: EquationParser_
PUBLIC :: EquationParserPointer_

!----------------------------------------------------------------------------
!                                                            EquationParser_
!----------------------------------------------------------------------------

TYPE :: EquationParser_
  PRIVATE

  INTEGER(I4B), ALLOCATABLE :: byteCode(:)
  INTEGER(I4B) :: byteCodeSize = 0
  REAL(DFP), ALLOCATABLE :: immed(:)
  INTEGER(I4B) :: immedSize = 0
  REAL(DFP), ALLOCATABLE :: stack(:)
  INTEGER(I4B) :: stackSize = 0
  INTEGER(I4B) :: stackPtr = 0
  CHARACTER(len=TypeUserFunctionOpt%maxlen) :: funcString = ''
  CHARACTER(len=TypeUserFunctionOpt%maxlen) :: funcStringOrig = ''
  CHARACTER(len=TypeUserFunctionOpt%maxVarLen), ALLOCATABLE :: &
    variableNames(:)

CONTAINS

  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate the data stored
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  !! Initialize equation parser object
  PROCEDURE, PUBLIC, PASS(obj) :: Evaluate => obj_Evaluate
  !! Evaluate compiled bytecode for given variable values
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content of equation parser object
  PROCEDURE, PASS(obj) :: Parse => obj_Parse
  !! Parse function string and compile it into bytecode
  PROCEDURE, PASS(obj) :: Compile => obj_Compile
  !! Compile function string into bytecode
  PROCEDURE, PASS(obj) :: AddCompiledByte => obj_AddCompiledByte
  !! Add compiled byte to bytecode
  PROCEDURE, PASS(obj) :: CompileSubstr => obj_CompileSubstr
  !! Compile substring of function string into bytecode
  PROCEDURE, PASS(obj) :: MathItemIndex => obj_MathItemIndex
  !! Return math item index, if item is real number,
  PROCEDURE, PASS(obj) :: CheckSyntax => obj_CheckSyntax
  !! Check syntax of function string

  ! IO:
  ! @TomlMethods
  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  PROCEDURE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, &
    ImportFromToml2
  !! Import equation parser from toml
END TYPE EquationParser_

!----------------------------------------------------------------------------
!                                                      EquationParserPointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: a vector of pointers to EquationParser_ objects

TYPE :: EquationParserPointer_
  TYPE(EquationParser_), POINTER :: ptr => NULL()
END TYPE EquationParserPointer_

!----------------------------------------------------------------------------
!                                                          Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-14
! summary:  Deallocate the data stored in the equation parser object

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(EquationParser_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-14
! summary: Constructor method for EquationParser class

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, funcStr, var)
    CLASS(EquationParser_), INTENT(INOUT) :: obj
  !! equation parser object to be initialized
    CHARACTER(LEN=*), INTENT(IN) :: FuncStr
  !! Function string
    CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: Var
  !! Array with variable names
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                             Constructor@ConstrtuctorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-14
! summary: constructor function

INTERFACE
  MODULE FUNCTION Constructor1(funcStr, var) RESULT(ans)
    CHARACTER(LEN=*), INTENT(IN) :: funcStr
    !! Function string
    CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: var
    !! Array with variable names
    TYPE(EquationParser_) :: ans
    !! equation parser object
  END FUNCTION Constructor1
END INTERFACE

INTERFACE EquationParser
  PROCEDURE Constructor1
END INTERFACE EquationParser

!----------------------------------------------------------------------------
!                                             Constructor@ConstrtuctorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-14
! summary: constructor function

INTERFACE
  MODULE FUNCTION Constructor_1(funcStr, var) RESULT(ans)
    CHARACTER(LEN=*), INTENT(IN) :: funcStr
    !! Function string
    CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: var
    !! Array with variable names
    TYPE(EquationParser_), POINTER :: ans
    !! equation parser object
  END FUNCTION Constructor_1
END INTERFACE

INTERFACE EquationParser_Pointer
  PROCEDURE Constructor_1
END INTERFACE EquationParser_Pointer

!----------------------------------------------------------------------------
!                                                             Display@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-15
! summary: Display the content of equation parser object

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(EquationParser_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Parse@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-14
! summary:  Parse ith function string FuncStr and compile it into bytecode

INTERFACE
  MODULE SUBROUTINE obj_Parse(obj)
    CLASS(EquationParser_) :: obj
  END SUBROUTINE obj_Parse
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Evaluate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-14
! summary: Evaluate bytecode of ith function for the values passed in
! array Val(:)

INTERFACE
  MODULE FUNCTION obj_Evaluate(obj, val) RESULT(ans)
    CLASS(EquationParser_), INTENT(INOUT) :: obj
    REAL(DFP), DIMENSION(:), INTENT(IN) :: val
    ! Variable values, arguments
    REAL(DFP) :: ans
  END FUNCTION obj_Evaluate
END INTERFACE

!----------------------------------------------------------------------------
!                                                        CheckSyntax@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-14
! summary: Check syntax of function string,  returns 0 if syntax is ok

INTERFACE
  MODULE SUBROUTINE obj_CheckSyntax(obj)
    CLASS(EquationParser_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_CheckSyntax
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-14
! summary: Compile i-th function string F into bytecode

INTERFACE
  MODULE SUBROUTINE obj_Compile(obj)
    CLASS(EquationParser_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Compile
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-14
! summary:  Add compiled byte to bytecode

INTERFACE
  MODULE SUBROUTINE obj_AddCompiledByte(obj, b)
    CLASS(EquationParser_) :: obj
    INTEGER(I4B), INTENT(IN) :: b
    !! Value of byte to be added
  END SUBROUTINE obj_AddCompiledByte
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-14
! summary:  Return math item index, if item is real number,
!           enter it into Comp-structure

INTERFACE
  MODULE FUNCTION obj_MathItemIndex(obj, b, l) RESULT(ans)
    ! Return math item index, if item is real number, enter it into Comp-structure
    CLASS(EquationParser_), INTENT(INOUT) :: obj
    INTEGER, INTENT(IN) :: b, l ! First and last pos. of substring
    INTEGER(I4B) :: ans ! Byte value of math item
  END FUNCTION obj_MathItemIndex
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE RECURSIVE SUBROUTINE obj_CompileSubstr(obj, b, l)
    ! Compile i-th function string funcString into bytecode
    CLASS(EquationParser_) :: obj
    INTEGER, INTENT(IN) :: b, l
    ! Begin and end position substring
  END SUBROUTINE obj_CompileSubstr
END INTERFACE

!----------------------------------------------------------------------------
!                                                  ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate param from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(EquationParser_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                  ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate kernel from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2( &
    obj, tomlName, afile, filename, printToml)
    CLASS(EquationParser_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE EquationParser_Class
