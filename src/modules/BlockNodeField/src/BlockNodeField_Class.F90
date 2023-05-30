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

!> authors: Vikas Sharma, Ph. D.
! date: 28 June 2021
! summary: Scalar field data type is defined

MODULE BlockNodeField_Class
USE GlobalData
USE BaseType
USE String_Class
USE AbstractField_Class
USE AbstractNodeField_Class
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE HDF5File_Class
USE Domain_Class
USE DirichletBC_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "BlockNodeField_Class"
CHARACTER(*), PARAMETER :: myprefix = "BlockNodeField"

!----------------------------------------------------------------------------
!                                                           BlockNodeField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This nodal field is designed for the multiphysics applications
!
!{!pages/BlockNodeField_.md}

TYPE, EXTENDS(AbstractNodeField_) :: BlockNodeField_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: checkEssentialParam => &
    & bnField_checkEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => bnField_Initiate1
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate3 => bnField_Initiate3
  FINAL :: bnField_Final
  !
  ! @IOMethods
  !
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => bnField_Import
  !
  ! @SetMethods
  !
  PROCEDURE, PASS(obj) :: set1 => bnField_set1
    !! set single entry
  PROCEDURE, PASS(obj) :: set2 => bnField_set2
    !! set all values to a scalar values
  PROCEDURE, PASS(obj) :: ASSIGN => bnField_assign
    !! set all values to a scalar values
  GENERIC, PUBLIC :: ASSIGNMENT(=) => ASSIGN
    !! Assignment, obj = scalar
  PROCEDURE, PASS(obj) :: set3 => bnField_set3
    !! set all values to a given vector
  PROCEDURE, PASS(obj) :: set4 => bnField_set4
    !! set selected values to given scalar
  PROCEDURE, PASS(obj) :: set5 => bnField_set5
  PROCEDURE, PASS(obj) :: set6 => bnField_set6
  PROCEDURE, PASS(obj) :: set7 => bnField_set7
  PROCEDURE, PASS(obj) :: set8 => bnField_set8
  PROCEDURE, PASS(obj) :: set9 => bnField_set9
  PROCEDURE, PASS(obj) :: set10 => bnField_set10
  PROCEDURE, PASS(obj) :: set11 => bnField_set11
  PROCEDURE, PASS(obj) :: set12 => bnField_set12
  PROCEDURE, PASS(obj) :: set13 => bnField_set13
  PROCEDURE, PASS(obj) :: set14 => bnField_set14
  PROCEDURE, PASS(obj) :: set15 => bnField_set15
  PROCEDURE, PASS(obj) :: set16 => bnField_set16
  PROCEDURE, PASS(obj) :: set17 => bnField_set17
  PROCEDURE, PASS(obj) :: set18 => bnField_set18
  GENERIC, PUBLIC :: set => set1, set2, set3, set4, &
    & set5, set6, set7, set8, set9, set10, set11, &
    & set12, set13, set14, set15, set16, set17, set18
  !
  ! @GetMethods
  !
  PROCEDURE, PASS(obj) :: get1 => bnField_get1
  PROCEDURE, PASS(obj) :: get2 => bnField_get2
  PROCEDURE, PASS(obj) :: get3 => bnField_get3
  PROCEDURE, PASS(obj) :: get4 => bnField_get4
  PROCEDURE, PASS(obj) :: get5 => bnField_get5
  PROCEDURE, PASS(obj) :: get6 => bnField_get6
  PROCEDURE, PASS(obj) :: get7 => bnField_get7
  PROCEDURE, PASS(obj) :: get8 => bnField_get8
  PROCEDURE, PASS(obj) :: get9 => bnField_get9
  GENERIC, PUBLIC :: get => get1, get2, get3, get4, &
    & get5, get6, get7, get8, get9

  PROCEDURE, PASS(obj) :: bnField_applyDirichletBC1
  PROCEDURE, PASS(obj) :: bnField_applyDirichletBC2
  GENERIC, PUBLIC :: applyDirichletBC => &
    & bnField_applyDirichletBC1, &
    & bnField_applyDirichletBC2
  !
  ! @Operator
  !
  PROCEDURE, PASS(obj) :: isEqual => bnField_isEqual
  GENERIC, PUBLIC :: OPERATOR(.EQ.) => isEqual
END TYPE BlockNodeField_

PUBLIC :: BlockNodeField_

TYPE(BlockNodeField_), PARAMETER, PUBLIC :: TypeBlockNodeField = &
  & BlockNodeField_(domains=NULL())

!----------------------------------------------------------------------------
!                                                    BlockNodeFieldPointer_
!----------------------------------------------------------------------------

TYPE :: BlockNodeFieldPointer_
  CLASS(BlockNodeField_), POINTER :: ptr => NULL()
END TYPE BlockNodeFieldPointer_

PUBLIC :: BlockNodeFieldPointer_

!----------------------------------------------------------------------------
!                                 setBlockNodeFieldParam@ConstructorMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 29 Sept 2021
! summary: Sets the essential parameters to construct an instance of block
! node field
!
!# Introduction
! The size of physicalVarNames, spaceCompo, timeCompo should be the same

INTERFACE
  MODULE SUBROUTINE SetBlockNodeFieldParam(param, &
    & name, &
    & engine, &
    & physicalVarNames, &
    & spaceCompo, &
    & timeCompo, &
    & fieldType, &
    & comm, &
    & local_n, &
    & global_n)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    !! Options to create [[BlockNodeField_]] will be stored in param
    CHARACTER(*), INTENT(IN) :: name
    !! Name of the block node field
    CHARACTER(*), INTENT(IN) :: engine
    !! Name of the engine
    CHARACTER(*), INTENT(IN) :: physicalVarNames(:)
    !! Names of the physical variables
    INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
    !! Space components in each physical variable
    INTEGER(I4B), INTENT(IN) :: timeCompo(:)
    !! Time component in each physical variable
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fieldType
    !! fieldType can be following
    !! FIELD_TYPE_NORMAL <-- DEFAULT
    !! FIELD_TYPE_CONSTANT
    !! FIELD_TYPE_CONSTANT_SPACE
    !! FIELD_TYPE_CONSTANT_TIME
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: comm
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: global_n
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_n
  END SUBROUTINE SetBlockNodeFieldParam
END INTERFACE

PUBLIC :: SetBlockNodeFieldParam

!----------------------------------------------------------------------------
!                                      checkEssentialParam@ConstructorMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine checks the essential parameters in param.

INTERFACE
  MODULE SUBROUTINE bnField_checkEssentialParam(obj, param)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE bnField_checkEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This subroutine initiates the BlockNodeField_ object
!
!# Introduction
!
! - This routine initiates the [[BlockNodeField_]] object.
! - `param` contains the information of parameters required to initiate the
! Block node field.
! - `dom`, represents the domain for all the variables
! - If we use this routine then all the physical variable are defined
! over the same computation domain
!
!@note
! `param` should be constructed by calling
! [[BlockNodeField_Class::setBloclNodeFieldParam]] routine.
!@endnote

INTERFACE
  MODULE SUBROUTINE bnField_Initiate1(obj, param, dom)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE bnField_Initiate1
END INTERFACE

INTERFACE BlockNodeFieldInitiate1
  MODULE PROCEDURE bnField_Initiate1
END INTERFACE BlockNodeFieldInitiate1

PUBLIC :: BlockNodeFieldInitiate1

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This subroutine initiates the BlockNodeField_ object
!
!# Introduction
!
! This routine initiate the [[BlockNodeField_]] object.
! `param` contains the information of parameters required to initiate the
! instance of BlockNodeField_ .
!
! - It is better to make `param` by calling
! [[BlockNodeField_::setBlockNodeFieldParam]]
! - The size of `dom` should be equal to the number of physical variables
! present in the block node field.
! - `dom` contains the pointer to [[Domain_]] class.

INTERFACE
  MODULE SUBROUTINE bnField_Initiate3(obj, param, dom)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(DomainPointer_), TARGET, INTENT(IN) :: dom(:)
  END SUBROUTINE bnField_Initiate3
END INTERFACE

INTERFACE BlockNodeFieldInitiate3
  MODULE PROCEDURE bnField_Initiate3
END INTERFACE BlockNodeFieldInitiate3

PUBLIC :: BlockNodeFieldInitiate3

!----------------------------------------------------------------------------
!                                                    Final@ConstructorMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE bnField_Final(obj)
    TYPE(BlockNodeField_), INTENT(INOUT) :: obj
  END SUBROUTINE bnField_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Imports the content

INTERFACE
  MODULE SUBROUTINE bnField_Import(obj, hdf5, group, dom, domains)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    TYPE(Domain_), TARGET, OPTIONAL, INTENT(IN) :: dom
    TYPE(DomainPointer_), TARGET, OPTIONAL, INTENT(IN) :: domains(:)
  END SUBROUTINE bnField_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine sets all values to a scalar

INTERFACE
  MODULE SUBROUTINE bnField_set1(obj, VALUE, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE bnField_set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine sets all values to val

INTERFACE
  MODULE SUBROUTINE bnField_set2(obj, VALUE, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE bnField_set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine sets the single value

INTERFACE
  MODULE SUBROUTINE bnField_set3(obj, globalNode, VALUE, ivar, idof, &
    & scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE bnField_set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine sets selected values to a scalar value

INTERFACE
  MODULE SUBROUTINE bnField_set4(obj, globalNode, VALUE, ivar, scale, &
    & addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE bnField_set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine sets selected values to a scalar value

INTERFACE
  MODULE SUBROUTINE bnField_set5(obj, globalNode, VALUE, ivar, scale, &
    & addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE bnField_set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine sets the selected values

INTERFACE
  MODULE SUBROUTINE bnField_set6(obj, globalNode, VALUE, ivar, idof, &
    & scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE bnField_set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine sets the selected values

INTERFACE
  MODULE SUBROUTINE bnField_set7(obj, globalNode, VALUE, ivar, idof, &
    & scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE bnField_set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine sets the selected values

INTERFACE
  MODULE SUBROUTINE bnField_set8(obj, globalNode, VALUE, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE bnField_set8
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine sets the selected values

INTERFACE
  MODULE SUBROUTINE bnField_set9(obj, globalNode, VALUE, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE bnField_set9
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine sets the selected values

INTERFACE
  MODULE SUBROUTINE bnField_set10(obj, globalNode, VALUE, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE bnField_set10
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine sets the selected values

INTERFACE
  MODULE SUBROUTINE bnField_set11(obj, globalNode, VALUE, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE bnField_set11
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine sets the selected values

INTERFACE
  MODULE SUBROUTINE bnField_set12(obj, globalNode, VALUE, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE bnField_set12
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine sets the selected values

INTERFACE
  MODULE SUBROUTINE bnField_set13(obj, globalNode, VALUE, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE bnField_set13
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine sets the selected values

INTERFACE
  MODULE SUBROUTINE bnField_set14(obj, globalNode, VALUE, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE bnField_set14
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine sets the selected values

INTERFACE
  MODULE SUBROUTINE bnField_set15(obj, globalNode, VALUE, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE bnField_set15
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine sets the selected values

INTERFACE
  MODULE SUBROUTINE bnField_set16(obj, globalNode, VALUE, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE bnField_set16
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine sets the selected entries

INTERFACE
  MODULE SUBROUTINE bnField_set17(obj, obj2, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    CLASS(BlockNodeField_), INTENT(IN) :: obj2
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE bnField_set17
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-29
! summary: Setvalues

INTERFACE
  MODULE SUBROUTINE bnField_set18(obj, ivar, idof, VALUE, ivar_value, &
    & idof_value, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    CLASS(AbstractNodeField_), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar_value
    INTEGER(I4B), INTENT(IN) :: idof_value
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE bnField_set18
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine sets all values to a scalar

INTERFACE
  MODULE SUBROUTINE bnField_assign(obj, VALUE)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE bnField_assign
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE bnField_get1(obj, VALUE, globalNode, ivar, idof)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
  !! physical variable number
    INTEGER(I4B), INTENT(IN) :: idof
  !! degree of freedom number
  END SUBROUTINE bnField_get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: returns all the values

INTERFACE
  MODULE SUBROUTINE bnField_get2(obj, VALUE)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
  END SUBROUTINE bnField_get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE bnField_get3(obj, VALUE, globalNode, ivar, idof)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE bnField_get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE bnField_get4(obj, VALUE, istart, iend, stride,  &
    & ivar, idof)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE bnField_get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE bnField_get5(obj, VALUE, globalNode, &
    & ivar, idof)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! NodalVariable
    !! TypeFEVariableScalar
    !! TypeFEVariableSpace
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE bnField_get5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE bnField_get6(obj, VALUE, globalNode, ivar)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! NodalVariable
    !! If spaceCompo is greater than 1, then FETypeVector
    !! IF spaceCompo is equal to 1, then FETypeScalar
    !! If timeCompo is equal to 1, then FETypeSpace
    !! If timeCompo is greater than 1, then FETypeSpaceTime
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
  END SUBROUTINE bnField_get6
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE bnField_get7(obj, VALUE, globalNode, ivar, &
    & spaceCompo, timeCompo)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
  END SUBROUTINE bnField_get7
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE bnField_get8(obj, VALUE, globalNode, ivar, &
    & spaceCompo, timeCompo)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! NodalVariable
    !! TypeFEVariableScalar
    !! TypeFEVariableSpace
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
  END SUBROUTINE bnField_get8
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: Returns values

INTERFACE
MODULE SUBROUTINE bnField_get9(obj, ivar, idof, VALUE, ivar_value, idof_value)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    CLASS(AbstractNodeField_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B), INTENT(IN) :: ivar_value
    INTEGER(I4B), INTENT(IN) :: idof_value
  END SUBROUTINE bnField_get9
END INTERFACE

!----------------------------------------------------------------------------
!                                               applyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 22 Jan 2021
! summary: Apply Dirichlet boundary condition

INTERFACE
  MODULE SUBROUTINE bnField_applyDirichletBC1(obj, dbc, ivar)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    CLASS(DirichletBC_), INTENT(IN) :: dbc
    INTEGER(I4B), INTENT(IN) :: ivar
  END SUBROUTINE bnField_applyDirichletBC1
END INTERFACE

!----------------------------------------------------------------------------
!                                               applyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 22 Jan 2021
! summary: Apply Dirichlet boundary condition

INTERFACE
  MODULE SUBROUTINE bnField_applyDirichletBC2(obj, dbc, ivar)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    CLASS(DirichletBCPointer_), INTENT(IN) :: dbc(:)
    INTEGER(I4B), INTENT(IN) :: ivar
  END SUBROUTINE bnField_applyDirichletBC2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       EQ@OperatorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION bnField_isEqual(obj, obj2) RESULT(Ans)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    CLASS(BlockNodeField_), INTENT(IN) :: obj2
    LOGICAL(LGT) :: ans
  END FUNCTION bnField_isEqual
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE BlockNodeField_Class
