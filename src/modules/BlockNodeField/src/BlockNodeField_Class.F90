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
USE BaSetype
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
PUBLIC :: BlockNodeFieldPointer_
PUBLIC :: BlockNodeField_
PUBLIC :: TypeBlockNodeField
PUBLIC :: SetBlockNodeFieldParam
PUBLIC :: BlockNodeFieldInitiate1
PUBLIC :: BlockNodeFieldInitiate3

!----------------------------------------------------------------------------
!                                                           BlockNodeField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This nodal field is designed for the multiphysics applications
!
!{!pages/docs-api/BlockNodeField/BlockNodeField_.md!}

TYPE, EXTENDS(AbstractNodeField_) :: BlockNodeField_
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethod
  PROCEDURE, PUBLIC, PASS(obj) :: checkEssentialParam => &
    & obj_checkEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => obj_Initiate1
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate3 => obj_Initiate3
  FINAL :: obj_Final

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import

  ! SET:
  ! @SetMethods
  PROCEDURE, PASS(obj) :: Set1 => obj_Set1
  !! Set single entry
  PROCEDURE, PASS(obj) :: Set2 => obj_Set2
  !! Set all values to a scalar values
  PROCEDURE, PASS(obj) :: ASSIGN => obj_Assign
  !! Set all values to a scalar values
  GENERIC, PUBLIC :: ASSIGNMENT(=) => ASSIGN
  !! Assignment, obj = scalar
  PROCEDURE, PASS(obj) :: Set3 => obj_Set3
  !! Set all values to a given vector
  PROCEDURE, PASS(obj) :: Set4 => obj_Set4
  !! Set selected values to given scalar
  PROCEDURE, PASS(obj) :: Set5 => obj_Set5
  PROCEDURE, PASS(obj) :: Set6 => obj_Set6
  PROCEDURE, PASS(obj) :: Set7 => obj_Set7
  PROCEDURE, PASS(obj) :: Set8 => obj_Set8
  PROCEDURE, PASS(obj) :: Set9 => obj_Set9
  PROCEDURE, PASS(obj) :: Set10 => obj_Set10
  PROCEDURE, PASS(obj) :: Set11 => obj_Set11
  PROCEDURE, PASS(obj) :: Set12 => obj_Set12
  PROCEDURE, PASS(obj) :: Set13 => obj_Set13
  PROCEDURE, PASS(obj) :: Set14 => obj_Set14
  PROCEDURE, PASS(obj) :: Set15 => obj_Set15
  PROCEDURE, PASS(obj) :: Set16 => obj_Set16
  PROCEDURE, PASS(obj) :: Set17 => obj_Set17
  PROCEDURE, PASS(obj) :: Set18 => obj_Set18
  GENERIC, PUBLIC :: Set => Set1, Set2, Set3, Set4, &
    & Set5, Set6, Set7, Set8, Set9, Set10, Set11, &
    & Set12, Set13, Set14, Set15, Set16, Set17, Set18

  ! GET:
  ! @GetMethods
  PROCEDURE, PASS(obj) :: Get1 => obj_Get1
  PROCEDURE, PASS(obj) :: Get2 => obj_Get2
  PROCEDURE, PASS(obj) :: Get3 => obj_Get3
  PROCEDURE, PASS(obj) :: Get4 => obj_Get4
  PROCEDURE, PASS(obj) :: Get5 => obj_Get5
  PROCEDURE, PASS(obj) :: Get6 => obj_Get6
  PROCEDURE, PASS(obj) :: Get7 => obj_Get7
  PROCEDURE, PASS(obj) :: Get8 => obj_Get8
  PROCEDURE, PASS(obj) :: Get9 => obj_Get9
  GENERIC, PUBLIC :: Get => Get1, Get2, Get3, Get4, &
    & Get5, Get6, Get7, Get8, Get9
  PROCEDURE, PUBLIC, PASS(obj) :: GetFEVariable => obj_GetFEVariable
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix

  ! SET:
  ! @DirichletBCMethods
  PROCEDURE, PASS(obj) :: obj_ApplyDirichletBC1
  PROCEDURE, PASS(obj) :: obj_ApplyDirichletBC2
  GENERIC, PUBLIC :: ApplyDirichletBC => obj_ApplyDirichletBC1, &
    & obj_ApplyDirichletBC2

  ! GET:
  ! @OperatorMethods
  PROCEDURE, PASS(obj) :: IsEqual => obj_IsEqual
  GENERIC, PUBLIC :: OPERATOR(.EQ.) => IsEqual
END TYPE BlockNodeField_

!----------------------------------------------------------------------------
!                                                       TypeBlockNodeField
!----------------------------------------------------------------------------

TYPE(BlockNodeField_), PARAMETER :: TypeBlockNodeField =  &
  & BlockNodeField_(domains=NULL())

!----------------------------------------------------------------------------
!                                                    BlockNodeFieldPointer_
!----------------------------------------------------------------------------

TYPE :: BlockNodeFieldPointer_
  CLASS(BlockNodeField_), POINTER :: ptr => NULL()
END TYPE BlockNodeFieldPointer_

!----------------------------------------------------------------------------
!                                 SetBlockNodeFieldParam@ConstructorMethod
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

!----------------------------------------------------------------------------
!                                      checkEssentialParam@ConstructorMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine checks the essential parameters in param.

INTERFACE
  MODULE SUBROUTINE obj_checkEssentialParam(obj, param)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_checkEssentialParam
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
! [[BlockNodeField_Class::SetBloclNodeFieldParam]] routine.
!@endnote
!
!!@note
! This routine calls [[BlockNodeField_Class::BlockNodeFieldInitiate3]]
! routine.
!@endnote

INTERFACE BlockNodeFieldInitiate1
  MODULE SUBROUTINE obj_Initiate1(obj, param, dom)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE obj_Initiate1
END INTERFACE BlockNodeFieldInitiate1

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
! [[BlockNodeField_::SetBlockNodeFieldParam]]
! - The size of `dom` should be equal to the number of physical variables
! present in the block node field.
! - `dom` contains the pointer to [[Domain_]] class.

INTERFACE BlockNodeFieldInitiate3
  MODULE SUBROUTINE obj_Initiate3(obj, param, dom)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(DomainPointer_), TARGET, INTENT(IN) :: dom(:)
  END SUBROUTINE obj_Initiate3
END INTERFACE BlockNodeFieldInitiate3

!----------------------------------------------------------------------------
!                                                    Final@ConstructorMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(BlockNodeField_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Imports the content

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, dom, domains)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    TYPE(Domain_), TARGET, OPTIONAL, INTENT(IN) :: dom
    TYPE(DomainPointer_), TARGET, OPTIONAL, INTENT(IN) :: domains(:)
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine Sets all values to a scalar

INTERFACE
  MODULE SUBROUTINE obj_Set1(obj, VALUE, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine Sets all values to val

INTERFACE
  MODULE SUBROUTINE obj_Set2(obj, VALUE, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine Sets the single value

INTERFACE
  MODULE SUBROUTINE obj_Set3(obj, globalNode, VALUE, ivar, idof, &
    & scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine Sets selected values to a scalar value

INTERFACE
  MODULE SUBROUTINE obj_Set4(obj, globalNode, VALUE, ivar, scale, &
    & addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine Sets selected values to a scalar value

INTERFACE
  MODULE SUBROUTINE obj_Set5(obj, globalNode, VALUE, ivar, scale, &
    & addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine Sets the selected values

INTERFACE
  MODULE SUBROUTINE obj_Set6(obj, globalNode, VALUE, ivar, idof, &
    & scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine Sets the selected values

INTERFACE
  MODULE SUBROUTINE obj_Set7(obj, globalNode, VALUE, ivar, idof, &
    & scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine Sets the selected values

INTERFACE
  MODULE SUBROUTINE obj_Set8(obj, globalNode, VALUE, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set8
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine Sets the selected values

INTERFACE
  MODULE SUBROUTINE obj_Set9(obj, globalNode, VALUE, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set9
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine Sets the selected values

INTERFACE
  MODULE SUBROUTINE obj_Set10(obj, globalNode, VALUE, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set10
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine Sets the selected values

INTERFACE
  MODULE SUBROUTINE obj_Set11(obj, globalNode, VALUE, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set11
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine Sets the selected values

INTERFACE
  MODULE SUBROUTINE obj_Set12(obj, globalNode, VALUE, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set12
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine Sets the selected values

INTERFACE
  MODULE SUBROUTINE obj_Set13(obj, globalNode, VALUE, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set13
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine Sets the selected values

INTERFACE
  MODULE SUBROUTINE obj_Set14(obj, globalNode, VALUE, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set14
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine Sets the selected values

INTERFACE
  MODULE SUBROUTINE obj_Set15(obj, globalNode, VALUE, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set15
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine Sets the selected values

INTERFACE
  MODULE SUBROUTINE obj_Set16(obj, globalNode, VALUE, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set16
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine Sets the selected entries

INTERFACE
  MODULE SUBROUTINE obj_Set17(obj, VALUE, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    CLASS(BlockNodeField_), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set17
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-29
! summary: Setvalues

INTERFACE
  MODULE SUBROUTINE obj_Set18(obj, ivar, idof, VALUE, ivar_value, &
    & idof_value, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    CLASS(AbstractNodeField_), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar_value
    INTEGER(I4B), INTENT(IN) :: idof_value
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set18
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine Sets all values to a scalar

INTERFACE
  MODULE SUBROUTINE obj_Assign(obj, VALUE)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE obj_Assign
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE obj_Get1(obj, VALUE, globalNode, ivar, idof)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
  !! physical variable number
    INTEGER(I4B), INTENT(IN) :: idof
  !! degree of freedom number
  END SUBROUTINE obj_Get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: returns all the values

INTERFACE
  MODULE SUBROUTINE obj_Get2(obj, VALUE)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
  END SUBROUTINE obj_Get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE obj_Get3(obj, VALUE, globalNode, ivar, idof)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE obj_Get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE obj_Get4(obj, VALUE, istart, iend, stride,  &
    & ivar, idof)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE obj_Get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE obj_Get5(obj, VALUE, globalNode, &
    & ivar, idof)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! NodalVariable
    !! TypeFEVariableScalar
    !! TypeFEVariableSpace
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE obj_Get5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE obj_Get6(obj, VALUE, globalNode, ivar)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! NodalVariable
    !! If spaceCompo is greater than 1, then FETypeVector
    !! IF spaceCompo is equal to 1, then FETypeScalar
    !! If timeCompo is equal to 1, then FETypeSpace
    !! If timeCompo is greater than 1, then FETypeSpaceTime
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
  END SUBROUTINE obj_Get6
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE obj_Get7(obj, VALUE, globalNode, ivar, &
    & spaceCompo, timeCompo)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
  END SUBROUTINE obj_Get7
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE obj_Get8(obj, VALUE, globalNode, ivar, &
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
  END SUBROUTINE obj_Get8
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: Returns values

INTERFACE
  MODULE SUBROUTINE obj_Get9(obj, ivar, idof, VALUE, ivar_value, idof_value)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    CLASS(AbstractNodeField_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B), INTENT(IN) :: ivar_value
    INTEGER(I4B), INTENT(IN) :: idof_value
  END SUBROUTINE obj_Get9
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetFEVariable@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: Set single entry

INTERFACE BlockFieldGetFEVariable
  MODULE SUBROUTINE obj_GetFeVariable(obj, globalNode, VALUE, ivar)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
  END SUBROUTINE obj_GetFeVariable
END INTERFACE BlockFieldGetFEVariable

!----------------------------------------------------------------------------
!                                                   GetPrefix@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-26
! summary:  Get prefix

INTERFACE
  MODULE FUNCTION obj_GetPrefix(obj) RESULT(ans)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!                                               ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 22 Jan 2021
! summary: Apply Dirichlet boundary condition

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC1(obj, dbc, ivar)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    CLASS(DirichletBC_), INTENT(IN) :: dbc
    INTEGER(I4B), INTENT(IN) :: ivar
  END SUBROUTINE obj_ApplyDirichletBC1
END INTERFACE

!----------------------------------------------------------------------------
!                                               ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 22 Jan 2021
! summary: Apply Dirichlet boundary condition

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC2(obj, dbc, ivar)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    CLASS(DirichletBCPointer_), INTENT(IN) :: dbc(:)
    INTEGER(I4B), INTENT(IN) :: ivar
  END SUBROUTINE obj_ApplyDirichletBC2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       EQ@OperatorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_IsEqual(obj, obj2) RESULT(Ans)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    CLASS(BlockNodeField_), INTENT(IN) :: obj2
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsEqual
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE BlockNodeField_Class
