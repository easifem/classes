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
USE GlobalData, ONLY: DFP, I4B, LGT, DOF_FMT, NodesToDOF
USE BaseType, ONLY: FEVariable_
USE AbstractField_Class, ONLY: AbstractField_
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE HDF5File_Class, ONLY: HDF5File_
USE FEDOF_Class, ONLY: FEDOF_, FEDOFPointer_
USE DirichletBC_Class, ONLY: DirichletBC_, DirichletBCPointer_
USE UserFunction_Class, ONLY: UserFunction_
USE TimeFEDOF_Class, ONLY: TimeFEDOF_, TimeFEDOFPointer_

IMPLICIT NONE

PRIVATE

CHARACTER(*), PARAMETER :: modName = "BlockNodeField_Class"
CHARACTER(*), PARAMETER :: myprefix = "BlockNodeField"
INTEGER(I4B), PARAMETER :: mystorageformat = DOF_FMT
INTEGER(I4B), PARAMETER :: myconversion = NodesToDOF

PUBLIC :: BlockNodeFieldPointer_
PUBLIC :: BlockNodeField_
PUBLIC :: SetBlockNodeFieldParam
PUBLIC :: BlockNodeFieldInitiate1
PUBLIC :: BlockNodeFieldInitiate3
PUBLIC :: BlockNodeFieldDeallocate
PUBLIC :: BlockNodeFieldExport

!----------------------------------------------------------------------------
!                                                           BlockNodeField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This nodal field is designed for the multiphysics applications
!
!{!pages/docs-api/BlockNodeField/BlockNodeField_.md!}

TYPE, EXTENDS(AbstractNodeField_) :: BlockNodeField_
  INTEGER(I4B), ALLOCATABLE :: idofs(:)
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethod

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: checkEssentialParam => &
    obj_checkEssentialParam
  !! Check essential parameter

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate by using parameter list

  PROCEDURE, PUBLIC, PASS(obj) :: Initiate3 => obj_Initiate3
  !! Initiate by using parameter list and fedofs

  FINAL :: obj_Final

  ! IO:
  ! @IOMethods

  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import

  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export

  ! SET:
  ! @SetMethods

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set1 => obj_Set1
  !! Set all values to a scalar values
  !! We call set all method

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set2 => obj_Set2
  !! Set all values of obj by using a rank-1 vector

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set3 => obj_Set3
  !! Set a single entry
  !! filter: ivar, idof

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set4 => obj_Set4
  !! Set multiple values to a constant value
  !! Filter: ivar

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set5 => obj_Set5
  !! Set multiple values by using rank-1 vector
  !! Filter: ivar

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set6 => obj_Set6
  !! Set miltiple values to a constant value
  !! Filter: ivar, idof

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set7 => obj_Set7
  !! Set multiple values by using rank-1 vector
  !! Filter: ivar, idof

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set8 => obj_Set8
  !! Set multiple values to a constant value
  !! Filter: ivar, spaceCompo, timeCompo

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set9 => obj_Set9
  !! Set multiple values to a constant value
  !! Filter: ivar, spaceCompo, timeCompo

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set10 => obj_Set10
  !! Set multiple values by using rank-1 vector
  !! Filter: ivar, spaceCompo, timeCompo(:)

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set11 => obj_Set11
  !! Set multiple values to a constant scalar
  !! filter spaceCompo and timeCompo(:)

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set12 => obj_Set12
  !! Set multiple values by using rank-1 vector
  !! Filter: ivar, spaceCompo(:), timeCompo

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set13 => obj_Set13
  !! Set multiple values to a constant scalar
  !! Filter: ivar, spaceCompo(:), timeCompo

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set14 => obj_Set14
  !! Set single entry
  !! filter: ivar, spaceCompo, timeCompo

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set15 => obj_Set15
  !! Set single entry
  !! filter: ivar, spaceCompo, timeCompo(:)

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set16 => obj_Set16
  !! Set single entry
  !! filter: ivar, spaceCompo(:), timeCompo

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set17 => obj_Set17
  !!

  PROCEDURE, PASS(obj) :: Set18 => obj_Set18
  !!

  GENERIC, PUBLIC :: Set => Set1, Set2, Set3, Set4, &
    Set5, Set6, Set7, Set8, Set9, Set10, Set11, &
    Set12, Set13, Set14, Set15, Set16, Set17, Set18

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: ASSIGN => obj_Assign
  !! Set all values to a scalar values

  GENERIC, PUBLIC :: ASSIGNMENT(=) => ASSIGN
  !! Assignment, obj = scalar

  ! GET:
  ! @GetMethods

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get1 => obj_Get1
  !! Set a single value to a scalar value
  !! filter: ivar, idof

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get2 => obj_Get2
  !! Get all the values in a rank-1 vector

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get3 => obj_Get3
  !! Get multiple values
  !! Filter: ivar, idof

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get4 => obj_Get4
  !! Get in FEVariable
  !! Filter: ivar, idof

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get5 => obj_Get5
  !! Get in FEVariable

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get6 => obj_Get6
  !! Get multiple values in a rank-1 vector
  !! Filter: ivar, spaceCompo, timeCompo

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get7 => obj_Get7
  !! Get value in FEVariable
  !! Filter: ivar, spaceCompo, timeCompo

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get8 => obj_Get8

  GENERIC, PUBLIC :: Get => Get1, Get2, Get3, Get4, &
    Get5, Get6, Get7, Get8

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetFEVariable => obj_GetFEVariable

  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix

  ! SET:
  ! @DirichletBCMethods

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: ApplyDirichletBC1 => &
    obj_ApplyDirichletBC1

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: ApplyDirichletBC2 => &
    obj_ApplyDirichletBC2

END TYPE BlockNodeField_

!----------------------------------------------------------------------------
!                                                    BlockNodeFieldPointer_
!----------------------------------------------------------------------------

TYPE :: BlockNodeFieldPointer_
  CLASS(BlockNodeField_), POINTER :: ptr => NULL()
END TYPE BlockNodeFieldPointer_

!----------------------------------------------------------------------------
!                                 SetBlockNodeFieldParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 29 Sept 2021
! summary: Sets the essential parameters to construct an instance of block
! node field
!
!# Introduction
! The size of physicalVarNames, spaceCompo, timeCompo should be the same

INTERFACE
  MODULE SUBROUTINE SetBlockNodeFieldParam(param, name, engine, &
  physicalVarNames, spaceCompo, timeCompo, fieldType, comm, local_n, global_n)
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
!                                      checkEssentialParam@ConstructorMethods
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
!                                                 Initiate@ConstructorMethods
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
  MODULE SUBROUTINE obj_Initiate1(obj, param, fedof, timefedof)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    CLASS(FEDOF_), TARGET, INTENT(IN) :: fedof
    CLASS(TimeFEDOF_), TARGET, OPTIONAL, INTENT(IN) :: timefedof
  END SUBROUTINE obj_Initiate1
END INTERFACE BlockNodeFieldInitiate1

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethods
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
  MODULE SUBROUTINE obj_Initiate3(obj, param, fedof, timefedof)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(FEDOFPointer_), INTENT(IN) :: fedof(:)
    TYPE(TimeFEDOFPointer_), OPTIONAL, INTENT(IN) :: timefedof(:)
  END SUBROUTINE obj_Initiate3
END INTERFACE BlockNodeFieldInitiate3

!----------------------------------------------------------------------------
!                                                    Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(BlockNodeField_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE BlockNodeFieldDeallocate
  MODULE SUBROUTINE obj_Deallocate_ptr_vector(obj)
    TYPE(BlockNodeFieldPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE obj_Deallocate_ptr_vector
END INTERFACE BlockNodeFieldDeallocate

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Imports the content

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, fedof, fedofs, timefedof, &
                               timefedofs)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    CLASS(FEDOF_), TARGET, OPTIONAL, INTENT(IN) :: fedof
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: fedofs(:)
    CLASS(TimeFEDOF_), TARGET, OPTIONAL, INTENT(IN) :: timefedof
    TYPE(TimeFEDOFPointer_), OPTIONAL, INTENT(IN) :: timefedofs(:)
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Export@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-24
! summary:  Export data into HDF5File_

INTERFACE BlockNodeFieldExport
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE BlockNodeFieldExport

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
    !! scalar value
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
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
    !! Size of value should be equal to the size of obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine Sets the single value

INTERFACE
  MODULE SUBROUTINE obj_Set3(obj, globalNode, islocal, VALUE, ivar, idof, &
                             scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, then globalNode is local node number
    REAL(DFP), INTENT(IN) :: VALUE
    !! value
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: idof
    !! degree of freedom number
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine Sets selected values to a scalar value

INTERFACE
  MODULE SUBROUTINE obj_Set4(obj, globalNode, islocal, VALUE, ivar, scale, &
                             addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, then globalNode is local node number
    REAL(DFP), INTENT(IN) :: VALUE
    !! value to be set
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine Sets selected values to a scalar value

INTERFACE
  MODULE SUBROUTINE obj_Set5(obj, globalNode, VALUE, islocal, ivar, scale, &
                             addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, then globalNode is local node number
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! obj = value
    !! set all degrees of freedom of physical variable ivar
    !! size of value should be equal to the degrees of freedo in
    !! ivar.
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine Sets the selected values

INTERFACE
  MODULE SUBROUTINE obj_Set6(obj, globalNode, islocal, VALUE, ivar, idof, &
                             scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, then globalNode is local node number
    REAL(DFP), INTENT(IN) :: VALUE
    !! constant value which will be assigned to the globalnode
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: idof
    !! local degree of freedom number
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine Sets the selected values

INTERFACE
  MODULE SUBROUTINE obj_Set7(obj, globalNode, islocal, VALUE, ivar, idof, &
                             scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, then globalNode is local node number
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! obj = value, size(value) = size(globalNode)
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: idof
    !! local degree of freedom number
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine Sets the selected values

INTERFACE
  MODULE SUBROUTINE obj_Set8(obj, globalNode, islocal, VALUE, ivar, &
                             spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, then globalNode is local node number
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! value to be set
    !! size(value) = size(globalNode)
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component number in the physical variable
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component number in the physical variable
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set8
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine Sets the selected values

INTERFACE
  MODULE SUBROUTINE obj_Set9(obj, globalNode, islocal, VALUE, ivar, &
                             spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, then globalNode is local node number
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component in the physical variable
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component in the physical variable
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set9
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine Sets the selected values

INTERFACE
  MODULE SUBROUTINE obj_Set10(obj, globalNode, islocal, VALUE, ivar, &
                              spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, then globalNode is local node number
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! value to be set
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component in the physical variable
    INTEGER(I4B), INTENT(IN) :: timeCompo(:)
    !! time component in the physical variable
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set10
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine Sets the selected values

INTERFACE
  MODULE SUBROUTINE obj_Set11(obj, globalNode, islocal, VALUE, ivar, &
                              spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, then globalNode is local node number
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component in the physical variable
    INTEGER(I4B), INTENT(IN) :: timeCompo(:)
    !! time component in the physical variable
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set11
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine Sets the selected values

INTERFACE
  MODULE SUBROUTINE obj_Set12(obj, globalNode, islocal, VALUE, ivar, &
                              spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, then globalNode is local node number
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! values, the size should be same as size of spacecompo
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
    !! space component in the physical variable
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component in the physical variable
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set12
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine Sets the selected values

INTERFACE
  MODULE SUBROUTINE obj_Set13(obj, globalNode, islocal, VALUE, ivar, &
                              spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then globalNode is local node number
    REAL(DFP), INTENT(IN) :: VALUE
    !! value
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
    !! space component in the physical variable
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component in the physical variable
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
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
                              spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global node number
    REAL(DFP), INTENT(IN) :: VALUE
    !! value
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component in the physical variable
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component in the physical variable
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
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
                              spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number
    REAL(DFP), INTENT(IN) :: VALUE
    !! value
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component
    INTEGER(I4B), INTENT(IN) :: timeCompo(:)
    !! time components in the physical variable
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
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
                              spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number
    REAL(DFP), INTENT(IN) :: VALUE
    !! value
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
    !! space component numbers in ivar
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component numbers in ivar
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add contribution
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
    !! BlockNodeField_
    CLASS(BlockNodeField_), INTENT(INOUT) :: VALUE
    !! BlockNodeField_
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
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
                              idof_value, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: idof
    !! local degree of freedom number in ivar
    CLASS(AbstractNodeField_), INTENT(IN) :: VALUE
    !! AbstractNodeField_
    INTEGER(I4B), INTENT(IN) :: ivar_value
    !! physical variable number in value
    INTEGER(I4B), INTENT(IN) :: idof_value
    !! local degree of freedom number in value
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
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
  MODULE SUBROUTINE obj_Get1(obj, VALUE, globalNode, islocal, ivar, idof)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE
    !! contant value
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, then globalNode is local node number
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
  MODULE SUBROUTINE obj_Get2(obj, VALUE, tsize)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! Get all values in a rank 1
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of VALUE should be equal to the size of obj
  END SUBROUTINE obj_Get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE obj_Get3(obj, VALUE, tsize, globalNode, islocal, &
                             ivar, idof)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! value
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size written in value
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, then globalNode is local node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: idof
    !! local degree of freedom number
  END SUBROUTINE obj_Get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE obj_Get4(obj, VALUE, globalNode, islocal, ivar, idof)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! NodalVariable
    !! TypeFEVariableScalar
    !! TypeFEVariableSpace
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, then globalNode is local node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: idof
    !! local degree of freedom number
  END SUBROUTINE obj_Get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE obj_Get5(obj, VALUE, globalNode, islocal, ivar)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! NodalVariable
    !! If spaceCompo is greater than 1, then FETypeVector
    !! IF spaceCompo is equal to 1, then FETypeScalar
    !! If timeCompo is equal to 1, then FETypeSpace
    !! If timeCompo is greater than 1, then FETypeSpaceTime
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, then globalNode is local node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number

  END SUBROUTINE obj_Get5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE obj_Get6(obj, VALUE, tsize, globalNode, islocal, ivar, &
                             spaceCompo, timeCompo)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
  END SUBROUTINE obj_Get6
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE obj_Get7(obj, VALUE, globalNode, islocal, ivar, &
                             spaceCompo, timeCompo)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! NodalVariable
    !! TypeFEVariableScalar
    !! TypeFEVariableSpace
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    LOGICAL(LGT), INTENT(IN) :: islocal
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
! summary: Returns values

INTERFACE
  MODULE SUBROUTINE obj_Get8(obj, ivar, idof, VALUE, ivar_value, idof_value)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    CLASS(AbstractNodeField_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B), INTENT(IN) :: ivar_value
    INTEGER(I4B), INTENT(IN) :: idof_value
  END SUBROUTINE obj_Get8
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetFEVariable@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: Set single entry

INTERFACE BlockFieldGetFEVariable
  MODULE SUBROUTINE obj_GetFeVariable(obj, globalNode, islocal, VALUE, ivar)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, then globalNode is local node number
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! NodalVariable
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    !! physical variable number
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
  MODULE SUBROUTINE obj_ApplyDirichletBC1(obj, dbc, times, ivar, extField)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    CLASS(DirichletBC_), INTENT(INOUT) :: dbc
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: extField
  END SUBROUTINE obj_ApplyDirichletBC1
END INTERFACE

!----------------------------------------------------------------------------
!                                               ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 22 Jan 2021
! summary: Apply Dirichlet boundary condition

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC2(obj, dbc, times, ivar, extField)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    TYPE(DirichletBCPointer_), INTENT(INOUT) :: dbc(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: extField
  END SUBROUTINE obj_ApplyDirichletBC2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE BlockNodeField_Class
