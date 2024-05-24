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

MODULE ScalarField_Class
USE GlobalData, ONLY: DFP, I4B, LGT
USE String_Class, ONLY: String
USE BaseType, ONLY: FEVariable_
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE HDF5File_Class, ONLY: HDF5File_
USE DirichletBC_Class, ONLY: DirichletBC_, DirichletBCPointer_
USE UserFunction_Class, ONLY: UserFunction_
USE FEDOF_Class, ONLY: FEDOF_, FEDOFPointer_

IMPLICIT NONE
PRIVATE

CHARACTER(*), PARAMETER :: modName = "ScalarField_Class"
CHARACTER(*), PARAMETER :: myprefix = "ScalarField"

PUBLIC :: ScalarField_
PUBLIC :: ScalarFieldPointer_
PUBLIC :: SetScalarFieldParam
PUBLIC :: ScalarFieldCheckEssentialParam
PUBLIC :: ScalarFieldInitiate1
PUBLIC :: ScalarField
PUBLIC :: ScalarField_Pointer
PUBLIC :: ScalarFieldImport
PUBLIC :: ScalarFieldDeallocate

!----------------------------------------------------------------------------
!                                                              ScalarField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Native vector type
!
!{!pages/docs-api/ScalarField/ScalarField_.md!}

TYPE, EXTENDS(AbstractNodeField_) :: ScalarField_
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: CheckEssentialParam => &
    obj_CheckEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => obj_Initiate1
  FINAL :: obj_Final

  ! SET:
  ! @SetMethods
  PROCEDURE, PASS(obj) :: Set1 => obj_Set1
    !! Set single entry
  PROCEDURE, PASS(obj) :: Set2 => obj_Set2
    !! Set all values to a scalar values
  PROCEDURE, PASS(obj) :: Set3 => obj_Set3
    !! Set all values to a given vector
  PROCEDURE, PASS(obj) :: Set4 => obj_Set4
    !! Set selected values to given scalar
  PROCEDURE, PASS(obj) :: Set5 => obj_Set5
    !! Set selected values to given vector
  PROCEDURE, PASS(obj) :: Set6 => obj_Set6
    !! Set values to a scalar by using triplet
  PROCEDURE, PASS(obj) :: Set7 => obj_Set7
    !! Set values to a vector by using triplet
  PROCEDURE, PASS(obj) :: Set8 => obj_Set8
    !! This method is used for assignment operator
  PROCEDURE, PASS(obj) :: Set9 => obj_Set9
    !! Set selected values using FEVariable
  PROCEDURE, PASS(obj) :: Set10 => obj_Set10
    !! Set selected values using FEVariable
  PROCEDURE, PASS(obj) :: Set11 => obj_Set11
    !! Set selected values using FEVariable
  PROCEDURE, PUBLIC, PASS(obj) :: SetByFunction => obj_SetByFunction
  !! Set scalar field using a function

  GENERIC, PUBLIC :: Set => Set1, Set2, Set3, Set4, &
    Set5, Set6, Set7, Set8, Set9, Set10, Set11

  GENERIC, PUBLIC :: ASSIGNMENT(=) => Set8
    !! Set values to a vector

  ! GET:
  ! @GetMethods
  PROCEDURE, PASS(obj) :: Get1 => obj_Get1
    !! Get single entry
  PROCEDURE, PASS(obj) :: Get2 => obj_Get2
    !! Get all values in Real vector
  PROCEDURE, PASS(obj) :: Get3 => obj_Get3
    !! Get selected values
  PROCEDURE, PASS(obj) :: Get4 => obj_Get4
    !! Get values from triplet
  PROCEDURE, PASS(obj) :: Get5 => obj_Get5
  PROCEDURE, PASS(obj) :: Get6 => obj_Get6
  PROCEDURE, PASS(obj) :: Get7 => obj_Get7
    !! Get selected values in FEVariable
  GENERIC, PUBLIC :: Get => Get1, Get2, Get3, Get4, Get5, Get6, Get7
  !! Get the entries of scalar field
  PROCEDURE, PUBLIC, PASS(obj) :: GetFEVariable => obj_GetFeVariable
  !! Get Finite Element variable
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix

  ! SET:
  ! @DirichletBCMethods
  PROCEDURE, PASS(obj) :: ApplyDirichletBC1 => obj_ApplyDirichletBC1
  PROCEDURE, PASS(obj) :: ApplyDirichletBC2 => obj_ApplyDirichletBC2

  ! IO:
  ! @IOMethods
  !! Apply Dirichlet Boundary Condition
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
END TYPE ScalarField_

!----------------------------------------------------------------------------
!                                                       ScalarFieldPointer_
!----------------------------------------------------------------------------

TYPE :: ScalarFieldPointer_
  CLASS(ScalarField_), POINTER :: ptr => NULL()
END TYPE ScalarFieldPointer_

!----------------------------------------------------------------------------
!                                           SetScalarFieldParam@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Sept 2021
! summary: Set the essential parameters

INTERFACE
  MODULE SUBROUTINE SetScalarFieldParam(param, name, engine, &
                                        fieldType, comm, local_n, global_n)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: name
    !! name of the variable
    CHARACTER(*), INTENT(IN) :: engine
    !! name of the engine
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fieldType
    !! field type
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: comm
    !! communication group
    !! Only needed for parallel environment
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_n
    !! local size of scalar field on each processor
    !! Only needed for parallel environment
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: global_n
    !! global size of scalar field on distributed on processors
    !! Only needed for parallel environment
  END SUBROUTINE SetScalarFieldParam
END INTERFACE

!----------------------------------------------------------------------------
!                                           CheckEssentialParam@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Check the essential parameters in param.

INTERFACE ScalarFieldCheckEssentialParam
  MODULE SUBROUTINE obj_CheckEssentialParam(obj, param)
    CLASS(ScalarField_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_CheckEssentialParam
END INTERFACE ScalarFieldCheckEssentialParam

!----------------------------------------------------------------------------
!                                                      Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This subroutine initiates the ScalarField_ object
!
!# Introduction
!
! This routine initiate the [[ScalarField_]] object.
! `param` contains the information of parameters required to initiate the
! scalar field. There are essential and optional information.
! Essential information are described below.

INTERFACE ScalarFieldInitiate1
  MODULE SUBROUTINE obj_Initiate1(obj, param, fedof)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    CLASS(FEDOF_), TARGET, INTENT(IN) :: fedof
  END SUBROUTINE obj_Initiate1
END INTERFACE ScalarFieldInitiate1

!----------------------------------------------------------------------------
!                                                         Final@Constructor
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(ScalarField_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE ScalarFieldDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj)
    TYPE(ScalarField_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE ScalarFieldDeallocate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE ScalarFieldDeallocate
  MODULE SUBROUTINE obj_Deallocate_ptr_vector(obj)
    TYPE(ScalarFieldPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE obj_Deallocate_ptr_vector
END INTERFACE ScalarFieldDeallocate

!----------------------------------------------------------------------------
!                                                         Vector@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary:         This function returns an instance of [[ScalarField_]]

INTERFACE ScalarField
  MODULE FUNCTION obj_Constructor1(param, fedof) RESULT(Ans)
    TYPE(ParameterList_), INTENT(IN) :: param
    CLASS(FEDOF_), TARGET, INTENT(IN) :: fedof
    TYPE(ScalarField_) :: ans
  END FUNCTION obj_Constructor1
END INTERFACE ScalarField

!----------------------------------------------------------------------------
!                                                 ScalarField_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary:         This function returns an instance of [[ScalarField_]]

INTERFACE ScalarField_Pointer
  MODULE FUNCTION obj_Constructor_1(param, fedof) RESULT(Ans)
    TYPE(ParameterList_), INTENT(IN) :: param
    CLASS(FEDOF_), TARGET, INTENT(IN) :: fedof
    CLASS(ScalarField_), POINTER :: ans
  END FUNCTION obj_Constructor_1
END INTERFACE ScalarField_Pointer

!----------------------------------------------------------------------------
!                                                                Import@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Imports the content

INTERFACE ScalarFieldImport
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, fedof, fedofs)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    CLASS(FEDOF_), TARGET, OPTIONAL, INTENT(IN) :: fedof
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: fedofs(:)
  END SUBROUTINE obj_Import
END INTERFACE ScalarFieldImport

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE obj_Set1(obj, globalNode, islocal, VALUE, scale, &
                             addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if present and true then globalNode is a local node
    REAL(DFP), INTENT(IN) :: VALUE
    !! value
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add contribution
  END SUBROUTINE obj_Set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets all the entries of a scalar field to value

INTERFACE
  MODULE SUBROUTINE obj_Set2(obj, VALUE, scale, addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    !! All values of scalar field will be set to value
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add contribution (add or set)
  END SUBROUTINE obj_Set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Set all the entries by using a fortran vector

INTERFACE
  MODULE SUBROUTINE obj_Set3(obj, VALUE, scale, addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the selected entries

INTERFACE
  MODULE SUBROUTINE obj_Set4(obj, globalNode, islocal, VALUE, scale, &
                             addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global nodes
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, then globalNodes are local nodes
    REAL(DFP), INTENT(IN) :: VALUE
    !! value to be assigned on globalNode
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale (if we are adding)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the selected entries

INTERFACE
  MODULE SUBROUTINE obj_Set5(obj, globalNode, islocal, VALUE, scale, &
                             addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local nodes
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, then globalNodes are local nodes
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! value to be assigned on globalNode
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale (if we are adding)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set

  END SUBROUTINE obj_Set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the selected entries using triplet

INTERFACE
  MODULE SUBROUTINE obj_Set6(obj, istart, iend, stride, islocal, VALUE, &
                             scale, addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: istart
    !! global/local node start
    INTEGER(I4B), INTENT(IN) :: iend
    !! global/local node end
    INTEGER(I4B), INTENT(IN) :: stride
    !! global/local node stride
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, then globalNodes are local nodes
    REAL(DFP), INTENT(IN) :: VALUE
    !! value to be assigned on globalNode
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale (if we are adding)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Set the vector vals using triplet

INTERFACE
  MODULE SUBROUTINE obj_Set7(obj, istart, iend, stride, islocal, VALUE, &
                             scale, addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! Scalar field
    INTEGER(I4B), INTENT(IN) :: istart
    !! global/local node start
    INTEGER(I4B), INTENT(IN) :: iend
    !! global/local node end
    INTEGER(I4B), INTENT(IN) :: stride
    !! global/local node stride
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, then globalNodes are local nodes
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! value to be assigned on globalNode
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale (if we are adding)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set

  END SUBROUTINE obj_Set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: used for assignment operator

INTERFACE
  MODULE SUBROUTINE obj_Set8(obj, VALUE)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    CLASS(ScalarField_), INTENT(IN) :: VALUE
  END SUBROUTINE obj_Set8
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the selected entries using FEVariable

INTERFACE
  MODULE SUBROUTINE obj_Set9(obj, globalNode, islocal, VALUE, scale, &
                             addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! Scalar field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local nodes
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, then globalNodes are local nodes
    TYPE(FEVariable_), INTENT(IN) :: VALUE
    !! Scalar, Nodal, FEVariable (Space or Constant)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale (if we are adding)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set9
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: obj=obj+scalar*obj2

INTERFACE
  MODULE SUBROUTINE obj_Set10(obj, obj2, scale, addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    CLASS(ScalarField_), INTENT(IN) :: obj2
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set10
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-23
! summary: Set

INTERFACE
  MODULE SUBROUTINE obj_Set11(obj, ivar, idof, VALUE, ivar_value, &
                              idof_value, scale, addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    CLASS(AbstractNodeField_), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar_value
    INTEGER(I4B), INTENT(IN) :: idof_value
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set11
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetByFunction@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-24
! summary: This sets the scalar field using a function
!
!# Introduction
!
!@note
!   This routine is valid only for the Lagrange polymials
!@endnote

INTERFACE
  MODULE SUBROUTINE obj_SetByFunction(obj, func, times, ivar, idof, &
                                      spaceCompo, timeCompo)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    CLASS(UserFunction_), INTENT(INOUT) :: func
      !! User function
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! If present then its size should be 1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    !! ivar (not used)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: idof
    !! idof (not used)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo
    !! space component, not used
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo
    !! time component, not used
  END SUBROUTINE obj_SetByFunction
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE obj_Get1(obj, VALUE, globalNode, islocal)
    CLASS(ScalarField_), INTENT(IN) :: obj
    !! Scalar field
    REAL(DFP), INTENT(INOUT) :: VALUE
    !! value to be returned
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if present and true then globalNode is a local node
  END SUBROUTINE obj_Get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns all the entries by using given scalar field

INTERFACE
  MODULE SUBROUTINE obj_Get2(obj, VALUE)
    CLASS(ScalarField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    !! real vector which contains the values stored in scalar field
  END SUBROUTINE obj_Get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE obj_Get3(obj, VALUE, globalNode, islocal)
    CLASS(ScalarField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    !! values to be returned
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local nodes
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, then globalNodes are local nodes
  END SUBROUTINE obj_Get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: returns the value using triplet

INTERFACE
  MODULE SUBROUTINE obj_Get4(obj, VALUE, istart, iend, stride, islocal)
    CLASS(ScalarField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    !! values to be returned
    INTEGER(I4B), INTENT(IN) :: istart
    !! istart of global or local node
    INTEGER(I4B), INTENT(IN) :: iend
    !! end of global or local node
    INTEGER(I4B), INTENT(IN) :: stride
    !! stride of global or local node
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, then globalNodes are local nodes
  END SUBROUTINE obj_Get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: returns the selected values in FEVariable

INTERFACE
  MODULE SUBROUTINE obj_Get5(obj, VALUE, globalNode, islocal)
    CLASS(ScalarField_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! FEVariable, which contains nodal value of scalar
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local nodes
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, then globalNodes are local nodes
  END SUBROUTINE obj_Get5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: returns the selected values in FEVariable

INTERFACE
  MODULE SUBROUTINE obj_Get6(obj, VALUE)
    CLASS(ScalarField_), INTENT(IN) :: obj
    CLASS(ScalarField_), INTENT(INOUT) :: VALUE
  END SUBROUTINE obj_Get6
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Get7(obj, ivar, idof, VALUE, ivar_value, idof_value)
    CLASS(ScalarField_), INTENT(IN) :: obj
    CLASS(AbstractNodeField_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B), INTENT(IN) :: ivar_value
    INTEGER(I4B), INTENT(IN) :: idof_value
  END SUBROUTINE obj_Get7
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetFEVariable@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: Set single entry

INTERFACE ScalarFieldGetFEVariable
  MODULE SUBROUTINE obj_GetFeVariable(obj, globalNode, islocal, VALUE, ivar)
    CLASS(ScalarField_), INTENT(IN) :: obj
    !! scalar field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then globalNode is a local node
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! returned value in fevariable
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    !! physical variable
  END SUBROUTINE obj_GetFeVariable
END INTERFACE ScalarFieldGetFEVariable

!----------------------------------------------------------------------------
!                                                   GetPrefix@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-26
! summary:  Get prefix

INTERFACE
  MODULE FUNCTION obj_GetPrefix(obj) RESULT(ans)
    CLASS(ScalarField_), INTENT(IN) :: obj
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
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    CLASS(DirichletBC_), INTENT(IN) :: dbc
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
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    CLASS(DirichletBCPointer_), INTENT(IN) :: dbc(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: extField
  END SUBROUTINE obj_ApplyDirichletBC2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ScalarField_Class
