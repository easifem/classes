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
USE GlobalData
USE String_Class
USE BaSetype
USE AbstractField_Class
USE AbstractNodeField_Class
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE HDF5File_Class
USE VTKFile_Class
USE Domain_Class
USE DirichletBC_Class
USE FiniteElement_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "ScalarField_Class"
CHARACTER(*), PARAMETER :: myprefix = "ScalarField"
PUBLIC :: ScalarField_
PUBLIC :: ScalarFieldPointer_
PUBLIC :: SetScalarFieldParam
PUBLIC :: sField_CheckEssentialParam
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
    & sField_CheckEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => sField_Initiate1
  FINAL :: sField_Final

  ! SET:
  ! @SetMethods
  PROCEDURE, PASS(obj) :: Set1 => sField_Set1
    !! Set single entry
  PROCEDURE, PASS(obj) :: Set2 => sField_Set2
    !! Set all values to a scalar values
  PROCEDURE, PASS(obj) :: Set3 => sField_Set3
    !! Set all values to a given vector
  PROCEDURE, PASS(obj) :: Set4 => sField_Set4
    !! Set selected values to given scalar
  PROCEDURE, PASS(obj) :: Set5 => sField_Set5
    !! Set selected values to given vector
  PROCEDURE, PASS(obj) :: Set6 => sField_Set6
    !! Set values to a scalar by using triplet
  PROCEDURE, PASS(obj) :: Set7 => sField_Set7
    !! Set values to a vector by using triplet
  PROCEDURE, PASS(obj) :: Set8 => sField_Set8
    !! This method is used for assignment operator
  PROCEDURE, PASS(obj) :: Set9 => sField_Set9
    !! Set selected values using FEVariable
  PROCEDURE, PASS(obj) :: Set10 => sField_Set10
    !! Set selected values using FEVariable
  PROCEDURE, PASS(obj) :: Set11 => sField_Set11
    !! Set selected values using FEVariable
  GENERIC, PUBLIC :: Set => Set1, Set2, Set3, Set4, &
    & Set5, Set6, Set7, Set8, Set9, Set10, Set11
  GENERIC, PUBLIC :: ASSIGNMENT(=) => Set8
    !! Set values to a vector

  ! Get:
  ! @GetMethods
  PROCEDURE, PASS(obj) :: Get1 => sField_Get1
    !! Get single entry
  PROCEDURE, PASS(obj) :: Get2 => sField_Get2
    !! Get all values in Real vector
  PROCEDURE, PASS(obj) :: Get3 => sField_Get3
    !! Get selected values
  PROCEDURE, PASS(obj) :: Get4 => sField_Get4
    !! Get values from triplet
  PROCEDURE, PASS(obj) :: Get5 => sField_Get5
  PROCEDURE, PASS(obj) :: Get6 => sField_Get6
  PROCEDURE, PASS(obj) :: Get7 => sField_Get7
    !! Get selected values in FEVariable
  GENERIC, PUBLIC :: Get => Get1, Get2, Get3, Get4, Get5, Get6, Get7
  !! Get the entries of scalar field
  PROCEDURE, PUBLIC, PASS(obj) :: GetFEVariable => sField_GetFeVariable
  !! Get Finite Element variable

  ! SET:
  ! @DirichletBCMethods
  PROCEDURE, PASS(obj) :: sField_ApplyDirichletBC1
  PROCEDURE, PASS(obj) :: sField_ApplyDirichletBC2
  GENERIC, PUBLIC :: ApplyDirichletBC => sField_ApplyDirichletBC1, &
    & sField_ApplyDirichletBC2

  ! IO:
  ! @IOMethods
  !! Apply Dirichlet Boundary Condition
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => sField_Import
END TYPE ScalarField_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE(ScalarField_), PARAMETER, PUBLIC ::  &
  & TypeScalarField = ScalarField_(domains=NULL())

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
  MODULE SUBROUTINE SetScalarFieldParam(param, name, engine,  &
    & fieldType, comm, local_n, global_n)
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

INTERFACE
  MODULE SUBROUTINE sField_CheckEssentialParam(obj, param)
    CLASS(ScalarField_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE sField_CheckEssentialParam
END INTERFACE

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

INTERFACE
  MODULE SUBROUTINE sField_Initiate_old(obj, param, dom)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE sField_Initiate_old
END INTERFACE

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
  MODULE SUBROUTINE sField_Initiate1(obj, param, dom)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE sField_Initiate1
END INTERFACE ScalarFieldInitiate1

!----------------------------------------------------------------------------
!                                                         Final@Constructor
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE sField_Final(obj)
    TYPE(ScalarField_), INTENT(INOUT) :: obj
  END SUBROUTINE sField_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Final@Constructor
!----------------------------------------------------------------------------

INTERFACE ScalarFieldDeallocate
  MODULE SUBROUTINE sField_Deallocate(obj)
    TYPE(ScalarField_), INTENT(INOUT) :: obj
  END SUBROUTINE sField_Deallocate
END INTERFACE ScalarFieldDeallocate

!----------------------------------------------------------------------------
!                                                         Vector@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary:         This function returns an instance of [[ScalarField_]]

INTERFACE
  MODULE FUNCTION sField_Constructor1(param, dom) RESULT(Ans)
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
    TYPE(ScalarField_) :: ans
  END FUNCTION sField_Constructor1
END INTERFACE

INTERFACE ScalarField
  MODULE PROCEDURE sField_Constructor1
END INTERFACE ScalarField

!----------------------------------------------------------------------------
!                                                 ScalarField_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary:         This function returns an instance of [[ScalarField_]]

INTERFACE
  MODULE FUNCTION sField_Constructor_1(param, dom) RESULT(Ans)
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
    CLASS(ScalarField_), POINTER :: ans
  END FUNCTION sField_Constructor_1
END INTERFACE

INTERFACE ScalarField_Pointer
  MODULE PROCEDURE sField_Constructor_1
END INTERFACE ScalarField_Pointer

!----------------------------------------------------------------------------
!                                                                Import@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Imports the content

INTERFACE ScalarFieldImport
  MODULE SUBROUTINE sField_Import(obj, hdf5, group, dom, domains)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    TYPE(Domain_), TARGET, OPTIONAL, INTENT(IN) :: dom
    TYPE(DomainPointer_), TARGET, OPTIONAL, INTENT(IN) :: domains(:)
  END SUBROUTINE sField_Import
END INTERFACE ScalarFieldImport

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE sField_Set1(obj, globalNode, VALUE, scale, &
    & addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE sField_Set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets all the entries of a scalar field

INTERFACE
  MODULE SUBROUTINE sField_Set2(obj, VALUE, scale, addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE sField_Set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Set all the entries by using a fortran vector

INTERFACE
  MODULE SUBROUTINE sField_Set3(obj, VALUE, scale, addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE sField_Set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the selected entries

INTERFACE
 MODULE SUBROUTINE sField_Set4(obj, globalNode, VALUE, scale, addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE sField_Set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the selected entries

INTERFACE
 MODULE SUBROUTINE sField_Set5(obj, globalNode, VALUE, scale, addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE sField_Set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the selected entries using triplet

INTERFACE
  MODULE SUBROUTINE sField_Set6(obj, istart, iend, stride, VALUE, &
    & scale, addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE sField_Set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Set the vector vals using triplet

INTERFACE
  MODULE SUBROUTINE sField_Set7(obj, istart, iend, stride, VALUE, &
    & scale, addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE sField_Set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: used for assignment operator

INTERFACE
  MODULE SUBROUTINE sField_Set8(obj, obj2)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    CLASS(ScalarField_), INTENT(IN) :: obj2
  END SUBROUTINE sField_Set8
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the selected entries using [[FEVariable_]]

INTERFACE
 MODULE SUBROUTINE sField_Set9(obj, globalNode, VALUE, scale, addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    TYPE(FEVariable_), INTENT(IN) :: VALUE
  !! Scalar, Nodal, FEVariable (Space or Constant)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE sField_Set9
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: obj=obj+scalar*obj2

INTERFACE
  MODULE SUBROUTINE sField_Set10(obj, obj2, scale, addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    CLASS(ScalarField_), INTENT(IN) :: obj2
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
  END SUBROUTINE sField_Set10
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE sField_Set11(obj, ivar, idof, VALUE, ivar_value, &
    & idof_value, scale, addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    CLASS(AbstractNodeField_), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar_value
    INTEGER(I4B), INTENT(IN) :: idof_value
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE sField_Set11
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE sField_Get1(obj, VALUE, globalNode)
    CLASS(ScalarField_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: globalNode
  END SUBROUTINE sField_Get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns all the entries by using given scalar field

INTERFACE
  MODULE SUBROUTINE sField_Get2(obj, VALUE)
    CLASS(ScalarField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
  END SUBROUTINE sField_Get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE sField_Get3(obj, VALUE, globalNode)
    CLASS(ScalarField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
  END SUBROUTINE sField_Get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: returns the value using triplet

INTERFACE
  MODULE SUBROUTINE sField_Get4(obj, VALUE, istart, iend, stride)
    CLASS(ScalarField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
  END SUBROUTINE sField_Get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: returns the selected values in FEVariable

INTERFACE
  MODULE SUBROUTINE sField_Get5(obj, VALUE, globalNode)
    CLASS(ScalarField_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
  !! Scalar Nodal FEVariable
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
  END SUBROUTINE sField_Get5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: returns the selected values in FEVariable

INTERFACE
  MODULE SUBROUTINE sField_Get6(obj, VALUE)
    CLASS(ScalarField_), INTENT(IN) :: obj
    CLASS(ScalarField_), INTENT(INOUT) :: VALUE
  END SUBROUTINE sField_Get6
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

INTERFACE
 MODULE SUBROUTINE sField_Get7(obj, ivar, idof, VALUE, ivar_value, idof_value)
    CLASS(ScalarField_), INTENT(IN) :: obj
    CLASS(AbstractNodeField_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B), INTENT(IN) :: ivar_value
    INTEGER(I4B), INTENT(IN) :: idof_value
  END SUBROUTINE sField_Get7
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetFEVariable@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: Set single entry

INTERFACE ScalarFieldGetFEVariable
  MODULE SUBROUTINE sField_GetFeVariable(obj, globalNode, VALUE, ivar)
    CLASS(ScalarField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
  END SUBROUTINE sField_GetFeVariable
END INTERFACE ScalarFieldGetFEVariable

!----------------------------------------------------------------------------
!                                               ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 22 Jan 2021
! summary: Apply Dirichlet boundary condition

INTERFACE
  MODULE SUBROUTINE sField_ApplyDirichletBC1(obj, dbc)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    CLASS(DirichletBC_), INTENT(IN) :: dbc
  END SUBROUTINE sField_ApplyDirichletBC1
END INTERFACE

!----------------------------------------------------------------------------
!                                               ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 22 Jan 2021
! summary: Apply Dirichlet boundary condition

INTERFACE
  MODULE SUBROUTINE sField_ApplyDirichletBC2(obj, dbc)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    CLASS(DirichletBCPointer_), INTENT(IN) :: dbc(:)
  END SUBROUTINE sField_ApplyDirichletBC2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ScalarField_Class
