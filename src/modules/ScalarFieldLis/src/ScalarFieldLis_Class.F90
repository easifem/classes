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
! date: 2023-03-22
! summary: Scalar field data type with LIS engine is defined

MODULE ScalarFieldLis_Class
USE GlobalData
USE String_Class
USE BaSetype
USE AbstractField_Class
USE AbstractNodeField_Class
USE ScalarField_Class
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE HDF5File_Class
USE AbstractDomain_Class, ONLY: AbstractDomain_, AbstractDomainPointer_
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "ScalarFieldLis_Class"
CHARACTER(*), PARAMETER :: myPrefix = "ScalarField"
PUBLIC :: ScalarFieldLis_
PUBLIC :: ScalarFieldLisPointer_
PUBLIC :: ScalarFieldLis
PUBLIC :: ScalarFieldLis_Pointer

!----------------------------------------------------------------------------
!                                                              ScalarField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: LIS Scalar field
!
!{!pages/docs-api/ScalarFieldLis/ScalarFieldLis_.md!}

TYPE, EXTENDS(ScalarField_) :: ScalarFieldLis_
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  FINAL :: obj_Final
  PROCEDURE, PUBLIC, PASS(obj) :: Size => obj_Size
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => obj_Initiate1

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import

  ! SET:
  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: SetSingle => obj_SetSingle
  PROCEDURE, PASS(obj) :: SetAll => obj_SetAll
  PROCEDURE, PASS(obj) :: SetMultiple => obj_SetMultiple
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

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetPointer => obj_GetPointer
  PROCEDURE, PUBLIC, PASS(obj) :: GetSingle => obj_GetSingle
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
    !! Get selected values in FEVariable

END TYPE ScalarFieldLis_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE(ScalarFieldLis_), PARAMETER, PUBLIC :: TypeScalarFieldLis = &
  & ScalarFieldLis_(domains=NULL())

!----------------------------------------------------------------------------
!                                                       ScalarFieldPointer_
!----------------------------------------------------------------------------

TYPE :: ScalarFieldLisPointer_
  CLASS(ScalarFieldLis_), POINTER :: ptr => NULL()
END TYPE ScalarFieldLisPointer_

!----------------------------------------------------------------------------
!                                                         Final@Constructor
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(ScalarFieldLis_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Vector@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary:         This function returns an instance of [[ScalarFieldLis_]]

INTERFACE ScalarFieldLis
  MODULE FUNCTION obj_Constructor1(param, dom) RESULT(Ans)
    TYPE(ParameterList_), INTENT(IN) :: param
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
    TYPE(ScalarFieldLis_) :: ans
  END FUNCTION obj_Constructor1
END INTERFACE ScalarFieldLis

!----------------------------------------------------------------------------
!                                         ScalarFieldLis_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary:         This function returns an instance of [[ScalarFieldLis_]]

INTERFACE ScalarFieldLis_Pointer
  MODULE FUNCTION obj_Constructor_1(param, dom) RESULT(Ans)
    TYPE(ParameterList_), INTENT(IN) :: param
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
    CLASS(ScalarFieldLis_), POINTER :: ans
  END FUNCTION obj_Constructor_1
END INTERFACE ScalarFieldLis_Pointer

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Size@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_Size(obj, dims) RESULT(ans)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL :: dims
    INTEGER(I4B) :: ans
  END FUNCTION obj_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-22
! summary: This subroutine initiates the ScalarFieldLis_ object

INTERFACE
  MODULE SUBROUTINE obj_Initiate1(obj, param, dom)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE obj_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Import@IOMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, dom, domains)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    CLASS(AbstractDomain_), TARGET, OPTIONAL, INTENT(IN) :: dom
    TYPE(AbstractDomainPointer_), TARGET, OPTIONAL, INTENT(IN) :: domains(:)
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetSingle@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetSingle(obj, indx, VALUE, scale, &
    & addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_SetSingle
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetAll@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetAll(obj, VALUE, scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_SetAll
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMultiple@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetMultiple(obj, indx, VALUE, scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_SetMultiple
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE obj_Set1(obj, globalNode, VALUE, scale, &
    & addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets all the entries of a scalar field

INTERFACE
  MODULE SUBROUTINE obj_Set2(obj, VALUE, scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
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
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
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
! summary: This routine Sets the multiple entries

INTERFACE
  MODULE SUBROUTINE obj_Set4(obj, globalNode, VALUE, scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the multiple entries

INTERFACE
  MODULE SUBROUTINE obj_Set5(obj, globalNode, VALUE, scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the selected entries using triplet

INTERFACE
  MODULE SUBROUTINE obj_Set6(obj, istart, iend, stride, VALUE, &
    & scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Set the vector vals using triplet

INTERFACE
  MODULE SUBROUTINE obj_Set7(obj, istart, iend, stride, VALUE, &
    & scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
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
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    CLASS(ScalarField_), INTENT(IN) :: VALUE
  END SUBROUTINE obj_Set8
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the selected entries using [[FEVariable_]]

INTERFACE
  MODULE SUBROUTINE obj_Set9(obj, globalNode, VALUE, scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    TYPE(FEVariable_), INTENT(IN) :: VALUE
  !! Scalar, Nodal, FEVariable (Space or Constant)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
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
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    CLASS(ScalarField_), INTENT(IN) :: obj2
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set10
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetPointer@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_GetPointer(obj) RESULT(ans)
    CLASS(ScalarFieldLis_), TARGET, INTENT(IN) :: obj
    REAL(DFP), POINTER :: ans(:)
  END FUNCTION obj_GetPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetSingle@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: Get single entry

INTERFACE
  MODULE SUBROUTINE obj_GetSingle(obj, indx, VALUE)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    REAL(DFP), INTENT(OUT) :: VALUE
  END SUBROUTINE obj_GetSingle
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE obj_Get1(obj, VALUE, globalNode)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: globalNode
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
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
  END SUBROUTINE obj_Get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE obj_Get3(obj, VALUE, globalNode)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
  END SUBROUTINE obj_Get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: returns the value using triplet

INTERFACE
  MODULE SUBROUTINE obj_Get4(obj, VALUE, istart, iend, stride)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
  END SUBROUTINE obj_Get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: returns the selected values in FEVariable

INTERFACE
  MODULE SUBROUTINE obj_Get5(obj, VALUE, globalNode)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! Scalar Nodal FEVariable
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
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
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    CLASS(ScalarField_), INTENT(INOUT) :: VALUE
  END SUBROUTINE obj_Get6
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ScalarFieldLis_Class
