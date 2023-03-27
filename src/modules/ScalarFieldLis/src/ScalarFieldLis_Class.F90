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
USE BaseType
USE AbstractField_Class
USE AbstractNodeField_Class
USE ScalarField_Class
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE HDF5File_Class
USE Domain_Class
USE DirichletBC_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "ScalarFieldLis_Class"
CHARACTER(*), PARAMETER :: myPrefix = "ScalarField"

!----------------------------------------------------------------------------
!                                                              ScalarField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: LIS Scalar field
!
!{!pages/ScalarFieldLis_.md}

TYPE, EXTENDS(ScalarField_) :: ScalarFieldLis_
#ifdef USE_LIS
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => sField_Deallocate
  FINAL :: sField_Final
  PROCEDURE, PUBLIC, PASS(obj) :: Display => sField_Display
  PROCEDURE, PUBLIC, PASS(obj) :: Export => sField_Export
  PROCEDURE, PUBLIC, PASS(obj) :: Norm2 => sField_Norm2
  PROCEDURE, PUBLIC, PASS(obj) :: Norm1 => sField_Norm1
  PROCEDURE, PUBLIC, PASS(obj) :: Normi => sField_Normi
  PROCEDURE, PUBLIC, PASS(obj) :: Size => sField_Size
  PROCEDURE, PUBLIC, PASS(obj) :: GetPointer => sField_GetPointer
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => sField_Initiate1
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => sField_Import
  !
  ! @SetMethods
  !
  PROCEDURE, PASS(obj) :: SetSingle => sField_SetSingle
  PROCEDURE, PASS(obj) :: SetAll => sField_SetAll
  PROCEDURE, PASS(obj) :: SetMultiple => sField_SetMultiple
  PROCEDURE, PASS(obj) :: set1 => sField_set1
    !! set single entry
  PROCEDURE, PASS(obj) :: set2 => sField_set2
    !! set all values to a scalar values
  PROCEDURE, PASS(obj) :: set3 => sField_set3
    !! set all values to a given vector
  PROCEDURE, PASS(obj) :: set4 => sField_set4
    !! set selected values to given scalar
  PROCEDURE, PASS(obj) :: set5 => sField_set5
    !! set selected values to given vector
  PROCEDURE, PASS(obj) :: set6 => sField_set6
    !! set values to a scalar by using triplet
  PROCEDURE, PASS(obj) :: set7 => sField_set7
    !! set values to a vector by using triplet
  PROCEDURE, PASS(obj) :: set8 => sField_set8
    !! This method is used for assignment operator
  PROCEDURE, PASS(obj) :: set9 => sField_set9
    !! Set selected values using FEVariable
  PROCEDURE, PASS(obj) :: set10 => sField_set10
    !! Set selected values using FEVariable
  PROCEDURE, PASS(obj) :: get1 => sField_get1
    !! get single entry
  PROCEDURE, PASS(obj) :: get2 => sField_get2
    !! get all values in Real vector
  PROCEDURE, PASS(obj) :: get3 => sField_get3
    !! get selected values
  PROCEDURE, PASS(obj) :: get4 => sField_get4
    !! get values from triplet
  PROCEDURE, PASS(obj) :: get5 => sField_get5
  PROCEDURE, PASS(obj) :: get6 => sField_get6
    !! get selected values in FEVariable
  PROCEDURE, PASS(obj) :: sField_applyDirichletBC1
  PROCEDURE, PASS(obj) :: sField_applyDirichletBC2
  !!
#endif
END TYPE ScalarFieldLis_

PUBLIC :: ScalarFieldLis_
TYPE(ScalarFieldLis_), PARAMETER, PUBLIC :: TypeScalarFieldLis =&
& ScalarFieldLis_(domains=NULL())

!----------------------------------------------------------------------------
!                                                       ScalarFieldPointer_
!----------------------------------------------------------------------------

TYPE :: ScalarFieldLisPointer_
  CLASS(ScalarFieldLis_), POINTER :: ptr => NULL()
END TYPE ScalarFieldLisPointer_

PUBLIC :: ScalarFieldLisPointer_

!----------------------------------------------------------------------------
!                                                         Final@Constructor
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE sField_Final(obj)
    TYPE(ScalarFieldLis_), INTENT(INOUT) :: obj
  END SUBROUTINE sField_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Vector@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary:         This function returns an instance of [[ScalarFieldLis_]]

INTERFACE
  MODULE FUNCTION sField_Constructor1(param, dom) RESULT(Ans)
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
    TYPE(ScalarFieldLis_) :: ans
  END FUNCTION sField_Constructor1
END INTERFACE

INTERFACE ScalarFieldLis
  MODULE PROCEDURE sField_Constructor1
END INTERFACE ScalarFieldLis

PUBLIC :: ScalarFieldLis

!----------------------------------------------------------------------------
!                                         ScalarFieldLis_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary:         This function returns an instance of [[ScalarFieldLis_]]

INTERFACE
  MODULE FUNCTION sField_Constructor_1(param, dom) RESULT(Ans)
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
    CLASS(ScalarFieldLis_), POINTER :: ans
  END FUNCTION sField_Constructor_1
END INTERFACE

INTERFACE ScalarFieldLis_Pointer
  MODULE PROCEDURE sField_Constructor_1
END INTERFACE ScalarFieldLis_Pointer

PUBLIC :: ScalarFieldLis_Pointer

#ifdef USE_LIS

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE sField_Deallocate(obj)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
  END SUBROUTINE sField_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE sField_Display(obj, msg, unitno)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE sField_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE sField_Export(obj, hdf5, group)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE sField_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Norm2@BlasMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION sField_Norm2(obj) RESULT(ans)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION sField_Norm2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Norm2@BlasMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION sField_Norm1(obj) RESULT(ans)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION sField_Norm1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Norm2@BlasMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION sField_Normi(obj) RESULT(ans)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION sField_Normi
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Size@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION sField_Size(obj, dims) RESULT(ans)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL :: dims
    INTEGER(I4B) :: ans
  END FUNCTION sField_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetPointer@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION sField_getPointer(obj) RESULT(ans)
    CLASS(ScalarFieldLis_), TARGET, INTENT(IN) :: obj
    REAL(DFP), POINTER :: ans(:)
  END FUNCTION sField_getPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-22
! summary: This subroutine initiates the ScalarFieldLis_ object

INTERFACE
  MODULE SUBROUTINE sField_Initiate1(obj, param, dom)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE sField_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Import@IOMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE sField_Import(obj, hdf5, group, dom, domains)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    TYPE(Domain_), TARGET, OPTIONAL, INTENT(IN) :: dom
    TYPE(DomainPointer_), TARGET, OPTIONAL, INTENT(IN) :: domains(:)
  END SUBROUTINE sField_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetSingle@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE sField_setSingle(obj, indx, VALUE, scale, &
    & addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE sField_setSingle
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetAll@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE sField_setAll(obj, VALUE, scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE sField_setAll
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMultiple@SetMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE sField_setMultiple(obj, indx, VALUE, scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE sField_setMultiple
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE sField_set1(obj, globalNode, VALUE, scale, &
    & addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE sField_set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets all the entries of a scalar field

INTERFACE
  MODULE SUBROUTINE sField_set2(obj, VALUE, scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE sField_set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine set all the entries by using a fortran vector

INTERFACE
  MODULE SUBROUTINE sField_set3(obj, VALUE, scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE sField_set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the multiple entries

INTERFACE
 MODULE SUBROUTINE sField_set4(obj, globalNode, VALUE, scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE sField_set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the multiple entries

INTERFACE
 MODULE SUBROUTINE sField_set5(obj, globalNode, VALUE, scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE sField_set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries using triplet

INTERFACE
  MODULE SUBROUTINE sField_set6(obj, istart, iend, stride, VALUE, &
    & scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE sField_set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: set the vector vals using triplet

INTERFACE
  MODULE SUBROUTINE sField_set7(obj, istart, iend, stride, VALUE, &
    & scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE sField_set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: used for assignment operator

INTERFACE
  MODULE SUBROUTINE sField_set8(obj, obj2)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    CLASS(ScalarField_), INTENT(IN) :: obj2
  END SUBROUTINE sField_set8
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries using [[FEVariable_]]

INTERFACE
 MODULE SUBROUTINE sField_set9(obj, globalNode, VALUE, scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    TYPE(FEVariable_), INTENT(IN) :: VALUE
  !! Scalar, Nodal, FEVariable (Space or Constant)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE sField_set9
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: obj=obj+scalar*obj2

INTERFACE
  MODULE SUBROUTINE sField_set10(obj, obj2, scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    CLASS(ScalarField_), INTENT(IN) :: obj2
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
  END SUBROUTINE sField_set10
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE sField_get1(obj, VALUE, globalNode)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: globalNode
  END SUBROUTINE sField_get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns all the entries by using given scalar field

INTERFACE
  MODULE SUBROUTINE sField_get2(obj, VALUE)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
  END SUBROUTINE sField_get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE sField_get3(obj, VALUE, globalNode)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
  END SUBROUTINE sField_get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: returns the value using triplet

INTERFACE
  MODULE SUBROUTINE sField_get4(obj, VALUE, istart, iend, stride)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
  END SUBROUTINE sField_get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: returns the selected values in FEVariable

INTERFACE
  MODULE SUBROUTINE sField_get5(obj, VALUE, globalNode)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! Scalar Nodal FEVariable
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
  END SUBROUTINE sField_get5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: returns the selected values in FEVariable

INTERFACE
  MODULE SUBROUTINE sField_get6(obj, VALUE)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    CLASS(ScalarField_), INTENT(INOUT) :: VALUE
  END SUBROUTINE sField_get6
END INTERFACE

!----------------------------------------------------------------------------
!                                               applyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 22 Jan 2021
! summary: Apply Dirichlet boundary condition

INTERFACE
  MODULE SUBROUTINE sField_applyDirichletBC1(obj, dbc)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    CLASS(DirichletBC_), INTENT(IN) :: dbc
  END SUBROUTINE sField_applyDirichletBC1
END INTERFACE

!----------------------------------------------------------------------------
!                                               applyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 22 Jan 2021
! summary: Apply Dirichlet boundary condition

INTERFACE
  MODULE SUBROUTINE sField_applyDirichletBC2(obj, dbc)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    CLASS(DirichletBCPointer_), INTENT(IN) :: dbc(:)
  END SUBROUTINE sField_applyDirichletBC2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
#endif

END MODULE ScalarFieldLis_Class
