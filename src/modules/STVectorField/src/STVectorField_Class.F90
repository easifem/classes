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
! summary: STVector field data type is defined

MODULE STVectorField_Class
USE GlobalData
USE BaseType
USE AbstractField_Class
USE AbstractNodeField_Class
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE HDF5File_Class
USE Domain_Class
USE DirichletBC_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "STVectorField_Class"
CHARACTER(*), PARAMETER :: myprefix = "STVectorField"

!----------------------------------------------------------------------------
!                                                             STVectorField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: STVector field
!
!{!pages/STVectorField_.md}

TYPE, EXTENDS(AbstractNodeField_) :: STVectorField_
  INTEGER(I4B) :: spaceCompo = 0_I4B
  INTEGER(I4B) :: timeCompo = 0_I4B
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: checkEssentialParam => &
    & stvField_checkEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => stvField_Initiate1
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate2 => stvField_Initiate2
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => stvField_Deallocate
  FINAL :: stvField_Final
  PROCEDURE, PUBLIC, PASS(obj) :: Display => stvField_Display
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => stvField_Import
  PROCEDURE, PUBLIC, PASS(obj) :: Export => stvField_Export
  PROCEDURE, PASS(obj) :: set1 => stvField_set1
    !! set single entry
  PROCEDURE, PASS(obj) :: set2 => stvField_set2
    !! set all values to a STVector values
  PROCEDURE, PASS(obj) :: set3 => stvField_set3
    !! set all values to a given STvector
  PROCEDURE, PASS(obj) :: set4 => stvField_set4
    !! set selected values to given STVector
  PROCEDURE, PASS(obj) :: set5 => stvField_set5
    !! set selected values to given STvector
  PROCEDURE, PASS(obj) :: set6 => stvField_set6
    !! set values to a STVector by using triplet
  PROCEDURE, PASS(obj) :: set7 => stvField_set7
    !! set values to a STvector by using triplet
  PROCEDURE, PASS(obj) :: set8 => stvField_set8
    !! set values to a STvector by using triplet
  PROCEDURE, PASS(obj) :: set9 => stvField_set9
    !! set values to a STvector by using triplet
  PROCEDURE, PASS(obj) :: set10 => stvField_set10
    !! set values to a STvector by using triplet
  PROCEDURE, PASS(obj) :: set11 => stvField_set11
    !! set values to a STvector by using triplet
  PROCEDURE, PASS(obj) :: set12 => stvField_set12
    !! set values to a STvector by using triplet
  PROCEDURE, PASS(obj) :: set13 => stvField_set13
  PROCEDURE, PASS(obj) :: set14 => stvField_set14
    !! set values to a STvector by using triplet
  PROCEDURE, PASS(obj) :: set15 => stvField_set15
  GENERIC, PUBLIC :: set => &
    & set1, set2, set3, set4, set5, set6, &
    & set7, set8, set9, set10, set11, &
    & set12, set13, set14, set15
  PROCEDURE, PASS(obj) :: get1 => stvField_get1
  PROCEDURE, PASS(obj) :: get2 => stvField_get2
  PROCEDURE, PASS(obj) :: get3 => stvField_get3
  PROCEDURE, PASS(obj) :: get4 => stvField_get4
  PROCEDURE, PASS(obj) :: get5 => stvField_get5
  PROCEDURE, PASS(obj) :: get6 => stvField_get6
  PROCEDURE, PASS(obj) :: get7 => stvField_get7
  PROCEDURE, PASS(obj) :: get8 => stvField_get8
  PROCEDURE, PASS(obj) :: get9 => stvField_get9
  PROCEDURE, PASS(obj) :: get10 => stvField_get10
  PROCEDURE, PASS(obj) :: get11 => stvField_get11
  GENERIC, PUBLIC :: get => get1, get2, get3, get4, get5, &
    & get6, get7, get8, get9, get10, get11
  PROCEDURE, PUBLIC, PASS(obj) :: getPointerOfComponent =>  &
    & stvField_getPointerOfComponent
    !! get the entries of STVector field
  PROCEDURE, PASS(obj) :: stvField_applyDirichletBC1
  PROCEDURE, PASS(obj) :: stvField_applyDirichletBC2
  GENERIC, PUBLIC :: applyDirichletBC => &
    & stvField_applyDirichletBC1, &
    & stvField_applyDirichletBC2
END TYPE STVectorField_

PUBLIC :: STVectorField_
TYPE(STVectorField_), PARAMETER, PUBLIC :: TypeSTVectorField =  &
  & STVectorField_(domains=NULL())

!---------------------------------------------------------------------------
!                                                     STVectorFieldPointer_
!----------------------------------------------------------------------------

TYPE :: STVectorFieldPointer_
  CLASS(STVectorField_), POINTER :: ptr => NULL()
END TYPE STVectorFieldPointer_

PUBLIC :: STVectorFieldPointer_

!----------------------------------------------------------------------------
!                                   setSTVectorFieldParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Aug 2021
! summary: set essential parameter

INTERFACE
  MODULE SUBROUTINE setSTVectorFieldParam(param, name, engine, &
    & spaceCompo, timeCompo, fieldType, comm, local_n, global_n)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: name
    CHARACTER(*), INTENT(IN) :: engine
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fieldType
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: comm
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: global_n
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_n
  END SUBROUTINE setSTVectorFieldParam
END INTERFACE

PUBLIC :: setSTVectorFieldParam

!----------------------------------------------------------------------------
!                                     checkEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine check the essential parameters in param.

INTERFACE
  MODULE SUBROUTINE stvField_checkEssentialParam(obj, param)
    CLASS(STVectorField_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE stvField_checkEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This subroutine initiates the space-time vector field
!
!# Introduction
!
! This routine initiate the space-time vector field object.
! `param` contains the information of parameters required to initiate the
! this field.
! There are essential and optional information.
! Essential information are described below.
! - `name`  character defining the name of STvector field
! - `spaceCompo` is the total degree of freedom or components
! - `timeCompo` is the total degree of freedom or components
! - `fieldType` type of field type; FIELD_TYPE_CONSTANT, FIELD_TYPE_NORMAL

INTERFACE
  MODULE SUBROUTINE stvField_Initiate1(obj, param, dom)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE stvField_Initiate1
END INTERFACE

INTERFACE STVectorFieldInitiate1
  MODULE PROCEDURE stvField_Initiate1
END INTERFACE STVectorFieldInitiate1

PUBLIC :: STVectorFieldInitiate1

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-29
! summary: Initiate2

INTERFACE
  MODULE SUBROUTINE stvField_Initiate2(obj, obj2, copyFull, copyStructure, &
    & usePointer)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    CLASS(AbstractField_), INTENT(INOUT) :: obj2
    !! It should be a child of AbstractNodeField_
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyFull
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyStructure
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: usePointer
  END SUBROUTINE stvField_Initiate2
END INTERFACE

INTERFACE STVectorFieldInitiate2
  MODULE PROCEDURE stvField_Initiate2
END INTERFACE STVectorFieldInitiate2

PUBLIC :: STVectorFieldInitiate2

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine deallocates the data stored inside the STVectorField_ obj

INTERFACE
  MODULE SUBROUTINE stvField_Deallocate(obj)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
  END SUBROUTINE stvField_Deallocate
END INTERFACE

INTERFACE STVectorFieldDeallocate
  MODULE PROCEDURE stvField_Deallocate
END INTERFACE STVectorFieldDeallocate

PUBLIC :: STVectorFieldDeallocate

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE stvField_Final(obj)
    TYPE(STVectorField_), INTENT(INOUT) :: obj
  END SUBROUTINE stvField_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                STVector@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This function returns an instance of [[STVectorField_]]

INTERFACE
  MODULE FUNCTION stvField_Constructor1(param, dom) RESULT(Ans)
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
    TYPE(STVectorField_) :: ans
  END FUNCTION stvField_Constructor1
END INTERFACE

INTERFACE STVectorField
  MODULE PROCEDURE stvField_Constructor1
END INTERFACE STVectorField

PUBLIC :: STVectorField

!----------------------------------------------------------------------------
!                                   STVectorField_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary:         This function returns an instance of [[STVectorField_]]

INTERFACE
  MODULE FUNCTION stvField_Constructor_1(param, dom) RESULT(Ans)
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
    CLASS(STVectorField_), POINTER :: ans
  END FUNCTION stvField_Constructor_1
END INTERFACE

INTERFACE STVectorField_Pointer
  MODULE PROCEDURE stvField_Constructor_1
END INTERFACE STVectorField_Pointer

PUBLIC :: STVectorField_Pointer

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Display the content of [[STVectorField_]]

INTERFACE
  MODULE SUBROUTINE stvField_Display(obj, msg, unitNo)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE stvField_Display
END INTERFACE

INTERFACE STVectorFieldDisplay
  MODULE PROCEDURE stvField_Display
END INTERFACE STVectorFieldDisplay

PUBLIC :: STVectorFieldDisplay

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Imports the content

INTERFACE
  MODULE SUBROUTINE stvField_Import(obj, hdf5, group, dom, domains)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    TYPE(Domain_), TARGET, OPTIONAL, INTENT(IN) :: dom
    TYPE(DomainPointer_), TARGET, OPTIONAL, INTENT(IN) :: domains(:)
  END SUBROUTINE stvField_Import
END INTERFACE

INTERFACE STVectorFieldImport
  MODULE PROCEDURE stvField_Import
END INTERFACE STVectorFieldImport

PUBLIC :: STVectorFieldImport

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Exports the content

INTERFACE
  MODULE SUBROUTINE stvField_Export(obj, hdf5, group)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE stvField_Export
END INTERFACE

INTERFACE STVectorFieldExport
  MODULE PROCEDURE stvField_Export
END INTERFACE STVectorFieldExport

PUBLIC :: STVectorFieldExport

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the single entry of the STVector field
!
!# Introduction
! This routine sets the single entry of the STvector field. Here,
! `value` is a two dimensional array of real numbers, denoting the space-time
! components of the vector. The first index denotes the space components,
! second index denotes the time-components. As a result, total number of rows
! and columns in `value` are equal to the total number of spaceCompo and
! timeCompo.
!
! STvector( :, :, globalNode ) = value( :, : )

INTERFACE
  MODULE SUBROUTINE stvField_set1(obj, globalNode, VALUE, scale, &
    & addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets all the entries of a STVector field
!
!# Introduction
! This routine sets all entries of the STvector field. Here,
! `value` is a two dimensional array of real numbers, denoting the space-time
! components of the vector. The first index denotes the space components,
! second index denotes the time-components. As a result, total number of rows
! and columns in `value` are equal to the total number of spaceCompo and
! timeCompo.
!
! STvector( :, :, i ) = value( :, : ), for i = 1, tNodes

INTERFACE
  MODULE SUBROUTINE stvField_set2(obj, VALUE, scale, addContribution)
    CLASS(STVectorField_), TARGET, INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets all the entries of a STVector field
!
!# Introduction
! This routine sets all entries of the space-time vector field. Here
! `spaceCompo` and `timeCompo` are the spatial temporal components, which we
! want to replace by a scalar value `value`.
!
! STvector( spaceCompo, timeCompo, i ) = value, for i = 1, tNodes

INTERFACE
  MODULE SUBROUTINE stvField_set3(obj, VALUE, spaceCompo, timeCompo, &
    & scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine set all the entries by using given STVector field
!
!# Introduction
! This routine set all entries of the space-time vector.
! The first index of `value` denotes the spatial components
! The second index of `value` denotes the temporal components
! The thrid index of `value` denotes the node number
!
! STvector( :, :, : ) = value( :, :, : )

INTERFACE
  MODULE SUBROUTINE stvField_set4(obj, VALUE, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:, :, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine set all nodal values of a given space-time component
!
!# Introduction
!
! This routine sets all entries of the space-time vector field. Here
! `spaceCompo` and `timeCompo` are the spatial temporal components, which we
! want to replace by a vector `value`. Note that the size of `value` should
! be equal to the total number of nodes in the mesh.
!
! STvector( spaceCompo, timeCompo, : ) = value( : )

INTERFACE
  MODULE SUBROUTINE stvField_set5(obj, VALUE, spaceCompo, timeCompo,  &
    & scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine set all nodal values of a given space-time component
!
!# Introduction
!
! This routine sets all entries of the space-time vector field. Here
! `spaceCompo` and `timeCompo` are the spatial temporal components, which we
! want to replace by a vector of scalars. These vectors of scalar are stored
! inside a scalar field called `value`. Note that the size of `value` should
! be equal to the total number of nodes in the mesh.
!
! STvector( spaceCompo, : ) = value

INTERFACE
  MODULE SUBROUTINE stvField_set6(obj, VALUE, spaceCompo, timeCompo,  &
    & scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    CLASS(AbstractNodeField_), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries
!
!# Introduction
! This soubroutine sets the selected enties in space-time vector to a
! constant space-time nodal values. Here globalNode is the list of global
! node number. `value` is a rank2 array of real numbers. Its first index
! denotes the space component and second component denotes the time component.
!
!Effectively it does the following:
!
! STvector( :, :, globalNode ) = value( :, : ), for entries in global nodes

INTERFACE
  MODULE SUBROUTINE stvField_set7(obj, VALUE, globalNode, scale,  &
    & addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries
!
!# Introduction
! This routine sets selected entries of space-time vector field. Here
! globalNode contains the list of global nodes where values will be changed.
!
! - `value` is a rank-3 array.
! - Its first index denotes the space component,
! - second index denotes the time components
! - third component denotes the node number.
!
!@note
!The size of dimension should be equal to the size of globalNode.
!@endnote
!
! STvector( :, :, globalNode ) = value( :, :, : )

INTERFACE
  MODULE SUBROUTINE stvField_set8(obj, globalNode, VALUE, scale, &
    & addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:, :, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_set8
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries
!
!# Introduction
! This routine sets the selected components of selected nodes to a given value
!
! STvector( spaceCompo, globalNode ) = value( : )

INTERFACE
  MODULE SUBROUTINE stvField_set9(obj, VALUE, globalNode, spaceCompo,  &
    & timeCompo, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_set9
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries
!
!# Introduction
! selected components, selected nodes
!
! STvector( spaceCompo, globalNode ) = value

INTERFACE
  MODULE SUBROUTINE stvField_set10(obj, VALUE, globalNode, spaceCompo, &
    & timeCompo, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_set10
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries

INTERFACE
  MODULE SUBROUTINE stvField_set11(obj, VALUE, istart, iend, stride,  &
    & scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_set11
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: set the STvector values using triplet

INTERFACE
  MODULE SUBROUTINE stvField_set12(obj, VALUE, istart, iend, stride,  &
    & scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:, :, :)
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_set12
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: set the STvector values using triplet

INTERFACE
  MODULE SUBROUTINE stvField_set13(obj, VALUE, globalNode, scale, &
    & addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    TYPE(FEVariable_), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_set13
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: set the STvector values using triplet

INTERFACE
  MODULE SUBROUTINE stvField_set14(obj, VALUE, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_set14
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-29
! summary: Setvalues

INTERFACE
  MODULE SUBROUTINE stvField_set15(obj, ivar, idof, VALUE, ivar_value, &
    & idof_value, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    CLASS(AbstractNodeField_), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar_value
    INTEGER(I4B), INTENT(IN) :: idof_value
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_set15
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns values from space-time vector field
!
!# Introduction
!
! This subroutine returns the values from the space-time vector field
! The values are returned in a vector of real numbers.
!
! Here GlobalNode denotes the node number, spaceCompo is the spatial
! component, and timeCompo is the time component.
! - Either globalNode should be present or

INTERFACE
  MODULE SUBROUTINE stvField_get1(obj, VALUE, globalNode, spaceCompo, &
    & timeCompo)
    CLASS(STVectorField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalNode
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo
  END SUBROUTINE stvField_get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine get all the entries of the space-time vector field
!
!# Introduction
! This routine returns all the nodal values of a space-time nodal vector field.
! Here value is a rank3 array of reals.
!
! - Its first index denotes the spatial component
! - the second index denotes the temporal component
! - the third index denotes the node number.

INTERFACE
  MODULE SUBROUTINE stvField_get2(obj, VALUE)
    CLASS(STVectorField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :, :)
  END SUBROUTINE stvField_get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the space-time nodal values of selected nodes
!
!# Introduction
!         This routine returns the space-time nodal values of selected nodes.
! The values will be returned in rank3 array value.
! - The first index corresponds to the spatial components
! - The second index corresponds to the temporal components,
! - The third index corresponds to the node number.
!
!@note
! The size of third dimension of value should be equal to the size of globalNode.
!@endnote

INTERFACE
  MODULE SUBROUTINE stvField_get3(obj, VALUE, globalNode)
    CLASS(STVectorField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :, :)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
  END SUBROUTINE stvField_get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the nodal value of a space-time vector field
!
!# Introduction
! This routine returns the nodal values of a space-time nodal vector.
! In this routine we can specify the spatial and temporal component using
! spaceCompo and timeCompo. globalNode contains the list of global node
! number. Also, the values are returned in the a vector scalar `values`. Note
! that the length of value should be equal to the size of globalNode vector.

INTERFACE
  MODULE SUBROUTINE stvField_get4(obj, VALUE, globalNode, spaceCompo, &
    & timeCompo)
    CLASS(STVectorField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
  END SUBROUTINE stvField_get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE stvField_get5(obj, VALUE, globalNode, spaceCompo, &
    & timeCompo)
    CLASS(STVectorField_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
  END SUBROUTINE stvField_get5
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE stvField_get6(obj, VALUE, istart, iend, stride)
    CLASS(STVectorField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :, :)
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
  END SUBROUTINE stvField_get6
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE stvField_get7(obj, VALUE, istart, iend, stride, &
    & spaceCompo, timeCompo)
    CLASS(STVectorField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
  END SUBROUTINE stvField_get7
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the space-time value at given node number

INTERFACE
  MODULE SUBROUTINE stvField_get8(obj, VALUE, globalNode)
    CLASS(STVectorField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
    INTEGER(I4B), INTENT(IN) :: globalNode
  END SUBROUTINE stvField_get8
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the space-time value at given node number

INTERFACE
  MODULE SUBROUTINE stvField_get9(obj, VALUE, globalNode)
    CLASS(STVectorField_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
  END SUBROUTINE stvField_get9
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine return value in FEVariable

INTERFACE
  MODULE SUBROUTINE stvField_get10(obj, VALUE, spaceCompo, timeCompo)
    CLASS(STVectorField_), INTENT(IN) :: obj
    CLASS(AbstractField_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo
  END SUBROUTINE stvField_get10
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: REturns the value

INTERFACE
MODULE SUBROUTINE stvField_get11(obj, ivar, idof, VALUE, ivar_value, idof_value)
    CLASS(STVectorField_), INTENT(IN) :: obj
    CLASS(AbstractNodeField_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B), INTENT(IN) :: ivar_value
    INTEGER(I4B), INTENT(IN) :: idof_value
  END SUBROUTINE stvField_get11
END INTERFACE

!----------------------------------------------------------------------------
!                                               applyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 22 Jan 2021
! summary: Apply Dirichlet boundary condition

INTERFACE
  MODULE SUBROUTINE stvField_applyDirichletBC1(obj, dbc)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    CLASS(DirichletBC_), INTENT(IN) :: dbc
  END SUBROUTINE stvField_applyDirichletBC1
END INTERFACE

!----------------------------------------------------------------------------
!                                               applyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 22 Jan 2021
! summary: Apply Dirichlet boundary condition

INTERFACE
  MODULE SUBROUTINE stvField_applyDirichletBC2(obj, dbc)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    CLASS(DirichletBCPointer_), INTENT(IN) :: dbc(:)
  END SUBROUTINE stvField_applyDirichletBC2
END INTERFACE

!----------------------------------------------------------------------------
!                                         getPointerOfComponent@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns pointer to a specific component

INTERFACE
  MODULE FUNCTION stvField_getPointerOfComponent(obj, spaceCompo, timeCompo) &
    & RESULT(ans)
    CLASS(STVectorField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), POINTER :: ans(:)
  END FUNCTION stvField_getPointerOfComponent
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE STVectorField_Class
