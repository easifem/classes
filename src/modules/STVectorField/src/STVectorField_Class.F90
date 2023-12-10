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
USE BaSetype
USE AbstractField_Class
USE AbstractNodeField_Class
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE HDF5File_Class
USE Domain_Class
USE DirichletBC_Class
USE FiniteElement_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "STVectorField_Class"
CHARACTER(*), PARAMETER :: myprefix = "STVectorField"

PUBLIC :: STVectorField_
PUBLIC :: TypeSTVectorField
PUBLIC :: STVectorFieldPointer_
PUBLIC :: SetSTVectorFieldParam
PUBLIC :: STVectorFieldInitiate1
PUBLIC :: STVectorFieldInitiate2
PUBLIC :: STVectorFieldDeallocate
PUBLIC :: STVectorField
PUBLIC :: STVectorField_Pointer
PUBLIC :: STVectorFieldDisplay
PUBLIC :: STVectorFieldImport
PUBLIC :: STVectorFieldExport

!----------------------------------------------------------------------------
!                                                             STVectorField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: STVector field
!
!{!pages/docs-api/STVectorField/STVectorField_.md}

TYPE, EXTENDS(AbstractNodeField_) :: STVectorField_
  INTEGER(I4B), PUBLIC :: spaceCompo = 0_I4B
  INTEGER(I4B), PUBLIC :: timeCompo = 0_I4B
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: CheckEssentialParam => &
    & stvField_CheckEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => stvField_Initiate1
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate2 => stvField_Initiate2
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => stvField_Deallocate
  FINAL :: stvField_Final

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => stvField_Display
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => stvField_Import
  PROCEDURE, PUBLIC, PASS(obj) :: Export => stvField_Export

  ! SET:
  ! @SetMethods
  PROCEDURE, PASS(obj) :: Set1 => stvField_Set1
    !! Set single entry
  PROCEDURE, PASS(obj) :: Set2 => stvField_Set2
    !! Set all values to a STVector values
  PROCEDURE, PASS(obj) :: Set3 => stvField_Set3
    !! Set all values to a given STvector
  PROCEDURE, PASS(obj) :: Set4 => stvField_Set4
    !! Set selected values to given STVector
  PROCEDURE, PASS(obj) :: Set5 => stvField_Set5
    !! Set selected values to given STvector
  PROCEDURE, PASS(obj) :: Set6 => stvField_Set6
    !! Set values to a STVector by using triplet
  PROCEDURE, PASS(obj) :: Set7 => stvField_Set7
    !! Set values to a STvector by using triplet
  PROCEDURE, PASS(obj) :: Set8 => stvField_Set8
    !! Set values to a STvector by using triplet
  PROCEDURE, PASS(obj) :: Set9 => stvField_Set9
    !! Set values to a STvector by using triplet
  PROCEDURE, PASS(obj) :: Set10 => stvField_Set10
    !! Set values to a STvector by using triplet
  PROCEDURE, PASS(obj) :: Set11 => stvField_Set11
    !! Set values to a STvector by using triplet
  PROCEDURE, PASS(obj) :: Set12 => stvField_Set12
    !! Set values to a STvector by using triplet
  PROCEDURE, PASS(obj) :: Set13 => stvField_Set13
  PROCEDURE, PASS(obj) :: Set14 => stvField_Set14
    !! Set values to a STvector by using triplet
  PROCEDURE, PASS(obj) :: Set15 => stvField_Set15
    !! Set selected values to given STvector
  PROCEDURE, PASS(obj) :: Set16 => stvField_Set16
  PROCEDURE, PASS(obj) :: Set17 => stvField_Set17
  GENERIC, PUBLIC :: Set => &
    & Set1, Set2, Set3, Set4, Set5, Set6, &
    & Set7, Set8, Set9, Set10, Set11, &
    & Set12, Set13, Set14, Set15, Set16,  &
    & Set17

  ! GET:
  ! @GetMethods
  PROCEDURE, PASS(obj) :: Get1 => stvField_Get1
  PROCEDURE, PASS(obj) :: Get2 => stvField_Get2
  PROCEDURE, PASS(obj) :: Get3 => stvField_Get3
  PROCEDURE, PASS(obj) :: Get4 => stvField_Get4
  PROCEDURE, PASS(obj) :: Get5 => stvField_Get5
  PROCEDURE, PASS(obj) :: Get6 => stvField_Get6
  PROCEDURE, PASS(obj) :: Get7 => stvField_Get7
  PROCEDURE, PASS(obj) :: Get8 => stvField_Get8
  PROCEDURE, PASS(obj) :: Get9 => stvField_Get9
  PROCEDURE, PASS(obj) :: Get10 => stvField_Get10
  PROCEDURE, PASS(obj) :: Get11 => stvField_Get11
  GENERIC, PUBLIC :: Get => Get1, Get2, Get3, Get4, Get5, &
    & Get6, Get7, Get8, Get9, Get10, Get11
  PROCEDURE, PUBLIC, PASS(obj) :: GetPointerOfComponent =>  &
    & stvField_GetPointerOfComponent
    !! Get the entries of STVector field
  PROCEDURE, PUBLIC, PASS(obj) :: GetFEVariable =>  &
    & stvField_GetFeVariable
  !! Get multiple values in FEVariable
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => stvField_GetPrefix
  !! Get prefix

  ! SET:
  ! @DirichletBCMethods
  PROCEDURE, PASS(obj) :: stvField_ApplyDirichletBC1
  PROCEDURE, PASS(obj) :: stvField_ApplyDirichletBC2
  GENERIC, PUBLIC :: ApplyDirichletBC => stvField_ApplyDirichletBC1, &
    & stvField_ApplyDirichletBC2
END TYPE STVectorField_

!----------------------------------------------------------------------------
!                                                         TypeSTVectorField
!----------------------------------------------------------------------------

TYPE(STVectorField_), PARAMETER :: TypeSTVectorField =  &
  & STVectorField_(domains=NULL())

!---------------------------------------------------------------------------
!                                                     STVectorFieldPointer_
!----------------------------------------------------------------------------

TYPE :: STVectorFieldPointer_
  CLASS(STVectorField_), POINTER :: ptr => NULL()
END TYPE STVectorFieldPointer_

!----------------------------------------------------------------------------
!                                   SetSTVectorFieldParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Aug 2021
! summary: Set essential parameter

INTERFACE
  MODULE SUBROUTINE SetSTVectorFieldParam(param, name, engine, &
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
  END SUBROUTINE SetSTVectorFieldParam
END INTERFACE

!----------------------------------------------------------------------------
!                                     CheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Check the essential parameters in param.

INTERFACE
  MODULE SUBROUTINE stvField_CheckEssentialParam(obj, param)
    CLASS(STVectorField_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE stvField_CheckEssentialParam
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

INTERFACE STVectorFieldInitiate1
  MODULE SUBROUTINE stvField_Initiate1(obj, param, dom)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE stvField_Initiate1
END INTERFACE STVectorFieldInitiate1

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-29
! summary: Initiate2

INTERFACE STVectorFieldInitiate2
  MODULE SUBROUTINE stvField_Initiate2(obj, obj2, copyFull, copyStructure, &
    & usePointer)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    CLASS(AbstractField_), INTENT(INOUT) :: obj2
    !! It should be a child of AbstractNodeField_
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyFull
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyStructure
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: usePointer
  END SUBROUTINE stvField_Initiate2
END INTERFACE STVectorFieldInitiate2

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Deallocates the data stored inside the STVectorField_ obj

INTERFACE STVectorFieldDeallocate
  MODULE SUBROUTINE stvField_Deallocate(obj)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
  END SUBROUTINE stvField_Deallocate
END INTERFACE STVectorFieldDeallocate

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

INTERFACE STVectorField
  MODULE FUNCTION stvField_Constructor1(param, dom) RESULT(Ans)
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
    TYPE(STVectorField_) :: ans
  END FUNCTION stvField_Constructor1
END INTERFACE STVectorField

!----------------------------------------------------------------------------
!                                   STVectorField_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary:         This function returns an instance of [[STVectorField_]]

INTERFACE STVectorField_Pointer
  MODULE FUNCTION stvField_Constructor_1(param, dom) RESULT(Ans)
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
    CLASS(STVectorField_), POINTER :: ans
  END FUNCTION stvField_Constructor_1
END INTERFACE STVectorField_Pointer

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Display the content of [[STVectorField_]]

INTERFACE STVectorFieldDisplay
  MODULE SUBROUTINE stvField_Display(obj, msg, unitNo)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE stvField_Display
END INTERFACE STVectorFieldDisplay

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Imports the content

INTERFACE STVectorFieldImport
  MODULE SUBROUTINE stvField_Import(obj, hdf5, group, dom, domains)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    TYPE(Domain_), TARGET, OPTIONAL, INTENT(IN) :: dom
    TYPE(DomainPointer_), TARGET, OPTIONAL, INTENT(IN) :: domains(:)
  END SUBROUTINE stvField_Import
END INTERFACE STVectorFieldImport

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Exports the content

INTERFACE STVectorFieldExport
  MODULE SUBROUTINE stvField_Export(obj, hdf5, group)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE stvField_Export
END INTERFACE STVectorFieldExport

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the single entry of the STVector field
!
!# Introduction
! This routine Sets the single entry of the STvector field. Here,
! `value` is a two dimensional array of real numbers, denoting the space-time
! components of the vector. The first index denotes the space components,
! second index denotes the time-components. As a result, total number of rows
! and columns in `value` are equal to the total number of spaceCompo and
! timeCompo.
!
! STvector( :, :, globalNode ) = value( :, : )

INTERFACE
  MODULE SUBROUTINE stvField_Set1(obj, globalNode, VALUE, scale, &
    & addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_Set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets all the entries of a STVector field
!
!# Introduction
! This routine Sets all entries of the STvector field. Here,
! `value` is a two dimensional array of real numbers, denoting the space-time
! components of the vector. The first index denotes the space components,
! second index denotes the time-components. As a result, total number of rows
! and columns in `value` are equal to the total number of spaceCompo and
! timeCompo.
!
! STvector( :, :, i ) = value( :, : ), for i = 1, tNodes

INTERFACE
  MODULE SUBROUTINE stvField_Set2(obj, VALUE, scale, addContribution)
    CLASS(STVectorField_), TARGET, INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_Set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets all the entries of a STVector field
!
!# Introduction
! This routine Sets all entries of the space-time vector field. Here
! `spaceCompo` and `timeCompo` are the spatial temporal components, which we
! want to replace by a scalar value `value`.
!
! STvector( spaceCompo, timeCompo, i ) = value, for i = 1, tNodes

INTERFACE
  MODULE SUBROUTINE stvField_Set3(obj, VALUE, spaceCompo, timeCompo, &
    & scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_Set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Set all the entries by using given STVector field
!
!# Introduction
! This routine Set all entries of the space-time vector.
! The first index of `value` denotes the spatial components
! The second index of `value` denotes the temporal components
! The thrid index of `value` denotes the node number
!
! STvector( :, :, : ) = value( :, :, : )

INTERFACE
  MODULE SUBROUTINE stvField_Set4(obj, VALUE, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:, :, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_Set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Set all nodal values of a given space-time component
!
!# Introduction
!
! This routine Sets all entries of the space-time vector field. Here
! `spaceCompo` and `timeCompo` are the spatial temporal components, which we
! want to replace by a vector `value`. Note that the size of `value` should
! be equal to the total number of nodes in the mesh.
!
! STvector( spaceCompo, timeCompo, : ) = value( : )

INTERFACE
  MODULE SUBROUTINE stvField_Set5(obj, VALUE, spaceCompo, timeCompo,  &
    & scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_Set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Set all nodal values of a given space-time component
!
!# Introduction
!
! This routine Sets all entries of the space-time vector field. Here
! `spaceCompo` and `timeCompo` are the spatial temporal components, which we
! want to replace by a vector of scalars. These vectors of scalar are stored
! inside a scalar field called `value`. Note that the size of `value` should
! be equal to the total number of nodes in the mesh.
!
! STvector( spaceCompo, : ) = value

INTERFACE
  MODULE SUBROUTINE stvField_Set6(obj, VALUE, spaceCompo, timeCompo,  &
    & scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    CLASS(AbstractNodeField_), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_Set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the selected entries
!
!# Introduction
! This soubroutine Sets the selected enties in space-time vector to a
! constant space-time nodal values. Here globalNode is the list of global
! node number. `value` is a rank2 array of real numbers. Its first index
! denotes the space component and second component denotes time component.
!
!Effectively it does the following:
!
! STvector( :, :, globalNode ) = value( :, : ), for entries in global nodes

INTERFACE
  MODULE SUBROUTINE stvField_Set7(obj, VALUE, globalNode, scale,  &
    & addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_Set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the selected entries
!
!# Introduction
! This routine Sets selected entries of space-time vector field. Here
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
  MODULE SUBROUTINE stvField_Set8(obj, globalNode, VALUE, scale, &
    & addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:, :, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_Set8
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the selected entries
!
!# Introduction
! This routine Sets the selected components of selected nodes to given value
!
! STvector( spaceCompo, globalNode ) = value( : )

INTERFACE
  MODULE SUBROUTINE stvField_Set9(obj, VALUE, globalNode, spaceCompo,  &
    & timeCompo, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_Set9
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the selected entries
!
!# Introduction
! selected components, selected nodes
!
! STvector( spaceCompo, globalNode ) = value

INTERFACE
  MODULE SUBROUTINE stvField_Set10(obj, VALUE, globalNode, spaceCompo, &
    & timeCompo, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_Set10
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the selected entries

INTERFACE
  MODULE SUBROUTINE stvField_Set11(obj, VALUE, istart, iend, stride,  &
    & scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_Set11
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Set the STvector values using triplet

INTERFACE
  MODULE SUBROUTINE stvField_Set12(obj, VALUE, istart, iend, stride,  &
    & scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:, :, :)
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_Set12
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Set the STvector values using triplet

INTERFACE
  MODULE SUBROUTINE stvField_Set13(obj, VALUE, globalNode, scale, &
    & addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    TYPE(FEVariable_), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_Set13
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Set the STvector values using triplet

INTERFACE
  MODULE SUBROUTINE stvField_Set14(obj, VALUE, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_Set14
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-29
! summary: Setvalues

INTERFACE
  MODULE SUBROUTINE stvField_Set15(obj, ivar, idof, VALUE, ivar_value, &
    & idof_value, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    CLASS(AbstractNodeField_), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar_value
    INTEGER(I4B), INTENT(IN) :: idof_value
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_Set15
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2023-12-10
! summary: Setvalues

INTERFACE
  MODULE SUBROUTINE stvField_Set16(obj, VALUE, spaceCompo, timeCompo,  &
    & scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_Set16
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2023-12-10
! summary: Setvalues

INTERFACE
  MODULE SUBROUTINE stvField_Set17(obj, VALUE, spaceCompo, timeCompo,  &
    & scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stvField_Set17
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
  MODULE SUBROUTINE stvField_Get1(obj, VALUE, globalNode, spaceCompo, &
    & timeCompo)
    CLASS(STVectorField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalNode
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo
  END SUBROUTINE stvField_Get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Get all the entries of the space-time vector field
!
!# Introduction
! This routine returns all the nodal values of space-time nodal vector field.
! Here value is a rank3 array of reals.
!
! - Its first index denotes the spatial component
! - the second index denotes the temporal component
! - the third index denotes the node number.

INTERFACE
  MODULE SUBROUTINE stvField_Get2(obj, VALUE)
    CLASS(STVectorField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :, :)
  END SUBROUTINE stvField_Get2
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
! The size of third dimension of value should be equal to size of globalNode.
!@endnote

INTERFACE
  MODULE SUBROUTINE stvField_Get3(obj, VALUE, globalNode)
    CLASS(STVectorField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :, :)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
  END SUBROUTINE stvField_Get3
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
  MODULE SUBROUTINE stvField_Get4(obj, VALUE, globalNode, spaceCompo, &
    & timeCompo)
    CLASS(STVectorField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
  END SUBROUTINE stvField_Get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE stvField_Get5(obj, VALUE, globalNode, spaceCompo, &
    & timeCompo)
    CLASS(STVectorField_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
  END SUBROUTINE stvField_Get5
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE stvField_Get6(obj, VALUE, istart, iend, stride)
    CLASS(STVectorField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :, :)
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
  END SUBROUTINE stvField_Get6
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE stvField_Get7(obj, VALUE, istart, iend, stride, &
    & spaceCompo, timeCompo)
    CLASS(STVectorField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
  END SUBROUTINE stvField_Get7
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the space-time value at given node number

INTERFACE
  MODULE SUBROUTINE stvField_Get8(obj, VALUE, globalNode)
    CLASS(STVectorField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
    INTEGER(I4B), INTENT(IN) :: globalNode
  END SUBROUTINE stvField_Get8
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the space-time value at given node number

INTERFACE
  MODULE SUBROUTINE stvField_Get9(obj, VALUE, globalNode)
    CLASS(STVectorField_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
  END SUBROUTINE stvField_Get9
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine return value in FEVariable

INTERFACE
  MODULE SUBROUTINE stvField_Get10(obj, VALUE, spaceCompo, timeCompo)
    CLASS(STVectorField_), INTENT(IN) :: obj
    CLASS(AbstractField_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo
  END SUBROUTINE stvField_Get10
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: REturns the value

INTERFACE
  MODULE SUBROUTINE stvField_Get11(obj, ivar, idof, VALUE, ivar_value,  &
    & idof_value)
    CLASS(STVectorField_), INTENT(IN) :: obj
    CLASS(AbstractNodeField_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B), INTENT(IN) :: ivar_value
    INTEGER(I4B), INTENT(IN) :: idof_value
  END SUBROUTINE stvField_Get11
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetFEVariable@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: Set single entry

INTERFACE STVectorFieldGetFEVariable
  MODULE SUBROUTINE stvField_GetFeVariable(obj, globalNode, VALUE, ivar)
    CLASS(STVectorField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
  END SUBROUTINE stvField_GetFeVariable
END INTERFACE STVectorFieldGetFEVariable

!----------------------------------------------------------------------------
!                                                   GetPrefix@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-26
! summary:  Get prefix

INTERFACE
  MODULE FUNCTION stvField_GetPrefix(obj) RESULT(ans)
    CLASS(STVectorField_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION stvField_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!                                               ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 22 Jan 2021
! summary: Apply Dirichlet boundary condition

INTERFACE
  MODULE SUBROUTINE stvField_ApplyDirichletBC1(obj, dbc, times)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    CLASS(DirichletBC_), INTENT(IN) :: dbc
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
  END SUBROUTINE stvField_ApplyDirichletBC1
END INTERFACE

!----------------------------------------------------------------------------
!                                               ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 22 Jan 2021
! summary: Apply Dirichlet boundary condition

INTERFACE
  MODULE SUBROUTINE stvField_ApplyDirichletBC2(obj, dbc, times)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    CLASS(DirichletBCPointer_), INTENT(IN) :: dbc(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
  END SUBROUTINE stvField_ApplyDirichletBC2
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetPointerOfComponent@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns pointer to a specific component

INTERFACE
  MODULE FUNCTION stvField_GetPointerOfComponent(obj, spaceCompo,  &
    & timeCompo) RESULT(ans)
    CLASS(STVectorField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), POINTER :: ans(:)
  END FUNCTION stvField_GetPointerOfComponent
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE STVectorField_Class
