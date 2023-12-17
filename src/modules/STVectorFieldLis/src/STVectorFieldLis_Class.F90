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
! date: 2023-03-25
! summary: STVector field data type is defined

MODULE STVectorFieldLis_Class
USE GlobalData
USE BaSetype
USE AbstractField_Class
USE AbstractNodeField_Class
USE STVectorField_Class
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE HDF5File_Class
USE Domain_Class
USE DirichletBC_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "STVectorFieldLis_Class"
CHARACTER(*), PARAMETER :: myprefix = "STVectorField"
PUBLIC :: STVectorFieldLis_
PUBLIC :: TypeSTVectorFieldLis
PUBLIC :: STVectorFieldLisPointer_
PUBLIC :: STVectorFieldLis
PUBLIC :: STVectorFieldLis_Pointer

!----------------------------------------------------------------------------
!                                                          STVectorFieldLis_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-25
! summary: STVector field
!
!{!pages/docs-api/STVectorFieldLis/STVectorFieldLis_.md!}

TYPE, EXTENDS(STVectorField_) :: STVectorFieldLis_
#ifdef USE_LIS
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => obj_Initiate1
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  FINAL :: obj_Final
  PROCEDURE, PUBLIC, PASS(obj) :: Size => obj_Size
  PROCEDURE, PUBLIC, PASS(obj) :: Norm2 => obj_Norm2
  PROCEDURE, PUBLIC, PASS(obj) :: Norm1 => obj_Norm1
  PROCEDURE, PUBLIC, PASS(obj) :: Normi => obj_Normi
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !
  ! @SetMethods
  !
  PROCEDURE, PUBLIC, PASS(obj) :: SetSingle => obj_SetSingle
  PROCEDURE, PASS(obj) :: SetAll => obj_SetAll
  PROCEDURE, PASS(obj) :: SetMultiple => obj_SetMultiple
  PROCEDURE, PASS(obj) :: Set1 => obj_Set1
    !! Set single entry
  PROCEDURE, PASS(obj) :: Set2 => obj_Set2
    !! Set all values to a STVector values
  PROCEDURE, PASS(obj) :: Set3 => obj_Set3
    !! Set all values to a given STvector
  PROCEDURE, PASS(obj) :: Set4 => obj_Set4
    !! Set selected values to given STVector
  PROCEDURE, PASS(obj) :: Set5 => obj_Set5
    !! Set selected values to given STvector
  PROCEDURE, PASS(obj) :: Set6 => obj_Set6
    !! Set values to a STVector by using triplet
  PROCEDURE, PASS(obj) :: Set7 => obj_Set7
    !! Set values to a STvector by using triplet
  PROCEDURE, PASS(obj) :: Set8 => obj_Set8
    !! Set values to a STvector by using triplet
  PROCEDURE, PASS(obj) :: Set9 => obj_Set9
    !! Set values to a STvector by using triplet
  PROCEDURE, PASS(obj) :: Set10 => obj_Set10
    !! Set values to a STvector by using triplet
  PROCEDURE, PASS(obj) :: Set11 => obj_Set11
    !! Set values to a STvector by using triplet
  PROCEDURE, PASS(obj) :: Set12 => obj_Set12
    !! Set values to a STvector by using triplet
  PROCEDURE, PASS(obj) :: Set13 => obj_Set13
  PROCEDURE, PASS(obj) :: Set14 => obj_Set14
    !! Set values to a STvector by using triplet
  !
  ! @GetMethods
  !
  PROCEDURE, PUBLIC, PASS(obj) :: GetSingle => obj_GetSingle
  PROCEDURE, PASS(obj) :: Get1 => obj_Get1
  PROCEDURE, PASS(obj) :: Get2 => obj_Get2
  PROCEDURE, PASS(obj) :: Get3 => obj_Get3
  PROCEDURE, PASS(obj) :: Get4 => obj_Get4
  PROCEDURE, PASS(obj) :: Get5 => obj_Get5
  PROCEDURE, PASS(obj) :: Get6 => obj_Get6
  PROCEDURE, PASS(obj) :: Get7 => obj_Get7
  PROCEDURE, PASS(obj) :: Get8 => obj_Get8
  PROCEDURE, PASS(obj) :: Get9 => obj_Get9
  PROCEDURE, PASS(obj) :: Get10 => obj_Get10
  PROCEDURE, PUBLIC, PASS(obj) :: GetPointer =>  &
    & obj_GetPointer
    !! Get the entries of STVector field
  PROCEDURE, PUBLIC, PASS(obj) :: GetPointerOfComponent =>  &
    & obj_GetPointerOfComponent
    !! Get the entries of STVector field
#endif
END TYPE STVectorFieldLis_

TYPE(STVectorFieldLis_), PARAMETER :: TypeSTVectorFieldLis =  &
  & STVectorFieldLis_(domains=NULL())

!---------------------------------------------------------------------------
!                                                     STVectorFieldLisPointer_
!----------------------------------------------------------------------------

TYPE :: STVectorFieldLisPointer_
  CLASS(STVectorFieldLis_), POINTER :: ptr => NULL()
END TYPE STVectorFieldLisPointer_

!----------------------------------------------------------------------------
!                                                         STVector@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-26
! summary: This function returns an instance of [[STVectorFieldLis_]]

INTERFACE STVectorFieldLis
  MODULE FUNCTION obj_Constructor1(param, dom) RESULT(Ans)
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
    TYPE(STVectorFieldLis_) :: ans
  END FUNCTION obj_Constructor1
END INTERFACE STVectorFieldLis

!----------------------------------------------------------------------------
!                                          STVectorFieldLis_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-26
! summary:         This function returns an instance of [[STVectorFieldLis_]]

INTERFACE STVectorFieldLis_Pointer
  MODULE FUNCTION obj_Constructor_1(param, dom) RESULT(Ans)
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
    CLASS(STVectorFieldLis_), POINTER :: ans
  END FUNCTION obj_Constructor_1
END INTERFACE STVectorFieldLis_Pointer

#ifdef USE_LIS

!----------------------------------------------------------------------------
!                                                          Norm2@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-26
! summary: Returns L2 norm

INTERFACE
  MODULE FUNCTION obj_Norm2(obj) RESULT(ans)
    CLASS(STVectorFieldLis_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION obj_Norm2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Norm2@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-26
! summary: Returns L1 norm

INTERFACE
  MODULE FUNCTION obj_Norm1(obj) RESULT(ans)
    CLASS(STVectorFieldLis_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION obj_Norm1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Norm2@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-26
! summary: Returns infinity norm

INTERFACE
  MODULE FUNCTION obj_Normi(obj) RESULT(ans)
    CLASS(STVectorFieldLis_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION obj_Normi
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Size@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: Returns the size

INTERFACE
  MODULE FUNCTION obj_Size(obj, dims) RESULT(ans)
    CLASS(STVectorFieldLis_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL :: dims
    INTEGER(I4B) :: ans
  END FUNCTION obj_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-26
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
  MODULE SUBROUTINE obj_Initiate1(obj, param, dom)
    CLASS(STVectorFieldLis_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE obj_Initiate1
END INTERFACE

INTERFACE STVectorFieldLisInitiate1
  MODULE PROCEDURE obj_Initiate1
END INTERFACE STVectorFieldLisInitiate1

PUBLIC :: STVectorFieldLisInitiate1

!----------------------------------------------------------------------------
!                                                 Deallocate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine deallocates the data stored inside the STVectorFieldLis_ obj

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(STVectorFieldLis_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Final@Constructor
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(STVectorFieldLis_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Display the content of [[STVectorFieldLis_]]

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(STVectorFieldLis_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Imports the content

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, dom, domains)
    CLASS(STVectorFieldLis_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    TYPE(Domain_), TARGET, OPTIONAL, INTENT(IN) :: dom
    TYPE(DomainPointer_), TARGET, OPTIONAL, INTENT(IN) :: domains(:)
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Exports the content

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(STVectorFieldLis_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetSingle@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetSingle(obj, indx, VALUE, scale, &
    & addContribution)
    CLASS(STVectorFieldLis_), INTENT(INOUT) :: obj
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
    CLASS(STVectorFieldLis_), INTENT(INOUT) :: obj
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
    CLASS(STVectorFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_SetMultiple
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the single entry of the STVector field
!
!# Introduction
! This routine Sets the single entry of the STVectorField.
! Here, `value` denotes the space-time components of the vector.
! The first index denotes the space components,
! The second index denotes the time-components.
! As a result, total number of rows and columns in `value`
! are equal to the total number of spaceCompo and
! timeCompo.
!
! STvector( :, :, globalNode ) = value( :, : )

INTERFACE
  MODULE SUBROUTINE obj_Set1(obj, globalNode, VALUE, scale, &
    & addContribution)
    CLASS(STVectorFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set1
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
  MODULE SUBROUTINE obj_Set2(obj, VALUE, scale, addContribution)
    CLASS(STVectorFieldLis_), TARGET, INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets all the entries of a STVector field
!
!# Introduction
! This routine Sets single entry of the space-time vector field. Here
! `spaceCompo` and `timeCompo` are the spatial temporal components, which we
! want to replace by a scalar value `value`.
!
! STvector( spaceCompo, timeCompo, i ) = value, for i = 1, tNodes

INTERFACE
  MODULE SUBROUTINE obj_Set3(obj, VALUE, spaceCompo, timeCompo, &
    & scale, addContribution)
    CLASS(STVectorFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set3
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
  MODULE SUBROUTINE obj_Set4(obj, VALUE, scale, addContribution)
    CLASS(STVectorFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:, :, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set4
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
  MODULE SUBROUTINE obj_Set5(obj, VALUE, spaceCompo, timeCompo,  &
    & scale, addContribution)
    CLASS(STVectorFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set5
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
  MODULE SUBROUTINE obj_Set6(obj, VALUE, spaceCompo, timeCompo,  &
    & scale, addContribution)
    CLASS(STVectorFieldLis_), INTENT(INOUT) :: obj
    CLASS(AbstractNodeField_), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set6
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
! denotes the space component and second component denotes the time component.
!
!Effectively it does the following:
!
! STvector( :, :, globalNode ) = value( :, : ), for entries in global nodes

INTERFACE
  MODULE SUBROUTINE obj_Set7(obj, VALUE, globalNode, scale,  &
    & addContribution)
    CLASS(STVectorFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set7
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
  MODULE SUBROUTINE obj_Set8(obj, globalNode, VALUE, scale, &
    & addContribution)
    CLASS(STVectorFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:, :, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set8
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the selected entries
!
!# Introduction
! This routine Sets the selected components of selected nodes to a given value
!
! STvector( spaceCompo, globalNode ) = value( : )

INTERFACE
  MODULE SUBROUTINE obj_Set9(obj, VALUE, globalNode, spaceCompo,  &
    & timeCompo, scale, addContribution)
    CLASS(STVectorFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
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
! date: 25 June 2021
! summary: This routine Sets the selected entries
!
!# Introduction
! selected components, selected nodes
!
! STvector( spaceCompo, globalNode ) = value

INTERFACE
  MODULE SUBROUTINE obj_Set10(obj, VALUE, globalNode, spaceCompo, &
    & timeCompo, scale, addContribution)
    CLASS(STVectorFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set10
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the selected entries

INTERFACE
  MODULE SUBROUTINE obj_Set11(obj, VALUE, istart, iend, stride,  &
    & scale, addContribution)
    CLASS(STVectorFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set11
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Set the STvector values using triplet

INTERFACE
  MODULE SUBROUTINE obj_Set12(obj, VALUE, istart, iend, stride,  &
    & scale, addContribution)
    CLASS(STVectorFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:, :, :)
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set12
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Set the STvector values using triplet

INTERFACE
  MODULE SUBROUTINE obj_Set13(obj, VALUE, globalNode, scale, &
    & addContribution)
    CLASS(STVectorFieldLis_), INTENT(INOUT) :: obj
    TYPE(FEVariable_), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set13
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Set the STvector values using triplet

INTERFACE
  MODULE SUBROUTINE obj_Set14(obj, VALUE, scale, addContribution)
    CLASS(STVectorFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set14
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
  MODULE SUBROUTINE obj_Get1(obj, VALUE, globalNode, spaceCompo, &
    & timeCompo)
    CLASS(STVectorFieldLis_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalNode
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo
  END SUBROUTINE obj_Get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Get all the entries of the space-time vector field
!
!# Introduction
! This routine returns all the nodal values of a space-time nodal vector field.
! Here value is a rank3 array of reals.
!
! - Its first index denotes the spatial component
! - the second index denotes the temporal component
! - the third index denotes the node number.

INTERFACE
  MODULE SUBROUTINE obj_Get2(obj, VALUE)
    CLASS(STVectorFieldLis_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :, :)
  END SUBROUTINE obj_Get2
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
  MODULE SUBROUTINE obj_Get3(obj, VALUE, globalNode)
    CLASS(STVectorFieldLis_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :, :)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
  END SUBROUTINE obj_Get3
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
  MODULE SUBROUTINE obj_Get4(obj, VALUE, globalNode, spaceCompo, &
    & timeCompo)
    CLASS(STVectorFieldLis_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
  END SUBROUTINE obj_Get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE obj_Get5(obj, VALUE, globalNode, spaceCompo, &
    & timeCompo)
    CLASS(STVectorFieldLis_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
  END SUBROUTINE obj_Get5
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE obj_Get6(obj, VALUE, istart, iend, stride)
    CLASS(STVectorFieldLis_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :, :)
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
  END SUBROUTINE obj_Get6
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE obj_Get7(obj, VALUE, istart, iend, stride, &
    & spaceCompo, timeCompo)
    CLASS(STVectorFieldLis_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
  END SUBROUTINE obj_Get7
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the space-time value at given node number

INTERFACE
  MODULE SUBROUTINE obj_Get8(obj, VALUE, globalNode)
    CLASS(STVectorFieldLis_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
    INTEGER(I4B), INTENT(IN) :: globalNode
  END SUBROUTINE obj_Get8
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the space-time value at given node number

INTERFACE
  MODULE SUBROUTINE obj_Get9(obj, VALUE, globalNode)
    CLASS(STVectorFieldLis_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
  END SUBROUTINE obj_Get9
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine return value in FEVariable

INTERFACE
  MODULE SUBROUTINE obj_Get10(obj, VALUE, spaceCompo, timeCompo)
    CLASS(STVectorFieldLis_), INTENT(IN) :: obj
    CLASS(AbstractField_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo
  END SUBROUTINE obj_Get10
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: This routine cannot be called

INTERFACE
  MODULE FUNCTION obj_GetPointer(obj) RESULT(ans)
    CLASS(STVectorFieldLis_), TARGET, INTENT(IN) :: obj
    REAL(DFP), POINTER :: ans(:)
  END FUNCTION obj_GetPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetPointerOfComponent@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns pointer to a specific component

INTERFACE
  MODULE FUNCTION obj_GetPointerOfComponent(obj, spaceCompo, timeCompo) &
    & RESULT(ans)
    CLASS(STVectorFieldLis_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), POINTER :: ans(:)
  END FUNCTION obj_GetPointerOfComponent
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetSingle@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: Get single entry

INTERFACE
  MODULE SUBROUTINE obj_GetSingle(obj, indx, VALUE)
    CLASS(STVectorFieldLis_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    REAL(DFP), INTENT(OUT) :: VALUE
  END SUBROUTINE obj_GetSingle
END INTERFACE
#endif

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE STVectorFieldLis_Class
