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
USE ScalarField_Class
USE VectorField_Class
USE STScalarField_Class
USE STVectorField_Class
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE HDF5File_Class
USE Domain_Class
USE DirichletBC_Class
IMPLICIT NONE
PRIVATE
CHARACTER(LEN=*), PARAMETER :: modName = "BlockNodeField_Class"

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
  PROCEDURE, PUBLIC, PASS(obj) :: initiate1 => bnField_initiate1
  PROCEDURE, PUBLIC, PASS(obj) :: initiate3 => bnField_initiate3
  PROCEDURE, PUBLIC, PASS(obj) :: Deallocate => bnField_Deallocate
  FINAL :: bnField_Final
  !! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => bnField_Display
  PROCEDURE, PUBLIC, PASS(obj) :: Import => bnField_Import
  PROCEDURE, PUBLIC, PASS(obj) :: Export => bnField_Export
  !! @SetMethods
  PROCEDURE, PASS(obj) :: set1 => bnField_set1
    !! set single entry
  PROCEDURE, PASS(obj) :: set2 => bnField_set2
    !! set all values to a scalar values
  PROCEDURE, PASS(obj) :: assign => bnField_assign
    !! set all values to a scalar values
  GENERIC, PUBLIC :: ASSIGNMENT(=) => assign
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
  GENERIC, PUBLIC :: set => set1, set2, set3, set4, &
    & set5, set6, set7, set8, set9, set10, set11, &
    & set12, set13, set14, set15, set16, set17
  !! @GetMethods
  PROCEDURE, PASS(obj) :: get1 => bnField_get1
  PROCEDURE, PASS(obj) :: get2 => bnField_get2
  PROCEDURE, PASS(obj) :: get3 => bnField_get3
  PROCEDURE, PASS(obj) :: get4 => bnField_get4
  PROCEDURE, PASS(obj) :: get5 => bnField_get5
  PROCEDURE, PASS(obj) :: get6 => bnField_get6
  PROCEDURE, PASS(obj) :: get7 => bnField_get7
  PROCEDURE, PASS(obj) :: get8 => bnField_get8
  PROCEDURE, PASS(obj) :: get9 => bnField_get9
  PROCEDURE, PASS(obj) :: get10 => bnField_get10
  PROCEDURE, PASS(obj) :: get11 => bnField_get11
  PROCEDURE, PASS(obj) :: get12 => bnField_get12
  PROCEDURE, PASS(obj) :: get13 => bnField_get13
  PROCEDURE, PASS(obj) :: get14 => bnField_get14
  PROCEDURE, PASS(obj) :: get15 => bnField_get15
  PROCEDURE, PASS(obj) :: get16 => bnField_get16
  GENERIC, PUBLIC :: get => get1, get2, get3, get4, &
    & get5, get6, get7, get8, get9, get10, get11, get12, &
    & get13, get14, get15, get16
  PROCEDURE, PASS(obj) :: bnField_applyDirichletBC1
  PROCEDURE, PASS(obj) :: bnField_applyDirichletBC2
  GENERIC, PUBLIC :: applyDirichletBC => &
    & bnField_applyDirichletBC1, &
    & bnField_applyDirichletBC2
  !!
  !! @Operator
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
  MODULE SUBROUTINE SetBlockNodeFieldParam(param, name, physicalVarNames, &
    & spaceCompo, timeCompo, fieldType)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    !! Options to create [[BlockNodeField_]] will be stored in param
    CHARACTER(LEN=*), INTENT(IN) :: name
    !! Name of the block node field
    CHARACTER(LEN=*), INTENT(IN) :: physicalVarNames(:)
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

!----------------------------------------------------------------------------
!                                           Deallocate@ConstructorMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: Deallocates the data stored inside the [[BlockNodeField_]] obj

INTERFACE
  MODULE SUBROUTINE bnField_Deallocate(obj)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
  END SUBROUTINE bnField_Deallocate
END INTERFACE

INTERFACE Deallocate
  MODULE PROCEDURE bnField_Deallocate
END INTERFACE Deallocate

PUBLIC :: Deallocate

!----------------------------------------------------------------------------
!                                                    Final@ConstructorMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE bnField_Final(obj)
    TYPE(BlockNodeField_), INTENT(INOUT) :: obj
  END SUBROUTINE bnField_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Display the content of [[BlockNodeField_]]

INTERFACE
  MODULE SUBROUTINE bnField_Display(obj, msg, unitNo)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    CHARACTER(LEN=*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE bnField_Display
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
    CHARACTER(LEN=*), INTENT(IN) :: group
    TYPE(Domain_), TARGET, OPTIONAL, INTENT(IN) :: dom
    TYPE(DomainPointer_), TARGET, OPTIONAL, INTENT(IN) :: domains(:)
  END SUBROUTINE bnField_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Exports the content

INTERFACE
  MODULE SUBROUTINE bnField_Export(obj, hdf5, group)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(LEN=*), INTENT(IN) :: group
  END SUBROUTINE bnField_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine sets all values to a scalar

INTERFACE
  MODULE SUBROUTINE bnField_set1(obj, value, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: value
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
  MODULE SUBROUTINE bnField_set2(obj, value, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: value(:)
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
  MODULE SUBROUTINE bnField_set3(obj, globalNode, value, ivar, idof, &
    & scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    REAL(DFP), INTENT(IN) :: value
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
  MODULE SUBROUTINE bnField_set4(obj, globalNode, value, ivar, scale, &
    & addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: value
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
  MODULE SUBROUTINE bnField_set5(obj, globalNode, value, ivar, scale, &
    & addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: value(:)
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
  MODULE SUBROUTINE bnField_set6(obj, globalNode, value, ivar, idof, &
    & scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: value
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
  MODULE SUBROUTINE bnField_set7(obj, globalNode, value, ivar, idof, &
    & scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: value(:)
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
  MODULE SUBROUTINE bnField_set8(obj, globalNode, value, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: value(:)
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
  MODULE SUBROUTINE bnField_set9(obj, globalNode, value, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: value
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
  MODULE SUBROUTINE bnField_set10(obj, globalNode, value, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: value(:)
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
  MODULE SUBROUTINE bnField_set11(obj, globalNode, value, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: value
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
  MODULE SUBROUTINE bnField_set12(obj, globalNode, value, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: value(:)
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
  MODULE SUBROUTINE bnField_set13(obj, globalNode, value, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: value
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
  MODULE SUBROUTINE bnField_set14(obj, globalNode, value, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    REAL(DFP), INTENT(IN) :: value
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
  MODULE SUBROUTINE bnField_set15(obj, globalNode, value, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    REAL(DFP), INTENT(IN) :: value
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
  MODULE SUBROUTINE bnField_set16(obj, globalNode, value, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    REAL(DFP), INTENT(IN) :: value
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
! date: 06 Jan 2022
! summary: This routine sets all values to a scalar

INTERFACE
  MODULE SUBROUTINE bnField_assign(obj, value)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: value
  END SUBROUTINE bnField_assign
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE bnField_get1(obj, value, globalNode, ivar, idof)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: value
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
  MODULE SUBROUTINE bnField_get2(obj, value)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: value(:)
  END SUBROUTINE bnField_get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE bnField_get3(obj, value, globalNode, ivar, idof)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: value(:)
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
  MODULE SUBROUTINE bnField_get4(obj, value, istart, iend, stride,  &
    & ivar, idof)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: value(:)
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
  MODULE SUBROUTINE bnField_get5(obj, value, globalNode, &
    & ivar, idof)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: value
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
  MODULE SUBROUTINE bnField_get6(obj, value, globalNode, ivar)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: value
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
  MODULE SUBROUTINE bnField_get7(obj, value, globalNode, ivar, &
    & spaceCompo, timeCompo)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: value(:)
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
  MODULE SUBROUTINE bnField_get8(obj, value, globalNode, ivar, &
    & spaceCompo, timeCompo)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: value
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
! summary: Returns ivar,idof in a scalar field

INTERFACE
  MODULE SUBROUTINE bnField_get9(obj, value, ivar, idof)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    CLASS(ScalarField_), INTENT(INOUT) :: value
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE bnField_get9
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE bnField_get10(obj, value, ivar, spaceCompo, timeCompo)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    CLASS(ScalarField_), INTENT(INOUT) :: value
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
  END SUBROUTINE bnField_get10
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE bnField_get11(obj, value, ivar, spaceCompo, timeCompo)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    CLASS(STScalarField_), INTENT(INOUT) :: value
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo(:)
  END SUBROUTINE bnField_get11
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE bnField_get12(obj, value, ivar, spaceCompo, timeCompo)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    CLASS(VectorField_), INTENT(INOUT) :: value
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
    INTEGER(I4B), INTENT(IN) :: timeCompo
  END SUBROUTINE bnField_get12
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE bnField_get13(obj, value, ivar)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    CLASS(ScalarField_), INTENT(INOUT) :: value
    INTEGER(I4B), INTENT(IN) :: ivar
  END SUBROUTINE bnField_get13
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE bnField_get14(obj, value, ivar)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    CLASS(STScalarField_), INTENT(INOUT) :: value
    INTEGER(I4B), INTENT(IN) :: ivar
  END SUBROUTINE bnField_get14
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE bnField_get15(obj, value, ivar)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    CLASS(VectorField_), INTENT(INOUT) :: value
    INTEGER(I4B), INTENT(IN) :: ivar
  END SUBROUTINE bnField_get15
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE bnField_get16(obj, value, ivar)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    CLASS(STVectorField_), INTENT(INOUT) :: value
    INTEGER(I4B), INTENT(IN) :: ivar
  END SUBROUTINE bnField_get16
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
  MODULE PURE FUNCTION bnField_isEqual(obj, obj2) RESULT(Ans)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    CLASS(BlockNodeField_), INTENT(IN) :: obj2
    LOGICAL(LGT) :: ans
  END FUNCTION bnField_isEqual
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE BlockNodeField_Class
