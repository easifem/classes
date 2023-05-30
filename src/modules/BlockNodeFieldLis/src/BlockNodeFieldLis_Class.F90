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

MODULE BlockNodeFieldLis_Class
USE GlobalData
USE BaseType
USE String_Class
USE AbstractField_Class
USE AbstractNodeField_Class
USE BlockNodeField_Class
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE HDF5File_Class
USE Domain_Class
USE DirichletBC_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "BlockNodeFieldLis_Class"
CHARACTER(*), PARAMETER :: myprefix = "BlockNodeField"

!----------------------------------------------------------------------------
!                                                         BlockNodeFieldLis_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This nodal field is designed for the multiphysics applications
!
!{!pages/BlockNodeFieldLis_.md}

TYPE, EXTENDS(BlockNodeField_) :: BlockNodeFieldLis_

#ifdef USE_LIS
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate3 => bnField_Initiate3
  PROCEDURE, PUBLIC, PASS(obj) :: Size => bnField_Size
  PROCEDURE, PUBLIC, PASS(obj) :: Norm2 => bnField_Norm2
  PROCEDURE, PUBLIC, PASS(obj) :: Norm1 => bnField_Norm1
  PROCEDURE, PUBLIC, PASS(obj) :: Normi => bnField_Normi
  FINAL :: bnField_Final
  !
  ! @IOMethods
  !
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => bnField_Import
  PROCEDURE, PUBLIC, PASS(obj) :: Export => bnField_Export
  PROCEDURE, PUBLIC, PASS(obj) :: Display => bnField_Display
  !
  ! @SetMethods
  !
  PROCEDURE, PUBLIC, PASS(obj) :: SetSingle => bnField_SetSingle
  PROCEDURE, PASS(obj) :: SetAll => bnField_SetAll
  PROCEDURE, PASS(obj) :: SetMultiple => bnField_SetMultiple
  PROCEDURE, PASS(obj) :: set1 => bnField_set1
    !! set single entry
  PROCEDURE, PASS(obj) :: set2 => bnField_set2
    !! set all values to a scalar values
  PROCEDURE, PASS(obj) :: ASSIGN => bnField_assign
    !! set all values to a scalar values
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
  !
  ! @GetMethods
  !
  PROCEDURE, PUBLIC, PASS(obj) :: GetSingle => bnField_getSingle
  PROCEDURE, PASS(obj) :: get1 => bnField_get1
  PROCEDURE, PASS(obj) :: get2 => bnField_get2
  PROCEDURE, PASS(obj) :: get3 => bnField_get3
  PROCEDURE, PASS(obj) :: get7 => bnField_get7
  !
  ! @Operator
  !
  PROCEDURE, PASS(obj) :: isEqual => bnField_isEqual
#endif
END TYPE BlockNodeFieldLis_

PUBLIC :: BlockNodeFieldLis_

TYPE(BlockNodeFieldLis_), PARAMETER, PUBLIC :: TypeBlockNodeFieldLis = &
& BlockNodeFieldLis_(domains=NULL())

!----------------------------------------------------------------------------
!                                                  BlockNodeFieldLisPointer_
!----------------------------------------------------------------------------

TYPE :: BlockNodeFieldLisPointer_
  CLASS(BlockNodeFieldLis_), POINTER :: ptr => NULL()
END TYPE BlockNodeFieldLisPointer_

PUBLIC :: BlockNodeFieldLisPointer_

#ifdef USE_LIS

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This subroutine initiates the BlockNodeFieldLis_ object
!
!# Introduction
!
! This routine initiate the [[BlockNodeFieldLis_]] object.
! `param` contains the information of parameters required to initiate the
! instance of BlockNodeFieldLis_ .
!
! - It is better to make `param` by calling
! [[BlockNodeFieldLis_::setBlockNodeFieldParam]]
! - The size of `dom` should be equal to the number of physical variables
! present in the block node field.
! - `dom` contains the pointer to [[Domain_]] class.

INTERFACE
  MODULE SUBROUTINE bnField_Initiate3(obj, param, dom)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(DomainPointer_), TARGET, INTENT(IN) :: dom(:)
  END SUBROUTINE bnField_Initiate3
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Final@ConstructorMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE bnField_Final(obj)
    TYPE(BlockNodeFieldLis_), INTENT(INOUT) :: obj
  END SUBROUTINE bnField_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Norm2@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-26
! summary: Returns L2 norm

INTERFACE
  MODULE FUNCTION bnField_Norm2(obj) RESULT(ans)
    CLASS(BlockNodeFieldLis_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION bnField_Norm2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Norm2@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-26
! summary: Returns L1 norm

INTERFACE
  MODULE FUNCTION bnField_Norm1(obj) RESULT(ans)
    CLASS(BlockNodeFieldLis_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION bnField_Norm1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Norm2@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-26
! summary: Returns infinity norm

INTERFACE
  MODULE FUNCTION bnField_Normi(obj) RESULT(ans)
    CLASS(BlockNodeFieldLis_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION bnField_Normi
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Size@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: Returns the size

INTERFACE
  MODULE FUNCTION bnField_Size(obj, dims) RESULT(ans)
    CLASS(BlockNodeFieldLis_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL :: dims
    INTEGER(I4B) :: ans
  END FUNCTION bnField_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-28
! summary: Display the content

INTERFACE
  MODULE SUBROUTINE bnField_Display(obj, msg, unitNo)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE bnField_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-28
! summary: This routine Imports the content

INTERFACE
  MODULE SUBROUTINE bnField_Import(obj, hdf5, group, dom, domains)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    TYPE(Domain_), TARGET, OPTIONAL, INTENT(IN) :: dom
    TYPE(DomainPointer_), TARGET, OPTIONAL, INTENT(IN) :: domains(:)
  END SUBROUTINE bnField_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: This routine Exports the content

INTERFACE
  MODULE SUBROUTINE bnField_Export(obj, hdf5, group)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE bnField_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetSingle@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE bnField_setSingle(obj, indx, VALUE, scale, &
    & addContribution)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE bnField_setSingle
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetAll@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE bnField_setAll(obj, VALUE, scale, addContribution)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE bnField_setAll
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMultiple@SetMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE bnField_setMultiple(obj, indx, VALUE, scale, addContribution)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE bnField_setMultiple
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine sets all values to a scalar

INTERFACE
  MODULE SUBROUTINE bnField_set1(obj, VALUE, scale, addContribution)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
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
!
!# Introduction
!
! Set all values by using a vector.
! The size of value should be same as the size of obj

INTERFACE
  MODULE SUBROUTINE bnField_set2(obj, VALUE, scale, addContribution)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
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
!
!# Introduction
!
! Set single entry.

INTERFACE
  MODULE SUBROUTINE bnField_set3(obj, globalNode, VALUE, ivar, idof, &
    & scale, addContribution)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    REAL(DFP), INTENT(IN) :: VALUE
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
  MODULE SUBROUTINE bnField_set4(obj, globalNode, VALUE, ivar, scale, &
    & addContribution)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE
    !! constant scalar value
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
  MODULE SUBROUTINE bnField_set5(obj, globalNode, VALUE, ivar, scale, &
    & addContribution)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! The storage format should be `FMT_DOF`
    !! The size should be size(globalNode) times tdof in ivar
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
  MODULE SUBROUTINE bnField_set6(obj, globalNode, VALUE, ivar, idof, &
    & scale, addContribution)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE
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
  MODULE SUBROUTINE bnField_set7(obj, globalNode, VALUE, ivar, idof, &
    & scale, addContribution)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
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
  MODULE SUBROUTINE bnField_set8(obj, globalNode, VALUE, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
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
  MODULE SUBROUTINE bnField_set9(obj, globalNode, VALUE, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE
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
  MODULE SUBROUTINE bnField_set10(obj, globalNode, VALUE, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! size of value should be equal to the size of globalNode times
    !! the size of timeCompo
    !! The storage format is FMT_DOF
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
  MODULE SUBROUTINE bnField_set11(obj, globalNode, VALUE, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE
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
  MODULE SUBROUTINE bnField_set12(obj, globalNode, VALUE, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! Size of value should be size(globalNode) times size(spaceCompo)
    !! Storage format should be FMT_DOF
    !! (x1, x2, x3, ...., y1, y2, y3, ..., z1, z2, z3, ...)
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
  MODULE SUBROUTINE bnField_set13(obj, globalNode, VALUE, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE
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
  MODULE SUBROUTINE bnField_set14(obj, globalNode, VALUE, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    REAL(DFP), INTENT(IN) :: VALUE
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
  MODULE SUBROUTINE bnField_set15(obj, globalNode, VALUE, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    REAL(DFP), INTENT(IN) :: VALUE
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
  MODULE SUBROUTINE bnField_set16(obj, globalNode, VALUE, ivar, &
    & spaceCompo, timeCompo, scale, addContribution)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    REAL(DFP), INTENT(IN) :: VALUE
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
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
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
  MODULE SUBROUTINE bnField_assign(obj, VALUE)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE bnField_assign
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE bnField_get1(obj, VALUE, globalNode, ivar, idof)
    CLASS(BlockNodeFieldLis_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE
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
  MODULE SUBROUTINE bnField_get2(obj, VALUE)
    CLASS(BlockNodeFieldLis_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
  END SUBROUTINE bnField_get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE bnField_get3(obj, VALUE, globalNode, ivar, idof)
    CLASS(BlockNodeFieldLis_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
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
  MODULE SUBROUTINE bnField_get7(obj, VALUE, globalNode, ivar, &
    & spaceCompo, timeCompo)
    CLASS(BlockNodeFieldLis_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
  END SUBROUTINE bnField_get7
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetSingle@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: Get single entry

INTERFACE
  MODULE SUBROUTINE bnField_getSingle(obj, indx, VALUE)
    CLASS(BlockNodeFieldLis_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    REAL(DFP), INTENT(OUT) :: VALUE
  END SUBROUTINE bnField_getSingle
END INTERFACE

!----------------------------------------------------------------------------
!                                                       EQ@OperatorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION bnField_isEqual(obj, obj2) RESULT(Ans)
    CLASS(BlockNodeFieldLis_), INTENT(IN) :: obj
    CLASS(BlockNodeField_), INTENT(IN) :: obj2
    LOGICAL(LGT) :: ans
  END FUNCTION bnField_isEqual
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#endif
END MODULE BlockNodeFieldLis_Class
