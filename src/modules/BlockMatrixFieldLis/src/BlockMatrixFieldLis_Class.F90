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

MODULE BlockMatrixFieldLis_Class
USE AbstractField_Class, ONLY: AbstractField_
USE AbstractMatrixField_Class, ONLY: AbstractMatrixField_
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE BlockMatrixField_Class, ONLY: BlockMatrixField_
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDOF_Class, ONLY: FEDOFPointer_
USE FPL, ONLY: ParameterList_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE HDF5File_Class, ONLY: HDF5File_
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
USE TimeFEDOF_Class, ONLY: TimeFEDOFPointer_
IMPLICIT NONE
PRIVATE

PUBLIC :: BlockMatrixFieldLis_

CHARACTER(*), PARAMETER :: modName = "BlockMatrixFieldLis_Class"

!----------------------------------------------------------------------------
!                                                          BlockMatrixField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 July 2021
! summary: This is native implementation of finite element tangent matrices.

TYPE, EXTENDS(BlockMatrixField_) :: BlockMatrixFieldLis_
  INTEGER(I4B), ALLOCATABLE :: lis_ia(:)
  INTEGER(I4B), ALLOCATABLE :: lis_ja(:)

CONTAINS
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate by copy
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  FINAL :: obj_Final
  !! Finalizer

  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the field
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import from hdf5 file
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! export matrix field in hdf5file_

  ! @MatvecMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Matvec2 => obj_Matvec2
  !! Matrix vector multiplication
END TYPE BlockMatrixFieldLis_

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(BlockMatrixFieldLis_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-30
! summary: This routine initiates the Matrix Field
!
!# Introduction
!
! This routine initiates the `obj` by copying contents
! from `obj2`, an instance of chid class of [[AbstractField_]].
! In this way, we try to minimize the computation effort.
!
!@todo
! At present, the routine works for `copyFull=.TRUE., copyStructure=.TRUE.,
! usePointer=.TRUE.`, which equivalent to the default behavior.
! Add functionality for other options too.
!@endtodo

INTERFACE
  MODULE SUBROUTINE obj_Initiate1( &
    obj, obj2, copyFull, copyStructure, usePointer)
    CLASS(BlockMatrixFieldLis_), INTENT(INOUT) :: obj
    CLASS(AbstractField_), INTENT(INOUT) :: obj2
    !! It should be an instance of MatrixField_
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyFull
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyStructure
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: usePointer
  END SUBROUTINE obj_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-30
! summary: This routine deallocates the data stored inside the matrix

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(BlockMatrixFieldLis_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-30
! summary: This routine displays the content

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(BlockMatrixFieldLis_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-30
! summary: This routine Exports the content of matrixfield_ to hdf5 file

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(BlockMatrixFieldLis_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-31
! summary: This routine Imports the content of matrix field from hdf5file

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, rowFedof, rowFedofs, &
                               rowGeoFedof, rowGeoFedofs, colFedof, &
                               colFedofs, colGeoFedof, colGeoFedofs)
    CLASS(BlockMatrixFieldLis_), INTENT(INOUT) :: obj
    !! matrix field, not initiated
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file open in read mode
    CHARACTER(*), INTENT(IN) :: group
    !! group of the matrix field in the HDF5 file
    CLASS(FEDOF_), OPTIONAL, TARGET, INTENT(IN) :: rowFedof
    !! FEDOF of the rows
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: rowFedofs(:)
    !! FEDOF of the rows, one for each physical variable
    CLASS(FEDOF_), OPTIONAL, TARGET, INTENT(IN) :: rowGeoFedof
    !! geometric FEDOF of the rows
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: rowGeoFedofs(:)
    !! geometric FEDOF of the rows, one for each physical variable
    CLASS(FEDOF_), OPTIONAL, TARGET, INTENT(IN) :: colFedof
    !! FEDOF of the columns, default is rowFedof
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: colFedofs(:)
    !! FEDOF of the columns, default is rowFedofs
    CLASS(FEDOF_), OPTIONAL, TARGET, INTENT(IN) :: colGeoFedof
    !! geometric FEDOF of the columns, default is rowGeoFedof
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: colGeoFedofs(:)
    !! geometric FEDOF of the columns, default is rowGeoFedofs
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Matvec@MatVecMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: This routine returns the maxtrix vector multiplication
!
!# Introduction
!
! This routine returns the matrix vector multiplication. Here, input vector
! is an instance of AbstractNodeField.
! The output vector is also an instance of AbstractNodeField.
! It should be noted that the output vector should be allocated
! outside and it should have same length as the input vector.

INTERFACE
  MODULE SUBROUTINE obj_Matvec2(obj, x, y, isTranspose, &
                                addContribution, scale)
    CLASS(BlockMatrixFieldLis_), INTENT(IN) :: obj
    CLASS(AbstractNodeField_), INTENT(IN) :: x
    !! Input vector in y=Ax
    CLASS(AbstractNodeField_), INTENT(INOUT) :: y
    !! Output vector y=Ax
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
  END SUBROUTINE obj_Matvec2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE BlockMatrixFieldLis_Class
