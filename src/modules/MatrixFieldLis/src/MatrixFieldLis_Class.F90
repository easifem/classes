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

MODULE MatrixFieldLis_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE FPL, ONLY: ParameterList_
USE HDF5File_Class, ONLY: HDF5File_
USE ExceptionHandler_Class, ONLY: e
USE AbstractField_Class, ONLY: AbstractField_
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE AbstractMatrixField_Class, ONLY: AbstractMatrixField_
USE MatrixField_Class, ONLY: MatrixField_
USE FEDOF_Class, ONLY: FEDOF_, FEDOFPointer_
USE TimeFEDOF_Class, ONLY: TimeFEDOF_, TimeFEDOFPointer_

IMPLICIT NONE

PRIVATE

CHARACTER(*), PRIVATE, PARAMETER :: modName = "MatrixFieldLis_Class"
CHARACTER(*), PRIVATE, PARAMETER :: myPrefix = "MatrixField"

PUBLIC :: MatrixFieldLis_
PUBLIC :: MatrixFieldLisInitiate2
PUBLIC :: MatrixFieldLisDeallocate

!----------------------------------------------------------------------------
!                                                              MatrixField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 July 2021
! summary: This is native implementation of finite element tangent matrices.
!
!{!pages/docs-api/MatrixFieldLis/MatrixFieldLis_.md!}

TYPE, EXTENDS(MatrixField_) :: MatrixFieldLis_
  INTEGER(I4B), ALLOCATABLE :: lis_ia(:)
  INTEGER(I4B), ALLOCATABLE :: lis_ja(:)
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods

  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate from the parameter list
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Initiate by copying other object
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate3 => obj_Initiate3
  !! Initiate for block matrices
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate the field
  FINAL :: obj_Final

  ! IO:
  ! @IOMethods

  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the field
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import from hdf5 file
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! export matrix field in hdf5file_

  ! GET:
  ! @MatvecMethods

  PROCEDURE, PASS(obj) :: Matvec2 => obj_Matvec2
  !! Matrix vector multiplication

END TYPE MatrixFieldLis_

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(MatrixFieldLis_), INTENT(INOUT) :: obj
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
! This routine initiates an instance of [[MatrixField_]].
! The options/arguments to initiate the matrix field are
! contained inside param, which is an instance of [[ParameterList_]].
! In addition, [[Domain_]] `dom` is target to the pointer
! [[AbstractField_:domain]]
!
! - Param contains both essential and optional parameters which are used in
! constructing the matrix field
! - dom is a pointer to a domain, where we are interested in constructing the
! matrix
!
! ESSENTIAL PARAMETERS are
!
! - `name` This is name of field (char)
! - `matrixProp`, UNSYM, SYM (char)
!
! OPTIONAL PARAMETERS
!
! - `spaceCompo`, INT, default is 1
! - `timeCompo`, INT, default is 1
! - `fieldType`, INT, default is FIELD_TYPE_NORMAL

INTERFACE
  MODULE SUBROUTINE obj_Initiate1(obj, param, fedof, timefedof)
    CLASS(MatrixFieldLis_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    CLASS(FEDOF_), TARGET, INTENT(IN) :: fedof
    CLASS(TimeFEDOF_), OPTIONAL, TARGET, INTENT(in) :: timefedof
  END SUBROUTINE obj_Initiate1
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
! This routine initiates the `obj` [[MatrixField_]] by copying contents
! from `obj2`, an instance of chid class of [[AbstractField_]].
! In this way we try to minimize the computation effort.
!
!@note
! If `copyFull, copyStructure, usePointer` are absent then this subroutine,
! copies the value of the matrix from obj2 to obj.
!@endnote
!
!@note
! However, in [[MatrixField_:mat]], it will not allocate space for
! [[CSRSparsity_]] field of
! [[CSRMatrix_]], that is [[CSRMatrix_:CSR]] field of [[MatrixField_:mat]].
! Instead, it will use the obj2%mat%csr as the target for the pointer
! obj%mat%csr.
! In this way, there is no need to create multiple sparsity patterns
! for the same domain.
!@endnote
!
!@todo
! At present, the routine works for `copyFull=.TRUE., copyStructure=.TRUE.,
! usePointer=.TRUE.`, which equivalent to the default behavior.
! Add functionality for other options too.
!@endtodo

INTERFACE MatrixFieldLisInitiate2
  MODULE SUBROUTINE obj_Initiate2(obj, obj2, copyFull, copyStructure, &
                                  usePointer)
    CLASS(MatrixFieldLis_), INTENT(INOUT) :: obj
    CLASS(AbstractField_), INTENT(INOUT) :: obj2
    !! It should be an instance of MatrixField_
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyFull
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyStructure
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: usePointer
  END SUBROUTINE obj_Initiate2
END INTERFACE MatrixFieldLisInitiate2

!----------------------------------------------------------------------------
!                                               Initiate@sConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-30
! summary: This routine initiates the Matrix Field

INTERFACE
  MODULE SUBROUTINE obj_Initiate3(obj, param, fedof, timefedof)
    CLASS(MatrixFieldLis_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(FEDOFPointer_), INTENT(IN) :: fedof(:)
    TYPE(TimeFEDOFPointer_), OPTIONAL, INTENT(IN) :: timefedof(:)
  END SUBROUTINE obj_Initiate3
END INTERFACE

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-30
! summary: This routine deallocates the data stored inside the matrix

INTERFACE MatrixFieldLisDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(MatrixFieldLis_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE MatrixFieldLisDeallocate

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-30
! summary: This routine displays the content

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(MatrixFieldLis_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-30
! summary: This routine Imports the content of matrix field from hdf5file

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, fedof, fedofs, timefedof, &
                               timefedofs)
    CLASS(MatrixFieldLis_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    CLASS(FEDOF_), TARGET, OPTIONAL, INTENT(IN) :: fedof
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: fedofs(:)
    CLASS(TimeFEDOF_), TARGET, OPTIONAL, INTENT(IN) :: timefedof
    TYPE(TimeFEDOFPointer_), OPTIONAL, INTENT(IN) :: timefedofs(:)
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-30
! summary: This routine Exports the content of matrixfield_ to hdf5 file

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(MatrixFieldLis_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
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
    CLASS(MatrixFieldLis_), INTENT(IN) :: obj
    CLASS(AbstractNodeField_), INTENT(IN) :: x
    !! Input vector in y=Ax
    CLASS(AbstractNodeField_), INTENT(INOUT) :: y
    !! Output vector y=Ax
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
  END SUBROUTINE obj_Matvec2
END INTERFACE

END MODULE MatrixFieldLis_Class
