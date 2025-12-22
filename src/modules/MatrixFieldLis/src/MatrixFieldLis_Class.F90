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
USE GlobalData, ONLY: I4B, DFP, LGT, INT64
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

PUBLIC :: MatrixFieldLis_
PUBLIC :: MatrixFieldLisInitiate
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
  INTEGER(I4B), ALLOCATABLE :: lis_ia(:), submat_lis_ia(:)
  !! IA of CSR matrix
  INTEGER(I4B), ALLOCATABLE :: lis_ja(:), submat_lis_ja(:)
  !! JA of CSR matrix
  INTEGER(I4B) :: submat_is = 0_I4B
  !! starting index (MPI)
  INTEGER(I4B) :: submat_ie = 0_I4B
  !! end index + 1 (MPI)
  INTEGER(INT64) :: submat_lis_ptr = 0_INT64
  !! lis_ptr is pointer returned by the LIS library
  !! It is used when engine is LIS_OMP or LIS_MPI
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Initiate by copying other object
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

  ! SET:
  ! @DBCMethods
  PROCEDURE, PASS(obj) :: ApplyDirichletBC1 => obj_ApplyDirichletBC1
  !! Apply dirichlet boundary condition
  PROCEDURE, PUBLIC, PASS(obj) :: ApplyDirichletBCtoRHS => &
    obj_ApplyDirichletBCToRHS
  !! Apply dirichlet boundary conditions to rhs
  PROCEDURE, PUBLIC, PASS(obj) :: GetDirichletBCSubMat => &
    obj_GetDirichletBCSubMat
  !! Get submatrix for dirichlet boundary condition
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

INTERFACE
  MODULE SUBROUTINE obj_Initiate2( &
    obj, obj2, copyFull, copyStructure, usePointer)
    CLASS(MatrixFieldLis_), INTENT(INOUT) :: obj
    CLASS(AbstractField_), INTENT(INOUT) :: obj2
    !! It should be an instance of MatrixField_
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyFull
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyStructure
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: usePointer
  END SUBROUTINE obj_Initiate2
END INTERFACE

INTERFACE MatrixFieldLisInitiate
  MODULE PROCEDURE obj_Initiate2
END INTERFACE MatrixFieldLisInitiate

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-30
! summary: This routine deallocates the data stored inside the matrix

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(MatrixFieldLis_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

INTERFACE MatrixFieldLisDeallocate
  MODULE PROCEDURE obj_Deallocate
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
  MODULE SUBROUTINE obj_Import( &
    obj, hdf5, group, fedof, fedofs, timefedof, timefedofs, geofedof, &
    geofedofs)
    CLASS(MatrixFieldLis_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    CLASS(FEDOF_), TARGET, OPTIONAL, INTENT(IN) :: fedof, geofedof
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: fedofs(:), geofedofs(:)
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
  MODULE SUBROUTINE obj_Matvec2( &
    obj, x, y, isTranspose, addContribution, scale)
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

!----------------------------------------------------------------------------
!                                                 ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary:  Apply dirichlet boundary condition to matrixfield_

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC1(obj, dbcPtrs)
    CLASS(MatrixFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dbcPtrs(:)
    !! These are column numbers which are local node
  END SUBROUTINE obj_ApplyDirichletBC1
END INTERFACE

!----------------------------------------------------------------------------
!                                            ApplyDirichletBCtoRHS@DBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary:  Apply dirichlet boundary condition to a node field

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBCToRHS( &
    obj, x, y, isTranspose, scale, addContribution)
    CLASS(MatrixFieldLis_), INTENT(INOUT) :: obj
    CLASS(AbstractNodeField_), INTENT(IN) :: x
    CLASS(AbstractNodeField_), INTENT(INOUT) :: y
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_ApplyDirichletBCToRHS
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetDirichletBCSubMat@DBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-14
! summary:  Get submatrix for apply dirichlet boundary condition

INTERFACE
  MODULE SUBROUTINE obj_GetDirichletBCSubMat(obj, submat)
    CLASS(MatrixFieldLis_), INTENT(INOUT) :: obj
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: submat
  END SUBROUTINE obj_GetDirichletBCSubMat
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE MatrixFieldLis_Class
