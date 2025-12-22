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
! date: 15 July 2021
! summary: This module defines [[BlockMatrixField_]] class
!
!# Introduction
!
! - This module defines [[BlockMatrixField_]] class
! - It is designed for handling the block tangent matrix in FEM
!
!@note
! [[BlockMatrixField_]] uses `NATIVE_SERIAL` engine for handling the
! global tangent matrices.
!@endnote

MODULE BlockMatrixField_Class
USE GlobalData, ONLY: I4B, DFP, LGT, DOF_FMT
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

CHARACTER(*), PARAMETER :: modName = "BlockMatrixField_Class"
INTEGER(I4B), PARAMETER :: mystorageformat = DOF_FMT

PUBLIC :: BlockMatrixField_

!----------------------------------------------------------------------------
!                                                          BlockMatrixField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 July 2021
! summary: This is native implementation of finite element tangent matrices.

TYPE, EXTENDS(MatrixField_) :: BlockMatrixField_
CONTAINS
  PRIVATE
  FINAL :: obj_Final
  !! Finalizer
  ! IO:
  !@IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import from hdf5 file
END TYPE BlockMatrixField_

!----------------------------------------------------------------------------
!                                SetBlockMatrixFieldParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2021
! summary: This routine Sets the parameter for creating [[BlockMatrixField_]]
!
!# Introduction
!
! - This routine Sets the essential parameter for initiating
! an instance of [[BlockMatrixField_]]
! - The results will be returned in `param`
! - After Getting `param` user can call [[BlockMatrixField_:Initiate]] method
!
!@note
! The size of `physicalVarNames`, `spaceCompo`, and `timeCompo` should be
! the same.
!@endnote

! INTERFACE
!   MODULE SUBROUTINE SetBlockMatrixFieldParam(param, name, matrixProp, &
!                  physicalVarNames, spaceCompo, timeCompo, engine, fieldType, &
!                                              comm, local_n, global_n)
!     TYPE(ParameterList_), INTENT(INOUT) :: param
!     !! Options to create [[BlockMatrixField_]] will be stored in this
!     CHARACTER(*), INTENT(IN) :: name
!     !! Name of the matrix field
!     CHARACTER(*), INTENT(IN) :: matrixProp
!     !! Matrix property, "SYM" or "UNSYM"
!     CHARACTER(*), INTENT(IN) :: physicalVarNames(:)
!     !! Name of physical variables
!     INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
!     !! Number of space-components in each physicalVarNames, see [[DOF_]]
!     INTEGER(I4B), INTENT(IN) :: timeCompo(:)
!     !! Number of time-components in each physicalVarNames, see [[DOF_]]
!     CHARACTER(*), INTENT(IN) :: engine
!     !! engine
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: fieldType
!     !! fieldType can be following
!     !! FIELD_TYPE_NORMAL <-- DEFAULT
!     !! FIELD_TYPE_CONSTANT
!     !! FIELD_TYPE_CONSTANT_SPACE
!     !! FIELD_TYPE_CONSTANT_TIME
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: comm
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_n
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: global_n
!   END SUBROUTINE SetBlockMatrixFieldParam
! END INTERFACE

!----------------------------------------------------------------------------
!                       SetBlockMatrixFieldPrecondParam@sConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2021
! summary: This routine Sets the parameter for precondition of BlockMatrixField_

! INTERFACE
!   MODULE SUBROUTINE SetBlockMatrixFieldPrecondParam(param, name, engine, &
!                 lfil, mbloc, droptol, permtol, alpha, comm, local_n, global_n)
!     TYPE(ParameterList_), INTENT(INOUT) :: param
!     !! Options to create precondition of [[BlockMatrixField_]]
!     INTEGER(I4B), INTENT(IN) :: name
!     !! Name of precondition
!     CHARACTER(*), INTENT(IN) :: engine
!     !! engine
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: lfil
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: mbloc
!     REAL(DFP), OPTIONAL, INTENT(IN) :: droptol
!     !! Droptoleranace
!     REAL(DFP), OPTIONAL, INTENT(IN) :: permtol
!     !! permutation tolerance
!     REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: comm
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_n
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: global_n
!   END SUBROUTINE SetBlockMatrixFieldPrecondParam
! END INTERFACE

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(BlockMatrixField_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Imports the content of matrix field from hdf5file

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, fedof, fedofs, timefedof, &
                               timefedofs, geofedof, geofedofs)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    CLASS(FEDOF_), TARGET, OPTIONAL, INTENT(IN) :: fedof, geofedof
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: fedofs(:), geofedofs(:)
    CLASS(TimeFEDOF_), TARGET, OPTIONAL, INTENT(IN) :: timefedof
    TYPE(TimeFEDOFPointer_), OPTIONAL, INTENT(IN) :: timefedofs(:)
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE BlockMatrixField_Class
