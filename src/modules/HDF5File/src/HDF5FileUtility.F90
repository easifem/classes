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
!

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2021
! summary: Some additional methods for HDF5File

MODULE HDF5FileUtility
USE GlobalData, ONLY: DFP, I4B, LGT
USE BaseType, ONLY: DOF_
USE BaseType, ONLY: CSRSparsity_
USE BaseType, ONLY: CSRMatrix_
USE BaseType, ONLY: RealVector_
USE BaseType, ONLY: IntVector_
USE String_Class, ONLY: String
USE HDF5File_Class, ONLY: HDF5File_
IMPLICIT NONE
PRIVATE

PUBLIC :: ExportDOF
PUBLIC :: ImportDOF
PUBLIC :: ExportCSRSparsity
PUBLIC :: ImportCSRSparsity
PUBLIC :: ExportCSRMatrix
PUBLIC :: ImportCSRMatrix
PUBLIC :: ExportRealVector
PUBLIC :: ImportRealVector
PUBLIC :: ExportIntVector
PUBLIC :: ImportIntVector
PUBLIC :: HDF5ReadScalar
PUBLIC :: HDF5ReadVector
PUBLIC :: HDF5ReadMatrix
PUBLIC :: HDF5GetEntities

!----------------------------------------------------------------------------
!                                                            HDF5GetEntities
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE HDF5GetEntities( &
    hdf5, group, dim, tEntities, myName, modName)
    CLASS(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), INTENT(OUT) :: tEntities
    CHARACTER(*), INTENT(IN) :: myName
    CHARACTER(*), INTENT(IN) :: modName
  END SUBROUTINE HDF5GetEntities
END INTERFACE

!----------------------------------------------------------------------------
!                                                          HDF5ReadIntMatrix
!----------------------------------------------------------------------------

INTERFACE HDF5ReadMatrix
  MODULE SUBROUTINE HDF5ReadIntMatrix( &
    hdf5, VALUE, group, fieldname, myname, modname, check)
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
    CHARACTER(*), INTENT(IN) :: group
    CHARACTER(*), INTENT(IN) :: fieldname
    CHARACTER(*), INTENT(IN) :: myname
    CHARACTER(*), INTENT(IN) :: modname
    LOGICAL(LGT), INTENT(IN) :: check
  END SUBROUTINE HDF5ReadIntMatrix
END INTERFACE HDF5ReadMatrix

!----------------------------------------------------------------------------
!                                                         HDF5ReadRealMatrix
!----------------------------------------------------------------------------

INTERFACE HDF5ReadMatrix
  MODULE SUBROUTINE HDF5ReadRealMatrix( &
    hdf5, VALUE, group, fieldname, myname, modname, check)
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
    CHARACTER(*), INTENT(IN) :: group
    CHARACTER(*), INTENT(IN) :: fieldname
    CHARACTER(*), INTENT(IN) :: myname
    CHARACTER(*), INTENT(IN) :: modname
    LOGICAL(LGT), INTENT(IN) :: check
  END SUBROUTINE HDF5ReadRealMatrix
END INTERFACE HDF5ReadMatrix

!----------------------------------------------------------------------------
!                                                        HDF5ReadRealVector
!----------------------------------------------------------------------------

INTERFACE HDF5ReadVector
  MODULE SUBROUTINE HDF5ReadRealVector( &
    hdf5, VALUE, group, fieldname, myname, modname, check)
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    CHARACTER(*), INTENT(IN) :: group
    CHARACTER(*), INTENT(IN) :: fieldname
    CHARACTER(*), INTENT(IN) :: myname
    CHARACTER(*), INTENT(IN) :: modname
    LOGICAL(LGT), INTENT(IN) :: check
  END SUBROUTINE HDF5ReadRealVector
END INTERFACE HDF5ReadVector

!----------------------------------------------------------------------------
!                                                         HDF5ReadIntVector
!----------------------------------------------------------------------------

INTERFACE HDF5ReadVector
  MODULE SUBROUTINE HDF5ReadIntVector( &
    hdf5, VALUE, group, fieldname, myname, modname, check)
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    CHARACTER(*), INTENT(IN) :: group
    CHARACTER(*), INTENT(IN) :: fieldname
    CHARACTER(*), INTENT(IN) :: myname
    CHARACTER(*), INTENT(IN) :: modname
    LOGICAL(LGT), INTENT(IN) :: check
  END SUBROUTINE HDF5ReadIntVector
END INTERFACE HDF5ReadVector

!----------------------------------------------------------------------------
!                                                            HDF5ReadScalar
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE HDF5ReadScalar( &
    hdf5, VALUE, group, fieldname, myname, modname, check)
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CLASS(*), INTENT(INOUT) :: VALUE
    CHARACTER(*), INTENT(IN) :: group
    CHARACTER(*), INTENT(IN) :: fieldname
    CHARACTER(*), INTENT(IN) :: myname
    CHARACTER(*), INTENT(IN) :: modname
    LOGICAL(LGT), INTENT(IN) :: check
  END SUBROUTINE HDF5ReadScalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 ExportDOF
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE ExportDOF(obj, hdf5, group)
    TYPE(DOF_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE ExportDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 ImportDOF
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE ImportDOF(obj, hdf5, group)
    TYPE(DOF_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE ImportDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                         ExportCSRSparsity
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE ExportCSRSparsity(obj, hdf5, group)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE ExportCSRSparsity
END INTERFACE

!----------------------------------------------------------------------------
!                                                         ImportCSRSparsity
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE ImportCSRSparsity(obj, hdf5, group)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE ImportCSRSparsity
END INTERFACE

!----------------------------------------------------------------------------
!                                                         ExportCSRMatrix
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE ExportCSRMatrix(obj, hdf5, group)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE ExportCSRMatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                                            ImportCSRMatrix
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE ImportCSRMatrix(obj, hdf5, group)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE ImportCSRMatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                                           ExportRealVector
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE ExportRealVector(obj, hdf5, group)
    TYPE(RealVector_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE ExportRealVector
END INTERFACE

!----------------------------------------------------------------------------
!                                                           ImportRealVector
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE ImportRealVector(obj, hdf5, group)
    TYPE(RealVector_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE ImportRealVector
END INTERFACE

!----------------------------------------------------------------------------
!                                                           ExportIntVector
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE ExportIntVector(obj, hdf5, group)
    TYPE(IntVector_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE ExportIntVector
END INTERFACE

!----------------------------------------------------------------------------
!                                                           ImportIntVector
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE ImportIntVector(obj, hdf5, group)
    TYPE(IntVector_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE ImportIntVector
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE HDF5FileUtility
