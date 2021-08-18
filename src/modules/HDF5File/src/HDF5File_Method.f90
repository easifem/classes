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

MODULE HDF5File_Method
USE BaseMethod
USE BaseType
USE HDF5File_Class
IMPLICIT NONE
PRIVATE

PUBLIC :: ExportDOF,  ExportCSRSparsity, ExportCSRMatrix

CONTAINS

!----------------------------------------------------------------------------
!                                                                 ExportDOF
!----------------------------------------------------------------------------

SUBROUTINE ExportDOF( obj, hdf5, group)
  TYPE( DOF_ ), INTENT( IN ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
  ! Internal variable
  TYPE(String) :: dname
  dname = trim(group)//"/storageFMT"
  CALL hdf5%write(dsetname=trim(dname%chars()), &
    & vals=obj%storageFMT )

  IF( ALLOCATED(obj%map) ) THEN
    dname = trim(group)//"/map"
    CALL hdf5%write(dsetname=trim(dname%chars()), &
      & vals=obj%map )
  END IF

  IF( ALLOCATED(obj%valMap) ) THEN
    dname = trim(group)//"/valMap"
    CALL hdf5%write(dsetname=trim(dname%chars()), &
      & vals=obj%valMap )
  END IF

END SUBROUTINE ExportDOF

!----------------------------------------------------------------------------
!                                                         ExportCSRSparsity
!----------------------------------------------------------------------------

SUBROUTINE ExportCSRSparsity( obj, hdf5, group)
  TYPE( CSRSparsity_ ), INTENT( IN ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
  ! Internal variable
  TYPE(String) :: dname

  dname = trim(group)//"/nnz"
  CALL hdf5%write(dsetname=trim(dname%chars()), &
    & vals=obj%nnz )

  dname = trim(group)//"/ncol"
  CALL hdf5%write(dsetname=trim(dname%chars()), &
    & vals=obj%ncol )

  dname = trim(group)//"/nrow"
  CALL hdf5%write(dsetname=trim(dname%chars()), &
    & vals=obj%nrow )

  dname = trim(group)//"/isSorted"
  CALL hdf5%write(dsetname=trim(dname%chars()), &
    & vals=obj%isSorted )

  dname = trim(group)//"/isInitiated"
  CALL hdf5%write(dsetname=trim(dname%chars()), &
    & vals=obj%isInitiated )

  dname = trim(group)//"/isSparsityLock"
  CALL hdf5%write(dsetname=trim(dname%chars()), &
    & vals=obj%isSparsityLock )

  CALL ExportDOF( obj=obj%dof, hdf5=hdf5, group=trim(group)//"/dof" )

  IF( ALLOCATED(obj%IA) ) THEN
    dname = trim(group)//"/IA"
    CALL hdf5%write(dsetname=trim(dname%chars()), &
      & vals=obj%IA )
  END IF

  IF( ALLOCATED(obj%JA) ) THEN
    dname = trim(group)//"/JA"
    CALL hdf5%write(dsetname=trim(dname%chars()), &
      & vals=obj%JA )
  END IF

END SUBROUTINE ExportCSRSparsity

!----------------------------------------------------------------------------
!                                                         ExportCSRMatrix
!----------------------------------------------------------------------------

SUBROUTINE ExportCSRMatrix( obj, hdf5, group)
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
  ! Internal variable
  TYPE(String) :: dname

  dname = trim(group)//"/csrOwnership"
  CALL hdf5%write(dsetname=trim(dname%chars()), &
    & vals=obj%csrOwnership )

  dname = trim(group)//"/tDimension"
  CALL hdf5%write(dsetname=trim(dname%chars()), &
    & vals=obj%tDimension )

  dname = trim(group)//"/matrixProp"
  CALL hdf5%write(dsetname=trim(dname%chars()), &
    & vals=obj%matrixProp )

  IF( ALLOCATED(obj%A) ) THEN
    dname = trim(group)//"/A"
    CALL hdf5%write(dsetname=trim(dname%chars()), &
      & vals=obj%A )
  END IF

  IF( ASSOCIATED(obj%csr) ) THEN
    CALL ExportCSRSparsity(obj=obj%csr, hdf5=hdf5, group=trim(group)//"/csr" )
  END IF

END SUBROUTINE ExportCSRMatrix



END MODULE HDF5File_Method