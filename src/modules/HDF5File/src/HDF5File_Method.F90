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

CONTAINS

!----------------------------------------------------------------------------
!                                                                 ExportDOF
!----------------------------------------------------------------------------

SUBROUTINE ExportDOF( obj, hdf5, group )
  TYPE( DOF_ ), INTENT( IN ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
  ! Internal variable
  TYPE(String) :: dsetname
  dsetname = trim(group)//"/storageFMT"
  CALL hdf5%write(dsetname=trim(dsetname%chars()), &
    & vals=obj%storageFMT )
  !>
  IF( ALLOCATED(obj%map) ) THEN
    dsetname = trim(group)//"/map"
    CALL hdf5%write(dsetname=trim(dsetname%chars()), &
      & vals=obj%map )
  END IF
  !>
  IF( ALLOCATED(obj%valMap) ) THEN
    dsetname = trim(group)//"/valMap"
    CALL hdf5%write(dsetname=trim(dsetname%chars()), &
      & vals=obj%valMap )
  END IF
END SUBROUTINE ExportDOF

!----------------------------------------------------------------------------
!                                                                 ImportDOF
!----------------------------------------------------------------------------

SUBROUTINE ImportDOF( obj, hdf5, group )
  TYPE( DOF_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
  ! Internal variable
  TYPE(String) :: dsetname
  dsetname = trim(group)//"/storageFMT"
  CALL hdf5%read(dsetname=trim(dsetname%chars()), vals=obj%storageFMT )
  !> Map
  dsetname = trim(group)//"/map"
  IF( hdf5%pathExists(trim(dsetname%chars()))) THEN
    CALL hdf5%read(dsetname=trim(dsetname%chars()), vals=obj%map )
  END IF
  !> valmap
  dsetname = trim(group)//"/valMap"
  IF( hdf5%pathExists(trim(dsetname%chars()))) THEN
    CALL hdf5%read(dsetname=trim(dsetname%chars()), vals=obj%valMap )
  END IF
END SUBROUTINE ImportDOF

!----------------------------------------------------------------------------
!                                                         ExportCSRSparsity
!----------------------------------------------------------------------------

SUBROUTINE ExportCSRSparsity( obj, hdf5, group)
  TYPE( CSRSparsity_ ), INTENT( IN ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
  ! Internal variable
  TYPE(String) :: dsetname

  dsetname = trim(group)//"/nnz"
  CALL hdf5%write(dsetname=trim(dsetname%chars()), &
    & vals=obj%nnz )
  !>
  dsetname = trim(group)//"/ncol"
  CALL hdf5%write(dsetname=trim(dsetname%chars()), &
    & vals=obj%ncol )
  !>
  dsetname = trim(group)//"/nrow"
  CALL hdf5%write(dsetname=trim(dsetname%chars()), &
    & vals=obj%nrow )
  !>
  dsetname = trim(group)//"/isSorted"
  CALL hdf5%write(dsetname=trim(dsetname%chars()), &
    & vals=obj%isSorted )
  !>
  dsetname = trim(group)//"/isInitiated"
  CALL hdf5%write(dsetname=trim(dsetname%chars()), &
    & vals=obj%isInitiated )
  !>
  dsetname = trim(group)//"/isSparsityLock"
  CALL hdf5%write(dsetname=trim(dsetname%chars()), &
    & vals=obj%isSparsityLock )
  !>
  CALL ExportDOF( obj=obj%dof, hdf5=hdf5, group=trim(group)//"/dof" )
  !>
  IF( ALLOCATED(obj%IA) ) THEN
    dsetname = trim(group)//"/IA"
    CALL hdf5%write(dsetname=trim(dsetname%chars()), &
      & vals=obj%IA )
  END IF
  !>
  IF( ALLOCATED(obj%JA) ) THEN
    dsetname = trim(group)//"/JA"
    CALL hdf5%write(dsetname=trim(dsetname%chars()), &
      & vals=obj%JA )
  END IF
END SUBROUTINE ExportCSRSparsity

!----------------------------------------------------------------------------
!                                                         ImportCSRSparsity
!----------------------------------------------------------------------------

SUBROUTINE ImportCSRSparsity( obj, hdf5, group)
  TYPE( CSRSparsity_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
  ! Internal variable
  TYPE(String) :: dsetname
  !> nnzz
  dsetname = trim(group)//"/nnz"
  CALL hdf5%read(dsetname=trim(dsetname%chars()), vals=obj%nnz )
  !> ncol
  dsetname = trim(group)//"/ncol"
  CALL hdf5%read(dsetname=trim(dsetname%chars()), vals=obj%ncol )
  !> nrow
  dsetname = trim(group)//"/nrow"
  CALL hdf5%read(dsetname=trim(dsetname%chars()), vals=obj%nrow )
  !> isSorted
  dsetname = trim(group)//"/isSorted"
  CALL hdf5%read(dsetname=trim(dsetname%chars()), vals=obj%isSorted )
  !> isInitiated
  dsetname = trim(group)//"/isInitiated"
  CALL hdf5%read(dsetname=trim(dsetname%chars()), vals=obj%isInitiated )
  !> isSparsityLock
  dsetname = trim(group)//"/isSparsityLock"
  CALL hdf5%read(dsetname=trim(dsetname%chars()), vals=obj%isSparsityLock )
  !> dof
  CALL ImportDOF( obj=obj%dof, hdf5=hdf5, group=trim(group)//"/dof" )
  !> IA
  dsetname = trim(group)//"/IA"
  IF( hdf5%pathExists(trim(dsetname%chars()))) THEN
    CALL hdf5%read(dsetname=trim(dsetname%chars()), vals=obj%IA )
  END IF
  !> JA
  dsetname = trim(group)//"/JA"
  IF( hdf5%pathExists(trim(dsetname%chars()))) THEN
    CALL hdf5%read(dsetname=trim(dsetname%chars()), vals=obj%JA )
  END IF
END SUBROUTINE ImportCSRSparsity

!----------------------------------------------------------------------------
!                                                         ExportCSRMatrix
!----------------------------------------------------------------------------

SUBROUTINE ExportCSRMatrix( obj, hdf5, group)
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
  ! Internal variable
  TYPE(String) :: dsetname
  !>
  dsetname = trim(group)//"/csrOwnership"
  CALL hdf5%write(dsetname=trim(dsetname%chars()), &
    & vals=obj%csrOwnership )
  !>
  dsetname = trim(group)//"/tDimension"
  CALL hdf5%write(dsetname=trim(dsetname%chars()), &
    & vals=obj%tDimension )
  !>
  dsetname = trim(group)//"/matrixProp"
  CALL hdf5%write(dsetname=trim(dsetname%chars()), &
    & vals=String(obj%matrixProp) )
  !>
  IF( ALLOCATED(obj%A) ) THEN
    dsetname = trim(group)//"/A"
    CALL hdf5%write(dsetname=trim(dsetname%chars()), &
      & vals=obj%A )
  END IF
  !>
  IF( ASSOCIATED(obj%csr) ) THEN
    CALL ExportCSRSparsity(obj=obj%csr, hdf5=hdf5, group=trim(group)//"/csr" )
  END IF
END SUBROUTINE ExportCSRMatrix

!----------------------------------------------------------------------------
!                                                            ImportCSRMatrix
!----------------------------------------------------------------------------

SUBROUTINE ImportCSRMatrix( obj, hdf5, group)
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
  ! Internal variable
  TYPE(String) :: dsetname, strval
  !> main
  !> csrOwnership
  dsetname = trim(group)//"/csrOwnership"
  CALL hdf5%read(dsetname=trim(dsetname%chars()), vals=obj%csrOwnership )
  !> tDimension
  dsetname = trim(group)//"/tDimension"
  CALL hdf5%read(dsetname=trim(dsetname%chars()), vals=obj%tDimension )
  !> matrixProp
  dsetname = trim(group)//"/matrixProp"
  CALL hdf5%read(dsetname=trim(dsetname%chars()), vals=strval )
  obj%matrixProp = strval%chars()
  !>
  dsetname = trim(group)//"/A"
  IF( hdf5%pathExists(trim(dsetname%chars()))) THEN
    CALL hdf5%read(dsetname=trim(dsetname%chars()), vals=obj%A )
  END IF
  !>
  dsetname = trim(group)//"/csr"
  IF( hdf5%pathExists(trim(dsetname%chars()))) THEN
    ALLOCATE( obj%csr )
    CALL ImportCSRSparsity(obj=obj%csr, hdf5=hdf5, &
      & group=trim(dsetname%chars()) )
  END IF
END SUBROUTINE ImportCSRMatrix

!----------------------------------------------------------------------------
!                                                           ExportRealVector
!----------------------------------------------------------------------------

SUBROUTINE ExportRealVector( obj, hdf5, group )
  TYPE( RealVector_ ), INTENT( IN ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
  !> internal variables
  TYPE( String ) :: dsetname
  !> tDimension
  dsetname = trim(group)//"/tDimension"
  CALL hdf5%write( dsetname=trim(dsetname%chars()), vals=obj%tDimension )
  !> Val
  dsetname = trim(group)//"/Val"
  CALL hdf5%write( dsetname=trim(dsetname%chars()), vals=obj%Val )
END SUBROUTINE ExportRealVector

!----------------------------------------------------------------------------
!                                                           ImportRealVector
!----------------------------------------------------------------------------

SUBROUTINE ImportRealVector( obj, hdf5, group )
  TYPE( RealVector_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
  !> internal variables
  TYPE( String ) :: dsetname
  !> tDimension
  dsetname = trim(group)//"/tDimension"
  CALL hdf5%read( dsetname=trim(dsetname%chars()), vals=obj%tDimension )
  !> Val
  dsetname = trim(group)//"/Val"
  CALL hdf5%read( dsetname=trim(dsetname%chars()), vals=obj%Val )
END SUBROUTINE ImportRealVector

!----------------------------------------------------------------------------
!                                                           ExportIntVector
!----------------------------------------------------------------------------

SUBROUTINE ExportIntVector( obj, hdf5, group )
  TYPE( IntVector_ ), INTENT( IN ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
  !> internal variables
  TYPE( String ) :: dsetname
  !> tDimension
  dsetname = trim(group)//"/tDimension"
  CALL hdf5%write( dsetname=trim(dsetname%chars()), vals=obj%tDimension )
  !> Val
  dsetname = trim(group)//"/Val"
  CALL hdf5%write( dsetname=trim(dsetname%chars()), vals=obj%Val )
END SUBROUTINE ExportIntVector

!----------------------------------------------------------------------------
!                                                           ImportIntVector
!----------------------------------------------------------------------------

SUBROUTINE ImportIntVector( obj, hdf5, group )
  TYPE( IntVector_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
  !> internal variables
  TYPE( String ) :: dsetname
  !> tDimension
  dsetname = trim(group)//"/tDimension"
  CALL hdf5%read( dsetname=trim(dsetname%chars()), vals=obj%tDimension )
  !> Val
  dsetname = trim(group)//"/Val"
  CALL hdf5%read( dsetname=trim(dsetname%chars()), vals=obj%Val )
END SUBROUTINE ImportIntVector

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE HDF5File_Method