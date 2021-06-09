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

module test_m
use easifemBase
use easifemClasses
USE HDF5
implicit none
contains

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test0
  type( hdf5File_ ) :: obj
  character( len = * ), parameter ::  filename = "./test.h5"
  TYPE( String ) :: val, names( 2 )
  TYPE( String ), ALLOCATABLE :: names_( : )

  CALL obj%initiate( filename, mode="NEW" )
  CALL obj%open()
  val = "name"
  CALL obj%write(dsetname="/name", vals=val )
  CALL obj%WriteAttribute("/name", "name_attribute", "string attr")
  names( 1 ) = "hello"
  names( 2 ) = "world"
  CALL obj%write( dsetname="/names", vals=names )
  CALL obj%close()
  CALL obj%DeallocateData()
  CALL obj%initiate( filename, mode="READ" )
  CALL obj%open( )
  CALL obj%read(dsetname="/name", vals=val)
  CALL Display( val, "val = ")
  CALL obj%read(dsetname="/names", vals=names_)
  CALL Display( names_(1), "val(1) = ")
  CALL Display( names_(2), "val(2) = ")
  CALL obj%close()
  CALL obj%DeallocateData()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test9
  type( hdf5File_ ) :: obj
  character( len = * ), parameter ::  filename = "./test.h5"
  real( Real64 ) :: time, dummyVec( 100 ), dummyMat( 10, 10 )
  real( Real64 ), ALLOCATABLE :: v1( : ), k1( :, : )
  CALL obj%initiate( filename, mode="NEW" )
  CALL obj%open()
  time = 2.0
  CALL obj%write(dsetname="/time", vals=time )
  call RANDOM_NUMBER( dummyVec )
  CALL obj%write(dsetname="/rank_1/v1", vals=dummyVec )
  CALL RANDOM_NUMBER( dummyMat )
  CALL obj%write(dsetname="/rank_2/k1", vals=dummyMat)
  CALL obj%close()
  CALL obj%DeallocateData()

  CALL obj%initiate( filename, mode="READ" )
  CALL obj%open( )
  CALL obj%read(dsetname="/time", vals=time)
  CALL obj%read(dsetname="/rank_1/v1", vals=v1)
  CALL obj%read(dsetname="/rank_2/k1", vals=k1)
  CALL obj%close()
  CALL obj%DeallocateData()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test8
  type( hdf5File_ ) :: obj
  character( len = * ), parameter ::  filename = "./test.h5"
  real( Real32 ) :: val( 10, 10 )

  CALL obj%initiate( filename, mode="NEW" )
  CALL obj%open()

  CALL RANDOM_NUMBER( val )
  CALL obj%write(dsetname="data_in_root", vals=val )
  CALL RANDOM_NUMBER( val )
  CALL obj%write(dsetname="/grp1/data_in_g1", vals=val )
  CALL RANDOM_NUMBER( val )
  CALL obj%write(dsetname="/grp2/data_in_g2", vals=val )
  CALL obj%close()
  CALL obj%DeallocateData()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test7
  type( hdf5File_ ) :: obj
  character( len = * ), parameter ::  filename = "./test.h5"
  real( dfp ) :: val( 10, 10 )

  CALL obj%initiate( filename, mode="NEW" )
  CALL obj%open()

  CALL RANDOM_NUMBER( val )
  CALL obj%write(dsetname="data_in_root", vals=val )
  CALL RANDOM_NUMBER( val )
  CALL obj%write(dsetname="/grp1/data_in_g1", vals=val )
  CALL RANDOM_NUMBER( val )
  CALL obj%write(dsetname="/grp2/data_in_g2", vals=val )
  CALL obj%close()
  CALL obj%DeallocateData()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test6
  type( hdf5File_ ) :: obj
  character( len = * ), parameter ::  filename = "./test.h5"
  real( dfp ) :: val( 10 )

  CALL obj%initiate( filename, mode="NEW" )
  CALL obj%open()

  CALL RANDOM_NUMBER( val )
  CALL obj%write(dsetname="data_in_root", vals=val )
  CALL RANDOM_NUMBER( val )
  CALL obj%write(dsetname="/grp1/data_in_g1", vals=val )
  CALL RANDOM_NUMBER( val )
  CALL obj%write(dsetname="/grp2/data_in_g2", vals=val )
  CALL obj%close()
  CALL obj%DeallocateData()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test5
  type( hdf5File_ ) :: obj
  character( len = * ), parameter ::  filename = "./test.h5"
  CALL obj%initiate( filename, mode="NEW" )
  CALL obj%open()

  CALL obj%write(dsetname="data_in_root", vals=1.0_DFP )
  CALL obj%mkdir("/grp1")
  CALL obj%write(dsetname="/grp1/data_in_g1", vals=2.0_DFP )
  CALL obj%write(dsetname="/grp2/data_in_g2", vals=3.0_DFP )

  CALL obj%close()
  CALL obj%DeallocateData()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test4
  type( hdf5File_ ) :: obj
  character( len = * ), parameter ::  filename = "./test.h5"
  CALL obj%initiate( filename, mode="NEW" )
  CALL obj%open()
  CALL obj%mkdir("/grp1")
  CALL Display( obj%pathExists("/grp1"), "T=")
  CALL Display( obj%ngrp("/"), "1=" )

  CALL obj%mkdir("/grp2")
  CALL Display( obj%pathExists("/grp2"), "T=")
  CALL Display( obj%ngrp("/"), "2=" )

  CALL obj%mkdir("/grp3")
  CALL Display( obj%pathExists("/grp3"), "T=")
  CALL Display( obj%ngrp("/"), "3=" )

  CALL obj%mkdir("/grp3/grp3a")
  CALL Display( obj%pathExists("/grp3/grp3a"), "T=")
  CALL Display( obj%ngrp("/"), "3=" )
  CALL Display( obj%ngrp("/grp3"), "1=" )

  CALL obj%mkalldir("/grp4/grp4a")
  CALL obj%mkalldir("/grp4/grp4a/grp4a1")
  CALL Display( obj%ngrp("/"), "4=" )
  CALL Display( obj%ngrp("/grp4"), "1=" )
  CALL Display( obj%ngrp("/grp4/grp4a"), "1=" )

  CALL obj%close()
  CALL obj%DeallocateData()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test3
  type( hdf5File_ ) :: obj
  character( len = * ), parameter ::  filename = "./test.h5"
  CALL obj%initiate( filename, mode="READ" )
  CALL obj%open()
  CALL obj%close()
  CALL obj%DeallocateData()
  CALL obj%initiate( filename, mode="WRITE" )
  CALL obj%open()
  CALL obj%close()
  CALL obj%DeallocateData()
end subroutine test3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test2
  type( hdf5File_ ) :: obj
  character( len = * ), parameter ::  filename = "./test.h5"

  CALL obj%initiate( filename, mode="OVERWRITE" )
  CALL obj%open()
  CALL obj%close()
  CALL obj%DeallocateData()
end subroutine test2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
  type( hdf5File_ ) :: obj
  character( len = * ), parameter ::  filename = "./test.h5"

  CALL obj%initiate( filename, mode="NEW" )
  CALL obj%open()
  CALL obj%close()
  CALL obj%DeallocateData()
end subroutine test1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------


end module


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_m
call test0
end program