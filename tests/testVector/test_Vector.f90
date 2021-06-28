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
implicit none
contains

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test0
  type( vector_ ) :: obj
  type( ParameterList_ ) :: param
  INTEGER( I4B ) :: ierr
  type( RealVector_ ) :: realVec

  call dashline()
  call display( "TESTING SET METHODS" )
  call display( "Single physical variable" )
  call display( "Physical name : U" )
  call display( "tNodes : 10" )
  call display( "spaceCompo : [3]" )
  call display( "timeCompo : [1]" )
  call display( "storageFMT : FMT_NODES" )
  call dashline()

  CALl FPL_Init()
  call param%init()
  ierr = param%set( key="names", value="U" )
  ierr = param%set( key="tNodes", value=[5] )
  ierr = param%set( key="spaceCompo", value=[3] )
  ierr = param%set( key="timeCompo", value=[1] )
  ierr = param%set( key="storageFMT", value=FMT_NODES )
  call obj%initiate( param )

  realVec = [1, 2, 3]
  call obj%set( nodeNum=[3], value=realVec%val, storageFMT=FMT_DOF )
  call obj%display(msg="obj : ")
  read( *, * ) ierr

  realVec = [100, 200]
  call obj%set( value=0.0_DFP )
  call obj%set( nodeNum=[2,3], value=realVec%val, storageFMT=FMT_DOF, &
    & dofs = [2] )
  call obj%display(msg="obj : ")
  read( *, * ) ierr

  realVec = [1,2,3]
  call obj%set( value=0.0_DFP )
  call obj%set( nodeNum=[3], value=realVec%val, storageFMT=FMT_NODES )
  call obj%display(msg="obj : ")
  read( *, * ) ierr

  realVec = [2,2,2,3,3,3]
  call obj%set( value=0.0_DFP )
  call obj%set( nodeNum=[2,3], value=realVec%val, storageFMT=FMT_NODES )
  call obj%display(msg="obj : ")
  read( *, * ) ierr

  call param%deallocateData()
  CALL FPL_FINALIZE()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test5
  type( vector_ ) :: obj
  type( ParameterList_ ) :: param
  INTEGER( I4B ) :: ierr
  type( RealVector_ ) :: realVec

  call dashline()
  call display( "TESTING SET METHODS" )
  call display( "Single physical variable" )
  call display( "Physical name : U" )
  call display( "tNodes : 10" )
  call display( "spaceCompo : [3]" )
  call display( "timeCompo : [1]" )
  call display( "storageFMT : FMT_DOF" )
  call dashline()

  CALl FPL_Init()
  call param%init()
  ierr = param%set( key="names", value="U" )
  ierr = param%set( key="tNodes", value=[5] )
  ierr = param%set( key="spaceCompo", value=[3] )
  ierr = param%set( key="timeCompo", value=[1] )
  ierr = param%set( key="storageFMT", value=FMT_DOF )
  call obj%initiate( param )

  realVec = [1, 2, 3]
  call obj%set( nodeNum=[3], value=realVec%val, storageFMT=FMT_DOF )
  call obj%display(msg="obj : ")
  read( *, * ) ierr

  realVec = [100, 200]
  call obj%set( value=0.0_DFP )
  call obj%set( nodeNum=[2,3], value=realVec%val, storageFMT=FMT_DOF, &
    & dofs = [2] )
  call obj%display(msg="obj : ")
  read( *, * ) ierr

  realVec = [1,2,3]
  call obj%set( value=0.0_DFP )
  call obj%set( nodeNum=[3], value=realVec%val, storageFMT=FMT_NODES )
  call obj%display(msg="obj : ")
  read( *, * ) ierr

  realVec = [2,2,2,3,3,3]
  call obj%set( value=0.0_DFP )
  call obj%set( nodeNum=[2,3], value=realVec%val, storageFMT=FMT_NODES )
  call obj%display(msg="obj : ")
  read( *, * ) ierr

  call param%deallocateData()
  CALL FPL_FINALIZE()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test4
  type( vector_ ) :: obj
  type( ParameterList_ ) :: param
  INTEGER( I4B ) :: ierr

  call dashline()
  call display( "TESTING CONSTRUCTOR METHODS" )
  call display( "Single physical variable" )
  call display( "Physical name : U" )
  call display( "tNodes : 10" )
  call display( "spaceCompo : [3]" )
  call display( "timeCompo : [1]" )
  call display( "storageFMT : FMT_DOF" )
  call dashline()

  CALl FPL_Init()
  call param%init()
  ierr = param%set( key="names", value="U" )
  ierr = param%set( key="tNodes", value=[10] )
  ierr = param%set( key="spaceCompo", value=[3] )
  ierr = param%set( key="timeCompo", value=[1] )
  ierr = param%set( key="storageFMT", value=FMT_DOF )
  call obj%initiate( param )
  call obj%display(msg="obj : ")
  call param%deallocateData()
  CALL FPL_FINALIZE()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test3
  type( vector_ ) :: obj
  type( ParameterList_ ) :: param
  INTEGER( I4B ) :: ierr

  call dashline()
  call display( "TESTING CONSTRUCTOR METHODS" )
  call display( "Single physical variable" )
  call display( "Physical name : U" )
  call display( "tNodes : 10" )
  call display( "spaceCompo : [3]" )
  call display( "timeCompo : [1]" )
  call display( "storageFMT : FMT_DOF" )
  call dashline()

  CALl FPL_Init()
  call param%init()
  ierr = param%set( key="names", value="U" )
  ierr = param%set( key="tNodes", value=[10] )
  ierr = param%set( key="spaceCompo", value=[3] )
  ierr = param%set( key="timeCompo", value=[1] )
  ierr = param%set( key="storageFMT", value=FMT_DOF )
  call obj%initiate( param )
  call obj%display(msg="obj : ")
  call param%deallocateData()
  CALL FPL_FINALIZE()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test2
  type( vector_ ) :: obj
  type( ParameterList_ ) :: param
  INTEGER( I4B ) :: ierr

  call dashline()
  call display( "TESTING CONSTRUCTOR METHODS" )
  call display( "Single physical variable" )
  call display( "Physical name : U" )
  call display( "tNodes : 10" )
  call display( "spaceCompo : [3]" )
  call display( "timeCompo : [1]" )
  call display( "storageFMT : FMT_NODES" )
  call dashline()

  CALl FPL_Init()
  call param%init()
  ierr = param%set( key="names", value="U" )
  ierr = param%set( key="tNodes", value=[10] )
  ierr = param%set( key="spaceCompo", value=[3] )
  ierr = param%set( key="timeCompo", value=[1] )
  ierr = param%set( key="storageFMT", value=FMT_NODES )
  call obj%initiate( param )
  call obj%display(msg="obj : ")
  call param%deallocateData()
  CALL FPL_FINALIZE()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
  type( vector_ ) :: obj
  type( ParameterList_ ) :: param
  INTEGER( I4B ) :: ierr

  call display( "testing constructor methods" )
  CALl FPL_Init()
  call param%init()
  ierr = param%set( key="names", value="U, V, W" )
  ierr = param%set( key="tNodes", value=[1, 1, 1] )
  call obj%initiate( param )
  call obj%display(msg="obj : ")
  call param%deallocateData()
  CALL FPL_FINALIZE()
end subroutine


end module test_m

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_m
implicit none
call test0
end program main