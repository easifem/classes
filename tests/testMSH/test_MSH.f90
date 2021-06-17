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
use mshFormat_Class
use mshPhysicalNames_Class
use mshEntity_Class
implicit none
contains

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE test0
  TYPE( MSH_ ) :: mshFile
  TYPE( string ) :: line
  INTEGER( I4B ) :: ii, error

  CALL mshFile%initiate( file="./mesh.msh", NSD=2 )
  CALL mshFile%ExportMesh( file="./mesh.h5" )
  CALL mshFile%DeallocateData()
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE test3
  TYPE( TxtFile_ ) :: mshFile
  TYPE( mshEntity_ ) :: pointEntity
  TYPE( string ) :: line
  INTEGER( I4B ) :: ii, error

  CALL mshFile%initiate(file="./mesh.msh", status='OLD', &
    & action='READ')
  CALL mshFile%open()
  CALL pointEntity%read(mshFile=mshFile, dim=0, readTag=.TRUE., error=error)
  CALL pointEntity%Display(msg="point entity")
  CALL mshFile%close()
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE test2
  TYPE( TxtFile_ ) :: mshFile
  TYPE( mshPhysicalNames_ ) :: mshPhysicalNames
  TYPE( string ) :: line
  TYPE( string ), ALLOCATABLE :: physicalNames( : )
  INTEGER( I4B ) :: ii, error

  CALL mshFile%initiate(file="./mesh.msh", status='OLD', &
    & action='READ')
  CALL mshFile%open()
  CALL mshPhysicalNames%read(mshFile=mshFile, error=error)
  CALL mshPhysicalNames%write(mshFile=mshFile, startStr="$PhysicalNames", endstr="$EndPhysicalNames")
  CALL Display(mshPhysicalNames%getTotalPhysicalEntities(), "6=" )
  CALL Display(mshPhysicalNames%getTotalPhysicalEntities([0]), "0=" )
  CALL Display(mshPhysicalNames%getTotalPhysicalEntities([1]), "5=" )
  CALL Display(mshPhysicalNames%getTotalPhysicalEntities([2]), "1=" )
  CALL Display(mshPhysicalNames%getTotalPhysicalEntities([3]), "0=" )
  CALL Display(mshPhysicalNames%getTotalPhysicalEntities([0,1,2]), "6=" )
  CALL Display(mshPhysicalNames%getTotalPhysicalEntities([1,2]), "6=" )
  CALL Display(mshPhysicalNames%getTotalPhysicalEntities([1,3]), "5=" )
  CALL Display(mshPhysicalNames%getTotalPhysicalEntities([2,3]), "1=" )
  CALL EqualLine()
  CALL Display( mshPhysicalNames%getIndex(String("right")), "2=")
  CALL Display( mshPhysicalNames%getIndex( [String("right"), String("left") ]), "2,4=")
  CALL Display( mshPhysicalNames%getIndex( dim=1, tag=1 ), "1=" )
  CALL Display( mshPhysicalNames%getIndex( dim=1 ), "1,2,3,4,5=" )

  PhysicalNames = mshPhysicalNames%getPhysicalNames()
  DO ii = 1, SIZE( PhysicalNames )
    CALL Display( PhysicalNames( ii ), "physicalnames = ")
  END DO
  PhysicalNames = mshPhysicalNames%getPhysicalNames(dim=1)
  DO ii = 1, SIZE( PhysicalNames )
    CALL Display( PhysicalNames( ii ), "physicalnames = ")
  END DO
  CALL mshFile%close()
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE test1
  TYPE( TxtFile_ ) :: mshFile
  TYPE( mshFormat_ ) :: msh_Format
  TYPE( string ) :: line
  INTEGER( I4B ) :: ii, error
  CALL mshFile%initiate(file="./example.msh", status='OLD', &
    & action='READ')
  CALL mshFile%open()
  CALL msh_Format%read(mshFile=mshFile, error=error)
  CALL msh_Format%write(mshFile=mshFile, str="$MeshFormat", endstr="$EndMeshFormat")
  IF( error .EQ. 0 ) CALL Display( "Success!! ", "msg=")
  CALL mshFile%close()
END SUBROUTINE

end module test_m

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_m
implicit none
call test0
end program main