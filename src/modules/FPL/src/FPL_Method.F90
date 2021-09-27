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
! date: 16 July 2021
! summary: This module contains extra method for Fortran Parameter Lists

MODULE FPL_Method
USE easifemBase
USE FPL, ONLY: ParameterList_
USE ExceptionHandler_Class
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName = "MATRIXFIELD_CLASS"
TYPE( ExceptionHandler_ ) :: e

INTERFACE set
  MODULE PROCEDURE fpl_set1
END INTERFACE set

PUBLIC :: set


INTERFACE getValue
  MODULE PROCEDURE fpl_getValue1
END INTERFACE getValue

PUBLIC :: getValue

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                                 fpl_set1
!----------------------------------------------------------------------------

SUBROUTINE fpl_set1(obj, key, value)
  ! Define dummy variables
  TYPE( ParameterList_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: key
  TYPE( DOF_ ), INTENT( IN ) :: value
  ! Internal variable
  INTEGER( I4B ) :: ierr
  ierr = obj%set( key=TRIM(key)//"-map", value=value%map )
  ierr = obj%set( key=TRIM(key)//"-valmap", value=value%valmap )
  ierr = obj%set( key=TRIM(key)//"-storageFMT", value=value%storageFMT )
END SUBROUTINE fpl_set1

!----------------------------------------------------------------------------
!                                                                 fpl_get
!----------------------------------------------------------------------------

SUBROUTINE fpl_getValue1(obj, key, value)
  ! Define dummy variables
  TYPE( ParameterList_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: key
  TYPE( DOF_ ), INTENT( INOUT ) :: value
  ! Internal variable
  INTEGER( I4B ) :: ierr
  INTEGER( I4B ), ALLOCATABLE :: s( : )
  ierr = obj%getShape( key=TRIM(key)//"-map", shape=s )
  CALL Reallocate( value%map, s(1), s(2) )
  ierr = obj%getShape( key=TRIM(key)//"-valmap", shape=s )
  CALL Reallocate( value%valmap, s(1) )
  ierr = obj%get( key=TRIM(key)//"-map", value=value%map )
  ierr = obj%get( key=TRIM(key)//"-valmap", value=value%valmap )
  ierr = obj%get( key=TRIM(key)//"-storageFMT", value=value%storageFMT )
  DEALLOCATE( s )
END SUBROUTINE fpl_getValue1

END MODULE FPL_Method