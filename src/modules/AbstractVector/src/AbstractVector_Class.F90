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

MODULE AbstractVector_Class
USE GlobalData
USE BaseType
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
USE FPL, ONLY: ParameterList_
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName = "ABSTRACTVECTOR_CLASS"

!----------------------------------------------------------------------------
!                                                           AbstractVector_
!----------------------------------------------------------------------------

TYPE, ABSTRACT :: AbstractVector_
  LOGICAL( LGT ) :: isInitiated = .FALSE.
  INTEGER( I4B ) :: tSize = 0
  CONTAINS
  PRIVATE
    PROCEDURE(aVec_Initiate), DEFERRED, PUBLIC, PASS( obj ) :: initiate
    PROCEDURE(aVec_Deallocate), DEFERRED, PUBLIC, PASS( obj ) :: Deallocate
    PROCEDURE(aVec_Display), DEFERRED, PUBLIC, PASS( obj ) :: Display
    PROCEDURE(aVec_set1), DEFERRED, PASS( obj ) :: set1
    PROCEDURE(aVec_set2), DEFERRED, PASS( obj ) :: set2
    PROCEDURE(aVec_set3), DEFERRED, PASS( obj ) :: set3
    PROCEDURE(aVec_set4), DEFERRED, PASS( obj ) :: set4
    PROCEDURE(aVec_set5), DEFERRED, PASS( obj ) :: set5
    PROCEDURE(aVec_set6), DEFERRED, PASS( obj ) :: set6
    PROCEDURE(aVec_set7), DEFERRED, PASS( obj ) :: set7
    GENERIC, PUBLIC :: set => set1, set2, set3, &
      & set4, set5, set6, set7

    PROCEDURE(aVec_get1), DEFERRED, PASS( obj ) :: get1
    PROCEDURE(aVec_get2), DEFERRED, PASS( obj ) :: get2
    PROCEDURE(aVec_get3), DEFERRED, PASS( obj ) :: get3
    PROCEDURE(aVec_get4), DEFERRED, PASS( obj ) :: get4
    GENERIC, PUBLIC :: get => get1, get2, &
      & get3, get4
END TYPE AbstractVector_

PUBLIC :: AbstractVector_

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE aVec_Initiate( obj, param )
  IMPORT :: AbstractVector_, ParameterList_
  CLASS( AbstractVector_ ), INTENT( INOUT ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
END SUBROUTINE aVec_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE aVec_Display( obj, msg, unitNo )
  IMPORT :: AbstractVector_, I4B
  CLASS( AbstractVector_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitNo
END SUBROUTINE aVec_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE aVec_Deallocate( obj )
  IMPORT :: AbstractVector_
  CLASS( AbstractVector_ ), INTENT( INOUT ) :: obj
END SUBROUTINE aVec_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE aVec_set1( obj, nodenum, value )
  IMPORT :: AbstractVector_, I4B, DFP
  CLASS( AbstractVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodenum
  REAL( DFP ), INTENT( IN ) :: value
END SUBROUTINE aVec_set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Set
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE aVec_set2( obj, value )
  IMPORT :: AbstractVector_, DFP
  CLASS( AbstractVector_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: value
END SUBROUTINE aVec_set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE aVec_set3( obj, value )
  IMPORT :: AbstractVector_, DFP
  CLASS( AbstractVector_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: value( : )
END SUBROUTINE aVec_set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE aVec_set4(obj, nodenum, value)
  IMPORT :: AbstractVector_, I4B, DFP
  CLASS( AbstractVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  REAL( DFP ), INTENT( IN ) :: value( : )
END SUBROUTINE aVec_set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE aVec_set5( obj, iStart, iEnd, stride, value )
  IMPORT :: AbstractVector_, I4B, DFP
  CLASS( AbstractVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: istart, iend, stride
  REAL( DFP ), INTENT( IN ) :: value
END SUBROUTINE aVec_set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE aVec_set6( obj, istart, iend, stride, value )
  IMPORT :: AbstractVector_, I4B, DFP
  CLASS( AbstractVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: istart
  INTEGER( I4B ), INTENT( IN ) :: iend
  INTEGER( I4B ), INTENT( IN ) :: stride
  REAL( DFP ), INTENT( IN ) :: value( : )
END SUBROUTINE aVec_set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE aVec_set7( obj, nodeNum, value, storageFMT, dofs )
  IMPORT :: AbstractVector_, I4B, DFP
  CLASS( AbstractVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodeNum( : )
  REAL( DFP ), INTENT( IN ) :: value( : )
  INTEGER( I4B ), INTENT( IN ) :: storageFMT
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: dofs(:)
END SUBROUTINE aVec_set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

INTERFACE
FUNCTION aVec_get1( obj, nodenum ) RESULT( ans )
  IMPORT :: AbstractVector_, I4B, DFP
  CLASS( AbstractVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodenum
  REAL( DFP ) :: ans
END FUNCTION aVec_get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

INTERFACE
FUNCTION aVec_get2( obj ) RESULT( ans )
  IMPORT :: AbstractVector_, DFP
  CLASS( AbstractVector_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE:: ans( : )
END FUNCTION aVec_get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

INTERFACE
FUNCTION aVec_get3( obj, nodenum ) RESULT( ans )
  IMPORT :: AbstractVector_, DFP, I4B
  CLASS( AbstractVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  REAL( DFP ) :: ans( SIZE( nodenum ) )
END FUNCTION aVec_get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

INTERFACE
FUNCTION aVec_get4( obj, istart, iend, stride ) RESULT( ans )
  IMPORT :: AbstractVector_, DFP, I4B
  CLASS( AbstractVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: istart, iend, stride
  REAL( DFP ), ALLOCATABLE :: ans( : )
END FUNCTION aVec_get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
FUNCTION aVec_get5( obj, nodeNum, storageFMT, dofs ) RESULT( ans )
  IMPORT :: AbstractVector_, I4B, DFP
  CLASS( AbstractVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodeNum( : )
  INTEGER( I4B ), INTENT( IN ) :: storageFMT
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: dofs(:)
  REAL( DFP ), ALLOCATABLE :: ans( : )
END FUNCTION aVec_get5
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractVector_Class