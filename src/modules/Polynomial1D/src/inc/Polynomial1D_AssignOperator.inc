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


!----------------------------------------------------------------------------
!                                                       Assign@AssignMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL SUBROUTINE func_AssignObjObj( obj, obj2 )
  CLASS( Polynomial1D_ ), INTENT( INOUT ) :: obj
  CLASS( Polynomial1D_ ), INTENT( IN ) :: obj2
END SUBROUTINE func_AssignObjObj
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Assign@AssignMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL SUBROUTINE func_AssignObjMono( obj, obj2 )
  CLASS( Polynomial1D_ ), INTENT( INOUT ) :: obj
  CLASS( Monomial1D_ ), INTENT( IN ) :: obj2
END SUBROUTINE func_AssignObjMono
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Assign@AssignMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL SUBROUTINE func_AssignObjInt8( obj, obj2 )
  CLASS( Polynomial1D_ ), INTENT( INOUT ) :: obj
  INTEGER( Int8 ), INTENT( IN ) :: obj2
END SUBROUTINE func_AssignObjInt8
!!
MODULE ELEMENTAL SUBROUTINE func_AssignObjInt16( obj, obj2 )
  CLASS( Polynomial1D_ ), INTENT( INOUT ) :: obj
  INTEGER( Int16 ), INTENT( IN ) :: obj2
END SUBROUTINE func_AssignObjInt16
!!
MODULE ELEMENTAL SUBROUTINE func_AssignObjInt32( obj, obj2 )
  CLASS( Polynomial1D_ ), INTENT( INOUT ) :: obj
  INTEGER( Int32 ), INTENT( IN ) :: obj2
END SUBROUTINE func_AssignObjInt32
!!
MODULE ELEMENTAL SUBROUTINE func_AssignObjInt64( obj, obj2 )
  CLASS( Polynomial1D_ ), INTENT( INOUT ) :: obj
  INTEGER( Int64 ), INTENT( IN ) :: obj2
END SUBROUTINE func_AssignObjInt64
!!
MODULE ELEMENTAL SUBROUTINE func_AssignObjReal32( obj, obj2 )
  CLASS( Polynomial1D_ ), INTENT( INOUT ) :: obj
  REAL( Real32), INTENT( IN ) :: obj2
END SUBROUTINE func_AssignObjReal32
!!
MODULE ELEMENTAL SUBROUTINE func_AssignObjReal64( obj, obj2 )
  CLASS( Polynomial1D_ ), INTENT( INOUT ) :: obj
  REAL( Real64), INTENT( IN ) :: obj2
END SUBROUTINE func_AssignObjReal64
END INTERFACE
