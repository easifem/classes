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

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: 	copy one [[Polynomial2D_]] into other.

INTERFACE
MODULE ELEMENTAL SUBROUTINE func_AssignObjObj( obj, obj2 )
  CLASS( Polynomial2D_ ), INTENT( INOUT ) :: obj
  CLASS( Polynomial2D_ ), INTENT( IN ) :: obj2
END SUBROUTINE func_AssignObjObj
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Assign@AssignMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: 	Copy [[Polynomial2D_]] from [[Monomial2D_]]

INTERFACE
MODULE ELEMENTAL SUBROUTINE func_AssignObjMono( obj, obj2 )
  CLASS( Polynomial2D_ ), INTENT( INOUT ) :: obj
  CLASS( Monomial2D_ ), INTENT( IN ) :: obj2
END SUBROUTINE func_AssignObjMono
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Assign@AssignMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: 	Copy [[Polynomial2D_]] from integer

INTERFACE
MODULE ELEMENTAL SUBROUTINE func_AssignObjInt8( obj, obj2 )
  CLASS( Polynomial2D_ ), INTENT( INOUT ) :: obj
  INTEGER( Int8 ), INTENT( IN ) :: obj2
END SUBROUTINE func_AssignObjInt8
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: 	Copy [[Polynomial2D_]] from integer

INTERFACE
MODULE ELEMENTAL SUBROUTINE func_AssignObjInt16( obj, obj2 )
  CLASS( Polynomial2D_ ), INTENT( INOUT ) :: obj
  INTEGER( Int16 ), INTENT( IN ) :: obj2
END SUBROUTINE func_AssignObjInt16
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: 	Copy [[Polynomial2D_]] from integer

INTERFACE
MODULE ELEMENTAL SUBROUTINE func_AssignObjInt32( obj, obj2 )
  CLASS( Polynomial2D_ ), INTENT( INOUT ) :: obj
  INTEGER( Int32 ), INTENT( IN ) :: obj2
END SUBROUTINE func_AssignObjInt32
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: 	Copy [[Polynomial2D_]] from integer

INTERFACE
MODULE ELEMENTAL SUBROUTINE func_AssignObjInt64( obj, obj2 )
  CLASS( Polynomial2D_ ), INTENT( INOUT ) :: obj
  INTEGER( Int64 ), INTENT( IN ) :: obj2
END SUBROUTINE func_AssignObjInt64
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: 	Copy [[Polynomial2D_]] from real

INTERFACE
MODULE ELEMENTAL SUBROUTINE func_AssignObjReal32( obj, obj2 )
  CLASS( Polynomial2D_ ), INTENT( INOUT ) :: obj
  REAL( Real32), INTENT( IN ) :: obj2
END SUBROUTINE func_AssignObjReal32
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: 	Copy [[Polynomial2D_]] from real

INTERFACE
MODULE ELEMENTAL SUBROUTINE func_AssignObjReal64( obj, obj2 )
  CLASS( Polynomial2D_ ), INTENT( INOUT ) :: obj
  REAL( Real64), INTENT( IN ) :: obj2
END SUBROUTINE func_AssignObjReal64
END INTERFACE
