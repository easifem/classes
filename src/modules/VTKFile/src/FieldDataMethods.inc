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
!                                          WriteFieldData@FieldDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: Write Field Data

INTERFACE
MODULE SUBROUTINE VTKFile_WriteFieldData_1( obj, name, x )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  CLASS( * ), INTENT( IN ) :: x
END SUBROUTINE VTKFile_WriteFieldData_1
END INTERFACE

!----------------------------------------------------------------------------
!                                          WriteFieldData@FieldDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: Write Field Data

INTERFACE
MODULE SUBROUTINE VTKFile_WriteFieldData_2( obj, action )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: action
    !! Open, Close
END SUBROUTINE VTKFile_WriteFieldData_2
END INTERFACE
