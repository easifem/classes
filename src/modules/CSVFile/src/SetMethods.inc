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

INTERFACE
MODULE SUBROUTINE txt_setCSVFileProperties( obj, isQuotedStrings, &
  & isQuotedData, TrueChar, FalseChar, chunk_size, echostat, echounit )
  CLASS( CSVFile_ ), INTENT( INOUT ) :: obj
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isQuotedStrings
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isQuotedData
  CHARACTER( LEN = 1 ), OPTIONAL, INTENT( IN ) :: TrueChar
  CHARACTER( LEN = 1 ), OPTIONAL, INTENT( IN ) :: FalseChar
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: chunk_size
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: echostat
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: echounit
END SUBROUTINE txt_setCSVFileProperties
END INTERFACE

!----------------------------------------------------------------------------
!                                                   setSkipRows@SetMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE txt_setSkipRows( obj, indx )
  CLASS( CSVFile_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: indx( : )
END SUBROUTINE txt_setSkipRows
END INTERFACE

!----------------------------------------------------------------------------
!                                                  setHeaderIndx@SetMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE txt_setHeaderIndx( obj, indx )
  CLASS( CSVFile_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: indx
END SUBROUTINE txt_setHeaderIndx
END INTERFACE
