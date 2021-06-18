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
! date: 18 June 2021
! summary: This submodule contains methods for domain object

SUBMODULE( Domain_Class ) DomainMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_Initiate
  CHARACTER( LEN = * ), PARAMETER :: myName = "Domain_Initiate"

  IF( obj%isInitiated ) THEN
    CALL eDomain%raiseError(modName//"::"//myName//" - "// &
      & "Domain is already initiated.")
  ELSE
    obj%isInitiated = .TRUE.
  END IF

  IF( PRESENT( tOmega ) ) THEN
    CALL obj%meshList( 3 )%initiate( tOmega )
  END IF

  IF( PRESENT( tBoundary ) ) THEN
    CALL obj%meshList( 2 )%initiate( tBoundary )
  END IF

  IF( PRESENT( tEdge ) ) THEN
    CALL obj%meshList( 1 )%initiate( tEdge )
  END IF
END PROCEDURE Domain_Initiate

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_DeallocateData
  INTEGER( I4B ) :: ii
  obj%isInitiated = .FALSE.
  CALL obj%domainData%DeallocateData()
  DO ii = 0, 3
    CALL obj%meshList( ii )%DeallocateData()
  END DO
END PROCEDURE Domain_DeallocateData

!----------------------------------------------------------------------------
!                                                              Final
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_Final
  CALL Obj%DeallocateData()
END PROCEDURE Domain_Final

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE DomainMethods