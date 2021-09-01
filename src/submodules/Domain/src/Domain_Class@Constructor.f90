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

SUBMODULE( Domain_Class ) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            addSurrogate
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_addSurrogate
  CALL e%addSurrogate(userObj)
END PROCEDURE Domain_addSurrogate

!----------------------------------------------------------------------------
!                                                            addSurrogate
!----------------------------------------------------------------------------

MODULE PROCEDURE addSurrogate_Domain
  CALL e%addSurrogate(userObj)
END PROCEDURE addSurrogate_Domain

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_Initiate
  CHARACTER( LEN = * ), PARAMETER :: myName="Domain_Initiate"
  CLASS( ExceptionHandler_ ), POINTER :: surr
  LOGICAL( LGT ) :: exist, opened

  !> setting up exception messages output settings
  surr => NULL(); CALL e%getSurrogate(surr)
  IF( ASSOCIATED( surr ) ) CALL addSurrogate_Mesh( surr )
  surr => NULL()
  ! > Exception related to Mesh_ data type wil be printed in the
  ! domain only
  CALL e%raiseInformation( modName//'::'//myName//'-'// &
    & 'Initiating domain' )
  CALL obj%import( hdf5, group )
  CALL e%raiseInformation( modName//'::'//myName//'-'// &
    & 'Domain has been initiated' )
  !> now we are going to fix the nodal coordinates
  CALL e%raiseInformation( modName//'::'//myName//'-'// &
    & 'Fixing nodal coordinates' )
  CALL e%raiseInformation( modName//'::'//myName//'-'// &
    & 'Nodal coordinates fixed' )
END PROCEDURE Domain_Initiate

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_DeallocateData
  INTEGER( I4B ) :: ii, jj
  obj%isInitiated = .FALSE.
  obj%engine = ''
  obj%majorVersion = 0
  obj%minorVersion = 0
  obj%version = 0.0_DFP
  obj%nsd = 0
  obj%maxNptrs = 0
  obj%minNptrs = 0
  obj%tNodes = 0
  obj%isNodeNumberSparse = .FALSE.
  obj%maxElemNum = 0
  obj%minElemNum = 0
  obj%isElemNumberSparse = .FALSE.
  obj%tEntitiesForNodes = 0
  obj%tEntitiesForElements = 0
  IF( ALLOCATED( obj%NSDVec ) ) DEALLOCATE( obj%NSDVec )
  IF( ALLOCATED( obj%tag ) ) DEALLOCATE( obj%tag )
  IF( ALLOCATED( obj%numElements ) ) DEALLOCATE( obj%numElements )
  IF( ALLOCATED( obj%numNodes ) ) DEALLOCATE( obj%numNodes )
  IF( ALLOCATED( obj%entities ) ) DEALLOCATE( obj%entities )
  IF( ALLOCATED( obj%physicalName ) ) DEALLOCATE( obj%physicalName )
  obj%tElements( 0:3 ) = 0
  obj%tEntities( 0:3 ) = 0
  IF( ALLOCATED( obj%meshList ) ) DEALLOCATE( obj%meshList )
  IF( ALLOCATED( obj%nodeCoord ) ) DEALLOCATE( obj%nodeCoord )
  IF( ALLOCATED( obj%local_nptrs ) ) DEALLOCATE( obj%local_nptrs )
END PROCEDURE Domain_DeallocateData

!----------------------------------------------------------------------------
!                                                              Final
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_Final
  CALL Obj%DeallocateData()
END PROCEDURE Domain_Final

!----------------------------------------------------------------------------
!                                                             Domain
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_Constructor1
  CALL ans%initiate( hdf5, group )
END PROCEDURE Domain_Constructor1

!----------------------------------------------------------------------------
!                                                            Domain_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_Constructor_1
  ALLOCATE( ans )
  CALL ans%initiate( hdf5, group )
END PROCEDURE Domain_Constructor_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE Constructor