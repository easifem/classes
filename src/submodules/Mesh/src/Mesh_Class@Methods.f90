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
! date: 	22 Apr 2021
! summary: 	This module contains type bound procedure of [[Mesh_]]

SUBMODULE( Mesh_Class ) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_initiate
  CHARACTER( LEN=* ), PARAMETER :: myName="mesh_initiate()"
  INTEGER( I4B ) :: ierr, psize

  IF( .NOT. param%ispresent(key="nsd") ) THEN
    CALL eMesh%raiseError(modName//"::"//myName//" - "// &
      & "nsd key should be present in param")
  ELSE
    ierr = param%get(key="nsd", value=obj%nsd)
  END IF

  CALL obj%list%Initiate()
  IF( param%ispresent(key="size") ) THEN
    ierr = param%get(key="size", value=psize)
    CALL obj%list%Reserve(psize)
  END IF

END PROCEDURE mesh_initiate

!----------------------------------------------------------------------------
!                                                                       Mesh
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor1
  CALL ans%Initiate(param)
END PROCEDURE Constructor1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor_1
  ALLOCATE(Mesh_::ans)
  CALL ans%Initiate(param)
END PROCEDURE Constructor_1

!----------------------------------------------------------------------------
!                                                                    SetSize
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_PruneMesh
  ! define internal variables
  INTEGER( I4B ) :: tElements, iel
  TYPE( ElementPointerIterator_ ) :: iterator
  CHARACTER( LEN=* ), PARAMETER :: myName="mesh_PruneMesh()"

  IF( obj%list%isEmpty() ) THEN
    CALL eMesh%raiseError(modName//"::"//myName//" - "// &
      & "mesh is empty")
  ELSE
    iterator = obj%list%Begin()
    DO iel = 1, obj%list%size()
      IF( ASSOCIATED(iterator%value%ptr) ) CYCLE
      CALL obj%list%Erase(pos=iterator)
      CALL iterator%Inc()
    END DO
    CALL obj%list%ShrinkToFit()
  END IF

END PROCEDURE mesh_PruneMesh

!----------------------------------------------------------------------------
!                                                              AppendElement
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_Pushback
  TYPE( ElementPointer_ ) :: val
  val%ptr => Elem
  CALL obj%list%pushback(val)
END PROCEDURE mesh_Pushback

!----------------------------------------------------------------------------
!                                                                 SetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_SetElement
  CHARACTER( LEN=* ), PARAMETER :: myName="mesh_SetElement()"
  CALL eMesh%raiseError(modName//"::"//myName//" - "// &
      & "This method has not been implemented yet")
END PROCEDURE mesh_SetElement

!----------------------------------------------------------------------------
!                                                            ElementPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getElementPointer
  TYPE( ElementPointerIterator_ ) :: iterator
  iterator = obj%list%begin()
  iterator = iterator + (iel-1)
  ans => iterator%value%ptr
END PROCEDURE mesh_getElementPointer

!----------------------------------------------------------------------------
!                                                             RemoveElement
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_EraseElement
  CALL obj%list%Erase(pos=iel)
END PROCEDURE mesh_EraseElement

!----------------------------------------------------------------------------
!                                                             total_elements
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_size
  ans = obj%list%size()
END PROCEDURE mesh_size

!----------------------------------------------------------------------------
!                                                                    getNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getNptrs
  ! Define internal variables
  INTEGER( I4B ) :: ii, Dummy, MaxNptrs, tElements
  INTEGER( I4B ), ALLOCATABLE :: Nptrs0( : ), DummyNptrs( : )
  TYPE( ElementPointerIterator_ ) :: iter

  IF( ALLOCATED( Nptrs ) ) DEALLOCATE( Nptrs )
  tElements = obj%list%size()
  MaxNptrs = 0
  iter = obj%list%begin()
  ! Find the largest nptrs
  DO ii = 1, tElements
    IF( ASSOCIATED( iter%value%ptr ) ) THEN
      Dummy = MAXVAL( iter%value%ptr%getNptrs() )
      IF( Dummy .GE. MaxNptrs ) MaxNptrs = Dummy
    END IF
    CALL iter%inc()
  END DO

  ALLOCATE( DummyNptrs( MaxNptrs ) ); DummyNptrs = 0

  iter = obj%list%begin()
  DO ii = 1, tElements
    IF( ASSOCIATED( iter%value%ptr ) ) THEN
      Nptrs0 = iter%value%ptr%getNptrs()
      DummyNptrs( Nptrs0 ) = Nptrs0
    END IF
  END DO

  Dummy = COUNT( DummyNptrs .NE. 0 )
  ALLOCATE( Nptrs( Dummy ) ); Dummy = 0

  DO ii = 1, MaxNptrs
    IF( DummyNptrs( ii ) .EQ. 0 ) CYCLE
    Dummy = Dummy + 1
    Nptrs( Dummy ) = DummyNptrs( iel )
  END DO
  !
  DEALLOCATE( Nptrs0, DummyNptrs )
  NULLIFY( Elem )
END PROCEDURE mesh_getNptrs

!----------------------------------------------------------------------------
!                                                                   getNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE meshPointer_getNptrs
  ! ! Define internal variables
  ! INTEGER( I4B ) :: iel, Dummy, MaxNptrs, imesh
  ! INTEGER( I4B ), ALLOCATABLE :: Nptrs0( : ), DummyNptrs( : )
  ! CLASS( Element_ ), POINTER :: Elem


  ! IF( ALLOCATED( Nptrs ) ) DEALLOCATE( Nptrs )
  ! MaxNptrs = 0
  ! Elem => NULL( )
  ! !
  ! ! Find the largest nptrs
  ! DO imesh = 1, SIZE( obj )
  !   IF( ASSOCIATED( obj( imesh ) % Ptr ) ) THEN
  !     DO iel = 1, obj( imesh ) % Ptr % tElements
  !       Elem => obj( imesh ) % Ptr % Elem( iel  ) % Ptr
  !       IF( .NOT. ASSOCIATED( Elem ) ) EXIT
  !       Nptrs0 = .Nptrs. Elem
  !       Dummy = MAXVAL( Nptrs0 )
  !       IF( Dummy .GE. MaxNptrs ) MaxNptrs = Dummy
  !     END DO
  !   END IF
  ! END DO
  ! !
  ! ALLOCATE( DummyNptrs( MaxNptrs ) )
  ! DummyNptrs = 0
  ! !
  ! DO imesh = 1, SIZE( obj )
  !   IF( ASSOCIATED( obj( imesh ) % Ptr ) ) THEN
  !     DO iel = 1, obj( imesh ) % Ptr % tElements
  !       Elem => obj( imesh ) % Ptr % Elem( iel  ) % Ptr
  !       IF( .NOT. ASSOCIATED( Elem ) ) EXIT
  !       Nptrs0 = .Nptrs. Elem
  !       DummyNptrs( Nptrs0 ) = Nptrs0
  !     END DO
  !   END IF
  ! END DO
  ! !
  ! Dummy = COUNT( DummyNptrs .NE. 0 )
  ! ALLOCATE( Nptrs( Dummy ) )
  ! !
  ! Dummy = 0
  ! DO iel = 1, MaxNptrs
  !   IF( DummyNptrs( iel ) .EQ. 0 ) CYCLE
  !   Dummy = Dummy + 1
  !   Nptrs( Dummy ) = DummyNptrs( iel )
  ! END DO
  ! !
  ! DEALLOCATE( Nptrs0, DummyNptrs )
  ! NULLIFY( Elem )
END PROCEDURE meshPointer_getNptrs

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_setMaterialType
  ! INTEGER( I4B ) :: iel
  ! CLASS( Element_ ), POINTER :: Elem

  ! Elem => NULL( )
  ! DO iel = 1, obj % tElements
  !   Elem => obj % Elem( iel ) % Ptr
  !   IF( .NOT. ASSOCIATED( Elem ) ) EXIT
  !   CALL Elem % setMaterialType( MatType )
  ! END DO
END PROCEDURE mesh_setMaterialType

!----------------------------------------------------------------------------
!                                                            DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_DeallocateData
  ! IF( ALLOCATED( obj % Elem ) ) DEALLOCATE( obj % Elem )
  ! obj % tElements = 0
  ! obj % maxElements = 0
  ! obj % NSD = 0
END PROCEDURE mesh_DeallocateData

!----------------------------------------------------------------------------
!                                                                 Final
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_final
  CALL obj%DeallocateData()
END PROCEDURE mesh_final

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_display
  ! ! define internal variable
  ! INTEGER( I4B ) :: I, tElements, iel
  ! CLASS( Element_ ), POINTER :: Elem
  ! !
  ! IF( PRESENT( UnitNo ) ) THEN
  !   I = UnitNo
  ! ELSE
  !   I = stdOut
  ! END IF
  ! !
  ! IF( LEN_TRIM( Msg ) .NE. 0 ) THEN
  !   WRITE( I, "(A)" ) TRIM( Msg )
  ! END IF
  ! !
  ! Elem => NULL( )
  ! DO iel =1, obj % tElements
  !   Elem => obj % ElementPointer( iel )
  !   IF( .NOT. ASSOCIATED( Elem ) ) CYCLE
  !   CALL BlankLines( UnitNo = I, NOL = 1 )
  !   CALL Elem % Display( &
  !     & Msg = "## Element( " // TRIM( INT2STR( iel ) ) // " )", &
  !     & UnitNo = I )
  ! END DO
  ! !
  ! NULLIFY( Elem )
END PROCEDURE mesh_display

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE Methods
