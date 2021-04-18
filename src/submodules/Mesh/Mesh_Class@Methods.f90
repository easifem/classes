SUBMODULE( Mesh_Class ) Methods
  !! This module contains type bound procedure of [[Mesh_]]
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE allocateMeshSize
  IF( PRESENT( factor ) ) THEN
    obj % maxElements= factor * tElements
  ELSE
    obj % maxElements = default_factor * tElements
  END IF
  IF( ALLOCATED( obj % Elem ) ) DEALLOCATE( obj % Elem )
  ALLOCATE( obj % Elem( obj % maxElements ) )
  obj % tElements = 0
  obj % NSD = NSD
END PROCEDURE allocateMeshSize

!----------------------------------------------------------------------------
!                                                                       Mesh
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor1
  IF( PRESENT( factor ) ) THEN
    CALL ans % Initiate( NSD = NSD, tElements = tElements, factor = factor )
  ELSE
    CALL ans % Initiate( NSD = NSD, tElements = tElements )
  END IF
END PROCEDURE Constructor1

!----------------------------------------------------------------------------
!                                                            DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE Deallocate_data
  IF( ALLOCATED( obj % Elem ) ) DEALLOCATE( obj % Elem )
  obj % tElements = 0
  obj % maxElements = 0
  obj % NSD = 0
END PROCEDURE Deallocate_data

!----------------------------------------------------------------------------
!                                                                    SetSize
!----------------------------------------------------------------------------

MODULE PROCEDURE set_total_elements
  ! define internal variables
  INTEGER( I4B ) :: tElements, iel
  CLASS( Element_ ), POINTER :: Elem
  !
  tElements = 0; Elem => NULL( )
  !
  DO iel = 1, obj % maxElements
    Elem => obj % Elem( iel ) % Ptr ! get elem pointer
    IF( .NOT. ASSOCIATED( Elem ) ) EXIT
    tElements = tElements + 1
  END DO
  !
  obj % tElements = tElements
  !
  NULLIFY( Elem )
END PROCEDURE set_total_elements

!----------------------------------------------------------------------------
!                                                              AppendElement
!----------------------------------------------------------------------------

MODULE PROCEDURE add_element
  TYPE( ElementPointer_ ), ALLOCATABLE :: TempElem( : )
  INTEGER( I4B ) :: NSD, tElements, iel

  IF( obj % tElements .EQ. obj % maxElements ) THEN
    NSD = obj % NSD; tElements= obj % tElements
    ALLOCATE( TempElem( tElements ) )
    DO iel = 1, tElements
      TempElem( iel ) % Ptr => obj % Elem( iel ) % Ptr
      obj % Elem( iel ) % Ptr => NULL( )
    END DO
    CALL DeallocateData( obj )
    CALL obj % Initiate( NSD = NSD,  tElements = tElements+1 )
    DO iel = 1, tElements
      obj % Elem( iel ) % Ptr => TempElem( iel ) % Ptr
      TempElem( iel ) % Ptr => NULL( )
    END DO
    obj % tElements = tElements + 1
    obj % Elem( obj % tElements ) % Ptr => Elem
  ELSE
    obj % tElements = obj % tElements + 1
    obj % Elem( obj % tElements ) % Ptr => Elem
  END IF
END PROCEDURE add_element

!----------------------------------------------------------------------------
!                                                                 SetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE set_Element
  ! define internal variable
  IF( iel .LE. obj % tElements ) THEN
    obj % Elem( iel ) % Ptr => Elem
  END IF
END PROCEDURE set_Element

!----------------------------------------------------------------------------
!                                                            ElementPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE getElement_Pointer
  IF( iel .LE. obj % maxElements ) THEN
    ans => obj % Elem( iel ) % Ptr
  ELSE
    ans => NULL( )
  END IF
END PROCEDURE getElement_Pointer

!----------------------------------------------------------------------------
!                                                             RemoveElement
!----------------------------------------------------------------------------

MODULE PROCEDURE remove_element
  INTEGER( I4B ) :: j

  SELECT CASE( extraoption )
  CASE( 0 )
  ! nullify
  obj % Elem( iel ) % Ptr => NULL( )

  CASE( 1 )
  ! nullify + deallocate
  IF( ASSOCIATED( obj % Elem( iel ) % Ptr ) ) THEN
    DEALLOCATE( obj % Elem( iel ) % Ptr )
  END IF
  obj % Elem( iel ) % Ptr => NULL( )

  CASE( 2 )
  ! nullify + rearrange
  obj % Elem( iel ) % Ptr => NULL( )
  DO j = iel, obj % tElements-1
    obj % Elem( j ) % Ptr => obj % Elem( j + 1 ) % Ptr
  END DO
  obj % Elem( obj % tElements ) % Ptr => NULL( )
  obj % tElements = obj % tElements - 1

  CASE( 3 )
  ! nullify + deallocate + rearrange
  DEALLOCATE( obj % Elem( iel ) % Ptr )
  obj % Elem( iel ) % Ptr => NULL( )
  DO j = iel, obj % tElements-1
    obj % Elem( j ) % Ptr => obj % Elem( j + 1 ) % Ptr
  END DO
  obj % Elem( obj % tElements ) % Ptr => NULL( )
  obj % tElements = obj % tElements - 1
  END SELECT
END PROCEDURE remove_element

!----------------------------------------------------------------------------
!                                                             total_elements
!----------------------------------------------------------------------------

MODULE PROCEDURE total_elements
  ans = obj % tElements
END PROCEDURE total_elements

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE display_mesh
  ! define internal variable
  INTEGER( I4B ) :: I, tElements, iel
  CLASS( Element_ ), POINTER :: Elem
  !
  IF( PRESENT( UnitNo ) ) THEN
    I = UnitNo
  ELSE
    I = stdOut
  END IF
  !
  IF( LEN_TRIM( Msg ) .NE. 0 ) THEN
    WRITE( I, "(A)" ) TRIM( Msg )
  END IF
  !
  Elem => NULL( )
  DO iel =1, obj % tElements
    Elem => obj % ElementPointer( iel )
    IF( .NOT. ASSOCIATED( Elem ) ) CYCLE
    CALL BlankLines( UnitNo = I, NOL = 1 )
    CALL Elem % Display( &
      & Msg = "## Element( " // TRIM( INT2STR( iel ) ) // " )", &
      & UnitNo = I )
  END DO
  !
  NULLIFY( Elem )
END PROCEDURE display_mesh

!----------------------------------------------------------------------------
!                                                                    getNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE get_nptrs
  ! Define internal variables
  INTEGER( I4B ) :: iel, Dummy, MaxNptrs, tElements
  INTEGER( I4B ), ALLOCATABLE :: Nptrs0( : ), DummyNptrs( : )
  CLASS( Element_ ), POINTER :: Elem
  !
  IF( ALLOCATED( Nptrs ) ) DEALLOCATE( Nptrs )
  !
  tElements = obj % tElements
  MaxNptrs = 0
  Elem => NULL( )
  !
  ! Find the largest nptrs
  DO iel = 1, obj % tElements
    Elem => obj % Elem( iel  ) % Ptr
    IF( .NOT. ASSOCIATED( Elem ) ) EXIT
    Nptrs0 = .Nptrs. Elem
    Dummy = MAXVAL( Nptrs0 )
    IF( Dummy .GE. MaxNptrs ) MaxNptrs = Dummy
  END DO
  !
  ALLOCATE( DummyNptrs( MaxNptrs ) )
  DummyNptrs = 0
  !
  DO iel = 1, tElements
    Elem => obj % Elem( iel  ) % Ptr
    IF( .NOT. ASSOCIATED( Elem ) ) EXIT
    Nptrs0 = .Nptrs. Elem
    DummyNptrs( Nptrs0 ) = Nptrs0
  END DO
  !
  Dummy = COUNT( DummyNptrs .NE. 0 )
  ALLOCATE( Nptrs( Dummy ) )
  !
  Dummy = 0
  DO iel = 1, MaxNptrs
    IF( DummyNptrs( iel ) .EQ. 0 ) CYCLE
    Dummy = Dummy + 1
    Nptrs( Dummy ) = DummyNptrs( iel )
  END DO
  !
  DEALLOCATE( Nptrs0, DummyNptrs )
  NULLIFY( Elem )
END PROCEDURE get_nptrs

!----------------------------------------------------------------------------
!                                                                   getNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_pointer_get_nptrs
  ! Define internal variables
  INTEGER( I4B ) :: iel, Dummy, MaxNptrs, imesh
  INTEGER( I4B ), ALLOCATABLE :: Nptrs0( : ), DummyNptrs( : )
  CLASS( Element_ ), POINTER :: Elem


  IF( ALLOCATED( Nptrs ) ) DEALLOCATE( Nptrs )
  MaxNptrs = 0
  Elem => NULL( )
  !
  ! Find the largest nptrs
  DO imesh = 1, SIZE( obj )
    IF( ASSOCIATED( obj( imesh ) % Ptr ) ) THEN
      DO iel = 1, obj( imesh ) % Ptr % tElements
        Elem => obj( imesh ) % Ptr % Elem( iel  ) % Ptr
        IF( .NOT. ASSOCIATED( Elem ) ) EXIT
        Nptrs0 = .Nptrs. Elem
        Dummy = MAXVAL( Nptrs0 )
        IF( Dummy .GE. MaxNptrs ) MaxNptrs = Dummy
      END DO
    END IF
  END DO
  !
  ALLOCATE( DummyNptrs( MaxNptrs ) )
  DummyNptrs = 0
  !
  DO imesh = 1, SIZE( obj )
    IF( ASSOCIATED( obj( imesh ) % Ptr ) ) THEN
      DO iel = 1, obj( imesh ) % Ptr % tElements
        Elem => obj( imesh ) % Ptr % Elem( iel  ) % Ptr
        IF( .NOT. ASSOCIATED( Elem ) ) EXIT
        Nptrs0 = .Nptrs. Elem
        DummyNptrs( Nptrs0 ) = Nptrs0
      END DO
    END IF
  END DO
  !
  Dummy = COUNT( DummyNptrs .NE. 0 )
  ALLOCATE( Nptrs( Dummy ) )
  !
  Dummy = 0
  DO iel = 1, MaxNptrs
    IF( DummyNptrs( iel ) .EQ. 0 ) CYCLE
    Dummy = Dummy + 1
    Nptrs( Dummy ) = DummyNptrs( iel )
  END DO
  !
  DEALLOCATE( Nptrs0, DummyNptrs )
  NULLIFY( Elem )
END PROCEDURE mesh_pointer_get_nptrs

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE setMaterialType_1
  INTEGER( I4B ) :: iel
  CLASS( Element_ ), POINTER :: Elem

  Elem => NULL( )
  DO iel = 1, obj % tElements
    Elem => obj % Elem( iel ) % Ptr
    IF( .NOT. ASSOCIATED( Elem ) ) EXIT
    CALL Elem % setMaterialType( MatType )
  END DO
END PROCEDURE setMaterialType_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE Methods
