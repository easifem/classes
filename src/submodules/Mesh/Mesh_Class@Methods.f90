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
    Obj % maxElements= factor * tElements
  ELSE
    Obj % maxElements = default_factor * tElements
  END IF
  IF( ALLOCATED( Obj % Elem ) ) DEALLOCATE( Obj % Elem )
  ALLOCATE( Obj % Elem( Obj % maxElements ) )
  Obj % tElements = 0
  Obj % NSD = NSD
END PROCEDURE allocateMeshSize

!----------------------------------------------------------------------------
!                                                                       Mesh
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor1
  IF( PRESENT( factor ) ) THEN
    CALL Ans % Initiate( NSD = NSD, tElements = tElements, factor = factor )
  ELSE
    CALL Ans % Initiate( NSD = NSD, tElements = tElements )
  END IF
END PROCEDURE Constructor1

!----------------------------------------------------------------------------
!                                                            DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE Deallocate_data
  IF( ALLOCATED( Obj % Elem ) ) DEALLOCATE( Obj % Elem )
  Obj % tElements = 0
  Obj % maxElements = 0
  Obj % NSD = 0
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
  DO iel = 1, Obj % maxElements
    Elem => Obj % Elem( iel ) % Ptr ! get elem pointer
    IF( .NOT. ASSOCIATED( Elem ) ) EXIT
    tElements = tElements + 1
  END DO
  !
  Obj % tElements = tElements
  !
  NULLIFY( Elem )
END PROCEDURE set_total_elements

!----------------------------------------------------------------------------
!                                                              AppendElement
!----------------------------------------------------------------------------

MODULE PROCEDURE add_element
  TYPE( ElementPointer_ ), ALLOCATABLE :: TempElem( : )
  INTEGER( I4B ) :: NSD, tElements, iel

  IF( Obj % tElements .EQ. Obj % maxElements ) THEN
    NSD = Obj % NSD; tElements= Obj % tElements
    ALLOCATE( TempElem( tElements ) )
    DO iel = 1, tElements
      TempElem( iel ) % Ptr => Obj % Elem( iel ) % Ptr
      Obj % Elem( iel ) % Ptr => NULL( )
    END DO
    CALL DeallocateData( Obj )
    CALL Obj % Initiate( NSD = NSD,  tElements = tElements+1 )
    DO iel = 1, tElements
      Obj % Elem( iel ) % Ptr => TempElem( iel ) % Ptr
      TempElem( iel ) % Ptr => NULL( )
    END DO
    Obj % tElements = tElements + 1
    Obj % Elem( Obj % tElements ) % Ptr => Elem
  ELSE
    Obj % tElements = Obj % tElements + 1
    Obj % Elem( Obj % tElements ) % Ptr => Elem
  END IF
END PROCEDURE add_element

!----------------------------------------------------------------------------
!                                                                 SetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE set_Element
  ! define internal variable
  IF( iel .LE. Obj % tElements ) THEN
    Obj % Elem( iel ) % Ptr => Elem
  END IF
END PROCEDURE set_Element

!----------------------------------------------------------------------------
!                                                            ElementPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE getElement_Pointer
  IF( iel .LE. Obj % maxElements ) THEN
    Ans => Obj % Elem( iel ) % Ptr
  ELSE
    Ans => NULL( )
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
  Obj % Elem( iel ) % Ptr => NULL( )

  CASE( 1 )
  ! nullify + deallocate
  IF( ASSOCIATED( Obj % Elem( iel ) % Ptr ) ) THEN
    DEALLOCATE( Obj % Elem( iel ) % Ptr )
  END IF
  Obj % Elem( iel ) % Ptr => NULL( )

  CASE( 2 )
  ! nullify + rearrange
  Obj % Elem( iel ) % Ptr => NULL( )
  DO j = iel, Obj % tElements-1
    Obj % Elem( j ) % Ptr => Obj % Elem( j + 1 ) % Ptr
  END DO
  Obj % Elem( Obj % tElements ) % Ptr => NULL( )
  Obj % tElements = Obj % tElements - 1

  CASE( 3 )
  ! nullify + deallocate + rearrange
  DEALLOCATE( Obj % Elem( iel ) % Ptr )
  Obj % Elem( iel ) % Ptr => NULL( )
  DO j = iel, Obj % tElements-1
    Obj % Elem( j ) % Ptr => Obj % Elem( j + 1 ) % Ptr
  END DO
  Obj % Elem( Obj % tElements ) % Ptr => NULL( )
  Obj % tElements = Obj % tElements - 1
  END SELECT
END PROCEDURE remove_element

!----------------------------------------------------------------------------
!                                                             total_elements
!----------------------------------------------------------------------------

MODULE PROCEDURE total_elements
  Ans = Obj % tElements
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
  DO iel =1, Obj % tElements
    Elem => Obj % ElementPointer( iel )
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
  tElements = Obj % tElements
  MaxNptrs = 0
  Elem => NULL( )
  !
  ! Find the largest nptrs
  DO iel = 1, Obj % tElements
    Elem => Obj % Elem( iel  ) % Ptr
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
    Elem => Obj % Elem( iel  ) % Ptr
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
  DO imesh = 1, SIZE( Obj )
    IF( ASSOCIATED( Obj( imesh ) % Ptr ) ) THEN
      DO iel = 1, Obj( imesh ) % Ptr % tElements
        Elem => Obj( imesh ) % Ptr % Elem( iel  ) % Ptr
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
  DO imesh = 1, SIZE( Obj )
    IF( ASSOCIATED( Obj( imesh ) % Ptr ) ) THEN
      DO iel = 1, Obj( imesh ) % Ptr % tElements
        Elem => Obj( imesh ) % Ptr % Elem( iel  ) % Ptr
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
  DO iel = 1, Obj % tElements
    Elem => Obj % Elem( iel ) % Ptr
    IF( .NOT. ASSOCIATED( Elem ) ) EXIT
    CALL Elem % setMaterialType( MatType )
  END DO
END PROCEDURE setMaterialType_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE Methods
