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

SUBMODULE( Mesh_Class ) MeshMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_Read
  CHARACTER( LEN = * ), PARAMETER :: myName="mesh_Read"
  TYPE( String ) :: dsetname


  SELECT CASE( xidim )
  CASE( 0 )
    dsetname = "pointEntities_"//TRIM(str(id, .true.) )
  CASE( 1 )
    dsetname = "curveEntities_"//TRIM(str(id, .true.) )
  CASE( 2 )
    dsetname = "surfaceEntities_"//TRIM(str(id, .true.) )
  CASE( 3 )
    dsetname = "volumeEntities_"//TRIM(str(id, .true.) )
  END SELECT

  IF( .NOT. meshFile%isOpen() ) THEN
    CALL obj%e%raiseError(modName//'::'//myName// &
      & 'HDF5 file is not opened')
  END IF

  IF( .NOT. meshFile%isRead() ) THEN
    CALL obj%e%raiseError(modName//'::'//myName// &
      & 'HDF5 file does not have read permission')
  END IF

  IF( .NOT. meshFile%isGroup(TRIM(dsetname%chars())) ) THEN
    CALL obj%e%raiseError(modName//'::'//myName// &
      & TRIM(dsetname) // ' is not a group; it should be a group which contains the meshEntity' )
  END IF

  IF( .NOT. meshFile%pathExists(TRIM(dsetname%chars())) ) THEN
    CALL obj%e%raiseError(modName//'::'//myName// &
      & TRIM(dsetname) // ' path does not exists' )
  END IF

  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/uid") ) THEN
    CALL obj%e%raiseError(modName//'::'//myName// &
      & TRIM(dsetname) // "/uid" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/uid", obj%meshData%Uid )
  END IF

  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/xidim") ) THEN
    CALL obj%e%raiseError(modName//'::'//myName// &
      & TRIM(dsetname) // "/xidim" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/xidim", obj%meshData%xidim )
  END IF

  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/elemType") ) THEN
    CALL obj%e%raiseError(modName//'::'//myName// &
      & TRIM(dsetname) // "/elemType" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/elemType", obj%meshData%elemType )
  END IF

  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/minX") ) THEN
    CALL obj%e%raiseError(modName//'::'//myName// &
      & TRIM(dsetname) // "/minX" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/minX", obj%meshData%minX )
  END IF

  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/minY") ) THEN
    CALL obj%e%raiseError(modName//'::'//myName// &
      & TRIM(dsetname) // "/minY" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/minY", obj%meshData%minY )
  END IF

  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/minZ") ) THEN
    CALL obj%e%raiseError(modName//'::'//myName// &
      & TRIM(dsetname) // "/minZ" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/minZ", obj%meshData%minZ )
  END IF

  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/maxX") ) THEN
    CALL obj%e%raiseError(modName//'::'//myName// &
      & TRIM(dsetname) // "/maxX" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/maxX", obj%meshData%maxX )
  END IF

  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/maxY") ) THEN
    CALL obj%e%raiseError(modName//'::'//myName// &
      & TRIM(dsetname) // "/maxY" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/maxY", obj%meshData%maxY )
  END IF

  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/maxZ") ) THEN
    CALL obj%e%raiseError(modName//'::'//myName// &
      & TRIM(dsetname) // "/maxZ" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/maxZ", obj%meshData%minZ )
  END IF

  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/x") ) THEN
    CALL obj%e%raiseError(modName//'::'//myName// &
      & TRIM(dsetname) // "/x" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/x", obj%meshData%x )
  END IF

  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/y") ) THEN
    CALL obj%e%raiseError(modName//'::'//myName// &
      & TRIM(dsetname) // "/y" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/y", obj%meshData%y )
  END IF

  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/z") ) THEN
    CALL obj%e%raiseError(modName//'::'//myName// &
      & TRIM(dsetname) // "/z" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/z", obj%meshData%z )
  END IF

  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/nodeCoord") ) THEN
    CALL obj%e%raiseError(modName//'::'//myName// &
      & TRIM(dsetname) // "/nodeCoord" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/nodeCoord", &
      & obj%meshData%nodeCoord )
  END IF

  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/physicalTag") ) THEN
    CALL obj%e%raiseError(modName//'::'//myName// &
      & TRIM(dsetname) // "/physicalTag" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/physicalTag", &
      & obj%meshData%physicalTag )
  END IF

  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/intNodeNumber") ) THEN
    CALL obj%e%raiseError(modName//'::'//myName// &
      & TRIM(dsetname) // "/intNodeNumber" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/intNodeNumber", &
      & obj%meshData%InternalNptrs )
  END IF

  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/elemNumber") ) THEN
    CALL obj%e%raiseError(modName//'::'//myName// &
      & TRIM(dsetname) // "/elemNumber" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/elemNumber", &
      & obj%meshData%elemNumber )
  END IF

  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/connectivity") ) THEN
    CALL obj%e%raiseError(modName//'::'//myName// &
      & TRIM(dsetname) // "/connectivity" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/connectivity", &
      & obj%meshData%connectivity )
  END IF

  IF( meshFile%pathExists(TRIM(dsetname) // "/boundingEntity") ) THEN
    CALL meshFile%read( TRIM(dsetname) // "/boundingEntity", &
      & obj%meshData%boundingEntity )
  END IF

  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/tElements") ) THEN
    CALL obj%e%raiseError(modName//'::'//myName// &
      & TRIM(dsetname) // "/tElements" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/tElements", &
      & obj%meshData%tElements )
  END IF

  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/tIntNodes") ) THEN
    CALL obj%e%raiseError(modName//'::'//myName// &
      & TRIM(dsetname) // "/tIntNodes" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/tIntNodes", &
      & obj%meshData%tIntNodes )
  END IF
END PROCEDURE mesh_Read

!----------------------------------------------------------------------------
!                                                                    Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_initiate
  CALL obj%read(meshFile, xidim, id)
  CALL obj%list%Initiate()
  CALL obj%list%Reserve(obj%meshData%tElements)
  obj%readFromFile = .TRUE.
END PROCEDURE mesh_initiate

!----------------------------------------------------------------------------
!                                                                      Mesh
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor1
  CALL ans%Initiate(meshFile, xidim, id)
END PROCEDURE Constructor1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor_1
  ALLOCATE(Mesh_::ans)
  CALL ans%Initiate(meshfile, xidim, id)
END PROCEDURE Constructor_1

!----------------------------------------------------------------------------
!                                                                    Final
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_final
  CALL obj%DeallocateData()
END PROCEDURE mesh_final

!----------------------------------------------------------------------------
!                                                            DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_DeallocateData
  CALL obj%list%DeallocateData()
  obj%meshData%nsd = 0
END PROCEDURE mesh_DeallocateData

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_display
  ! define internal variable
  INTEGER( I4B ) :: I
  I = Input( default=stdout, option=unitno )
  IF( LEN_TRIM( msg ) .NE. 0 ) WRITE( I, "(A)" ) TRIM( msg )
  CALL obj%list%Display( msg="Element",  unitNo = I )
END PROCEDURE mesh_display

!----------------------------------------------------------------------------
!                                                                    SetSize
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_PruneMesh
  ! define internal variables
  INTEGER( I4B ) :: tElements, iel
  TYPE( ElementPointerIterator_ ) :: iterator
  CHARACTER( LEN=* ), PARAMETER :: myName="mesh_PruneMesh"

  IF( obj%list%isEmpty() ) THEN
    CALL obj%e%raiseError(modName//"::"//myName//" - "// &
      & "mesh is empty")
  ELSE
    iterator = obj%list%Begin()
    DO iel = 1, obj%list%size()
      IF( .NOT. ASSOCIATED(iterator%value%ptr) ) THEN
        CALL obj%list%Erase(pos=iterator)
      END IF
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
  TYPE( ElementPointer_ ) :: val
  val%ptr => Elem
  CALL obj%list%set( pos=iel, val=val )
END PROCEDURE mesh_SetElement

!----------------------------------------------------------------------------
!                                                            ElementPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getElementPointer
  TYPE( ElementPointerIterator_ ) :: iterator
  CHARACTER( LEN = * ), PARAMETER :: myName = "mesh_getElementPointer"

  IF( iel .GT. obj%size() ) THEN
    CALL obj%e%raiseError(modName//"::"//myName//" - "// &
      & "iel is greater thant the size of the mesh.")
  ELSE
    iterator = obj%list%begin()
    iterator = iterator + (iel-1)
    ans => iterator%value%ptr
  END IF
END PROCEDURE mesh_getElementPointer

!----------------------------------------------------------------------------
!                                                                      SIZE
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_size
  ans = obj%list%size()
END PROCEDURE mesh_size

!----------------------------------------------------------------------------
!                                                             RemoveElement
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_RemoveElement
  CALL obj%list%Erase( pos=iel, freeMem=freeMem )
END PROCEDURE mesh_RemoveElement

! !----------------------------------------------------------------------------
! !                                                                    getNptrs
! !----------------------------------------------------------------------------

! MODULE PROCEDURE mesh_getNptrs
!   ! Define internal variables
!   INTEGER( I4B ) :: ii, Dummy, MaxNptrs, tElements
!   INTEGER( I4B ), ALLOCATABLE :: Nptrs0( : ), DummyNptrs( : )
!   TYPE( ElementPointerIterator_ ) :: iter

!   IF( ALLOCATED( Nptrs ) ) DEALLOCATE( Nptrs )
!   tElements = obj%list%size()
!   MaxNptrs = 0
!   iter = obj%list%begin()
!   ! Find the largest nptrs
!   DO ii = 1, tElements
!     IF( ASSOCIATED( iter%value%ptr ) ) THEN
!       Dummy = MAXVAL( iter%value%ptr%getNptrs() )
!       IF( Dummy .GE. MaxNptrs ) MaxNptrs = Dummy
!     END IF
!     CALL iter%inc()
!   END DO

!   ALLOCATE( DummyNptrs( MaxNptrs ) ); DummyNptrs = 0

!   iter = obj%list%begin()
!   DO ii = 1, tElements
!     IF( ASSOCIATED( iter%value%ptr ) ) THEN
!       Nptrs0 = iter%value%ptr%getNptrs()
!       DummyNptrs( Nptrs0 ) = Nptrs0
!     END IF
!   END DO

!   Dummy = COUNT( DummyNptrs .NE. 0 )
!   ALLOCATE( Nptrs( Dummy ) ); Dummy = 0

!   DO ii = 1, MaxNptrs
!     IF( DummyNptrs( ii ) .EQ. 0 ) CYCLE
!     Dummy = Dummy + 1
!     Nptrs( Dummy ) = DummyNptrs( iel )
!   END DO
!   !
!   DEALLOCATE( Nptrs0, DummyNptrs )
!   NULLIFY( Elem )
! END PROCEDURE mesh_getNptrs

! !----------------------------------------------------------------------------
! !                                                                   getNptrs
! !----------------------------------------------------------------------------

! MODULE PROCEDURE meshPointer_getNptrs
!   ! ! Define internal variables
!   ! INTEGER( I4B ) :: iel, Dummy, MaxNptrs, imesh
!   ! INTEGER( I4B ), ALLOCATABLE :: Nptrs0( : ), DummyNptrs( : )
!   ! CLASS( Element_ ), POINTER :: Elem


!   ! IF( ALLOCATED( Nptrs ) ) DEALLOCATE( Nptrs )
!   ! MaxNptrs = 0
!   ! Elem => NULL( )
!   ! !
!   ! ! Find the largest nptrs
!   ! DO imesh = 1, SIZE( obj )
!   !   IF( ASSOCIATED( obj( imesh )%Ptr ) ) THEN
!   !     DO iel = 1, obj( imesh )%Ptr%tElements
!   !       Elem => obj( imesh )%Ptr%Elem( iel  )%Ptr
!   !       IF( .NOT. ASSOCIATED( Elem ) ) EXIT
!   !       Nptrs0 = .Nptrs. Elem
!   !       Dummy = MAXVAL( Nptrs0 )
!   !       IF( Dummy .GE. MaxNptrs ) MaxNptrs = Dummy
!   !     END DO
!   !   END IF
!   ! END DO
!   ! !
!   ! ALLOCATE( DummyNptrs( MaxNptrs ) )
!   ! DummyNptrs = 0
!   ! !
!   ! DO imesh = 1, SIZE( obj )
!   !   IF( ASSOCIATED( obj( imesh )%Ptr ) ) THEN
!   !     DO iel = 1, obj( imesh )%Ptr%tElements
!   !       Elem => obj( imesh )%Ptr%Elem( iel  )%Ptr
!   !       IF( .NOT. ASSOCIATED( Elem ) ) EXIT
!   !       Nptrs0 = .Nptrs. Elem
!   !       DummyNptrs( Nptrs0 ) = Nptrs0
!   !     END DO
!   !   END IF
!   ! END DO
!   ! !
!   ! Dummy = COUNT( DummyNptrs .NE. 0 )
!   ! ALLOCATE( Nptrs( Dummy ) )
!   ! !
!   ! Dummy = 0
!   ! DO iel = 1, MaxNptrs
!   !   IF( DummyNptrs( iel ) .EQ. 0 ) CYCLE
!   !   Dummy = Dummy + 1
!   !   Nptrs( Dummy ) = DummyNptrs( iel )
!   ! END DO
!   ! !
!   ! DEALLOCATE( Nptrs0, DummyNptrs )
!   ! NULLIFY( Elem )
! END PROCEDURE meshPointer_getNptrs

! !----------------------------------------------------------------------------
! !
! !----------------------------------------------------------------------------

! MODULE PROCEDURE mesh_setMaterialType
!   ! INTEGER( I4B ) :: iel
!   ! CLASS( Element_ ), POINTER :: Elem

!   ! Elem => NULL( )
!   ! DO iel = 1, obj%tElements
!   !   Elem => obj%Elem( iel )%Ptr
!   !   IF( .NOT. ASSOCIATED( Elem ) ) EXIT
!   !   CALL Elem%setMaterialType( MatType )
!   ! END DO
! END PROCEDURE mesh_setMaterialType

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_GenerateMeshData
  CHARACTER( LEN = * ), PARAMETER :: myName = "mesh_GenerateMeshData"
  IF( obj%readFromFile ) THEN
    CALL GenerateMeshDataForReadFromFile(obj)
  ELSE
    CALL obj%e%raiseError(modName//"::"//myName//" - "// &
      & "Currently mesh data can be generated when mesh is read from the file")
  END IF
END PROCEDURE mesh_GenerateMeshData

!----------------------------------------------------------------------------
!                                                           GenerateMeshData
!----------------------------------------------------------------------------

SUBROUTINE GenerateMeshDataForReadFromFile(obj)
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
  ! Internal variables
  obj%meshData%isInitiated = .TRUE.
  CALL obj%meshData%InitiateLocalNptrs()
  CALL obj%meshData%InitiateLocalElementNumbers()
END SUBROUTINE GenerateMeshDataForReadFromFile


END SUBMODULE MeshMethods
