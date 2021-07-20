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
!                                                                 Import
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_Import
  CHARACTER( LEN = * ), PARAMETER :: myName="mesh_Import"
  TYPE( String ) :: dsetname
  CLASS( ReferenceElement_ ), POINTER :: refelem
  INTEGER( I4B ) :: elemOrder, ii

  CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
    & 'Making dsetname')

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

  CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
    & 'dsetname = '//TRIM(dsetname) )

  !>check
  IF( .NOT. meshFile%isOpen() ) THEN
    CALL eMesh%raiseError(modName//'::'//myName// " - "// &
      & 'HDF5 file is not opened')
  END IF

  !>check
  IF( .NOT. meshFile%isRead() ) THEN
    CALL eMesh%raiseError(modName//'::'//myName// " - "// &
      & 'HDF5 file does not have read permission')
  END IF

  !>check
  IF( .NOT. meshFile%isGroup(TRIM(dsetname%chars())) ) THEN
    CALL eMesh%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // ' is not a group; it should be a group which contains the meshEntity' )
  END IF

  !>check
  IF( .NOT. meshFile%pathExists(TRIM(dsetname%chars())) ) THEN
    CALL eMesh%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // ' path does not exists' )
  END IF

  !> read Uid
  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/uid") ) THEN
    CALL eMesh%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/uid" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/uid", obj%Uid )
    CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'uid = ' // TRIM(str(obj%Uid, .true.)) )
  END IF

  !> read xidim
  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/xidim") ) THEN
    CALL eMesh%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/xidim" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/xidim", obj%xidim )
    CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'xidim = ' // TRIM(str(obj%xidim, .true.)) )
  END IF

  !>reading elemtype
  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/elemType") ) THEN
    CALL eMesh%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/elemType" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/elemType", obj%elemType )
    CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'elemType = ' // TRIM(str(obj%elemType, .true.)) )
  END IF

  !>reading minX
  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/minX") ) THEN
    CALL eMesh%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/minX" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/minX", obj%minX )
    CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'minX = ' // TRIM(str(obj%minX, .true.)) )
  END IF

  !> reading minY
  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/minY") ) THEN
    CALL eMesh%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/minY" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/minY", obj%minY )
    CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'minY = ' // TRIM(str(obj%minY, .true.)) )
  END IF

  !> reading minZ
  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/minZ") ) THEN
    CALL eMesh%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/minZ" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/minZ", obj%minZ )
    CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'minZ = ' // TRIM(str(obj%minZ, .true.)) )
  END IF

  !> reading maxX
  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/maxX") ) THEN
    CALL eMesh%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/maxX" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/maxX", obj%maxX )
    CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'maxX = ' // TRIM(str(obj%maxX, .true.)) )
  END IF

  !> reading maxY
  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/maxY") ) THEN
    CALL eMesh%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/maxY" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/maxY", obj%maxY )
    CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'maxY = ' // TRIM(str(obj%maxY, .true.)) )
  END IF

    !> reading maxZ
  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/maxZ") ) THEN
    CALL eMesh%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/maxZ" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/maxZ", obj%minZ )
    CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'maxZ = ' // TRIM(str(obj%maxZ, .true.)) )
  END IF

    !> reading x
  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/x") ) THEN
    CALL eMesh%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/x" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/x", obj%x )
    CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'x = ' // TRIM(str(obj%x, .true.)) )
  END IF

    !> reading y
  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/y") ) THEN
    CALL eMesh%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/y" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/y", obj%y )
    CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'y = ' // TRIM(str(obj%y, .true.)) )
  END IF

    !> reading z
  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/z") ) THEN
    CALL eMesh%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/z" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/z", obj%z )
    CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'z = ' // TRIM(str(obj%z, .true.)) )
  END IF

    !> reading tElements
  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/tElements") ) THEN
    CALL eMesh%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/tElements" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/tElements", &
      & obj%tElements )
    CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'tElements = ' // TRIM(str(obj%tElements, .true.)) )
  END IF

    !> reading tIntNodes
  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/tIntNodes") ) THEN
    CALL eMesh%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/tIntNodes" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/tIntNodes", &
      & obj%tIntNodes )
    CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'tIntNodes = ' // TRIM(str(obj%tIntNodes, .true.)) )
  END IF

  !> reading nodeCoord
  CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'reading nodeCoord' )
  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/nodeCoord") ) THEN
    CALL eMesh%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/nodeCoord" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/nodeCoord", &
      & obj%nodeCoord )
    IF( eMesh%isLogActive() ) THEN
      CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
        & 'nodal coordinates are given below = ' )
      CALL DISP( title="x", x=obj%nodeCoord( 1, : ), &
        & unit = eMesh%getLogFileUnit(), style="UNDERLINE", &
        & advance="NO" )
      CALL DISP( title="y", x=obj%nodeCoord( 2, : ), &
        & unit = eMesh%getLogFileUnit(), style="UNDERLINE", &
        & advance="NO" )
      CALL DISP( title="z", x=obj%nodeCoord( 3, : ), &
        & unit = eMesh%getLogFileUnit(), style="UNDERLINE", &
        & advance="DOUBLE" )
    END IF
  END IF

  !> reading physicalTag
  CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
    & 'reading physicalTag' )
  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/physicalTag") ) THEN
    CALL eMesh%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/physicalTag" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/physicalTag", &
      & obj%physicalTag )
    IF( eMesh%isLogActive() ) THEN
      CALL eMesh%raiseInformation(modName//'::'//myName// " - " )
      CALL Display( obj%physicalTag, "physicalTag = ", &
        & unitNo = eMesh%getLogFileUnit() )
    END IF
  END IF

  !> reading intNodeNumber
  CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
    & 'reading InternalNptrs' )
  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/intNodeNumber") ) THEN
    CALL eMesh%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/intNodeNumber" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/intNodeNumber", &
      & obj%InternalNptrs )
    IF( eMesh%isLogActive() ) THEN
      CALL eMesh%raiseInformation(modName//'::'//myName// " - " )
      CALL Display( obj%InternalNptrs, "InternalNptrs = ", &
        & unitNo = eMesh%getLogFileUnit() )
    END IF
  END IF

  !> reading elemNumber
  CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'reading elemNumber' )
  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/elemNumber") ) THEN
    CALL eMesh%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/elemNumber" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/elemNumber", &
      & obj%elemNumber )
    IF( eMesh%isLogActive() ) THEN
      CALL eMesh%raiseInformation(modName//'::'//myName// " - " )
      CALL Display( obj%elemNumber, "elemNumber = ", &
        & unitNo = eMesh%getLogFileUnit() )
    END IF
  END IF

  !> reading connectivity
  CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
    & 'reading connectivity' )
  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/connectivity") ) THEN
    CALL eMesh%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/connectivity" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/connectivity", &
      & obj%connectivity )
    IF( eMesh%isLogActive() ) THEN
      CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
        & 'connectivity(1:NNS, 1:tElements) is given below = ' )
      DO ii = 1, SIZE( obj%connectivity, 1)
        CALL DISP( title="Node-"//trim(str(ii, .true.)), &
          & x=obj%connectivity( ii, : ), &
          & unit = eMesh%getLogFileUnit(), style="UNDERLINE", &
          & advance="NO" )
      END DO
      CALL DISP( title='', x='', unit = eMesh%getLogFileUnit(), &
        & advance="DOUBLE" )
    END IF
  END IF

  !> reading boundingEntity
  CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
    & 'reading boundingEntity' )
  IF( meshFile%pathExists(TRIM(dsetname) // "/boundingEntity") ) THEN
    CALL meshFile%read( TRIM(dsetname) // "/boundingEntity", &
      & obj%boundingEntity )
    IF( eMesh%isLogActive() ) THEN
      CALL eMesh%raiseInformation(modName//'::'//myName// " - " )
      CALL Display( obj%boundingEntity, "boundingEntity = ", &
        & unitNo = eMesh%getLogFileUnit() )
    END IF
  END IF

  ! Working on reference element
  CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
    & 'adding reference element' )

  SELECT CASE( obj%xidim )
  CASE( 0 )
    CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'reference element = referencePoint' )
    obj%refelem => ReferencePoint_Pointer(nsd=obj%nsd)
    CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'referencePoint element added' )

  CASE( 1 )
    CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'reference element = referenceLine' )
    elemOrder = ElementOrder( obj%elemType )
    IF( elemOrder .NE. 1 ) THEN
      refelem => ReferenceLine_Pointer(nsd=obj%nsd)
      ALLOCATE( ReferenceLine_ :: obj%refelem )
      CALL refelem%LagrangeElement( order=elemOrder, &
        & HighOrderObj=obj%refelem )
      CALL DeallocateData( refelem )
      DEALLOCATE( refelem )
      refelem => NULL()
    ELSE
      obj%refelem => ReferenceLine_Pointer(nsd=obj%nsd)
    END IF
    CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'referenceLine element added' )

  CASE( 2 )
    elemOrder = ElementOrder( obj%elemType )

    IF( isTriangle( obj%elemType ) ) THEN
      CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
        & 'reference element = referenceTriangle' )
      IF( elemOrder .NE. 1 ) THEN
        refelem => ReferenceTriangle_Pointer(nsd=obj%nsd)
        ALLOCATE( ReferenceTriangle_ :: obj%refelem )
        CALL refelem%LagrangeElement( order=elemOrder, &
          & HighOrderObj=obj%refelem )
        CALL DeallocateData( refelem )
        DEALLOCATE( refelem )
        refelem => NULL()
      ELSE
        obj%refelem => ReferenceTriangle_Pointer(nsd=obj%nsd)
      END IF
      CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
        & 'referenceTriangle element added' )

    ELSE IF( isQuadrangle( obj%elemType ) ) THEN
      CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'reference element = referenceQuadrangle' )
      IF( elemOrder .NE. 1 ) THEN
        refelem => ReferenceQuadrangle_Pointer(nsd=obj%nsd)
        ALLOCATE( ReferenceQuadrangle_ :: obj%refelem )
        CALL refelem%LagrangeElement( order=elemOrder, &
          & HighOrderObj=obj%refelem )
        CALL DeallocateData( refelem )
        DEALLOCATE( refelem )
        refelem => NULL()
      ELSE
        obj%refelem => ReferenceQuadrangle_Pointer(nsd=obj%nsd)
      END IF
      CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'referenceQuadrangle element added' )
    END IF

  CASE( 3 )
    elemOrder = ElementOrder( obj%elemType )

    IF( isTetrahedron( obj%elemType ) ) THEN
      CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'reference element = referenceTetrahedron' )
      IF( elemOrder .NE. 1 ) THEN
        refelem => ReferenceTetrahedron_Pointer(nsd=obj%nsd)
        ALLOCATE( ReferenceTetrahedron_ :: obj%refelem )
        CALL refelem%LagrangeElement( order=elemOrder, &
          & HighOrderObj=obj%refelem )
        CALL DeallocateData( refelem )
        DEALLOCATE( refelem )
        refelem => NULL()
      ELSE
        obj%refelem => ReferenceTetrahedron_Pointer(nsd=obj%nsd)
      END IF
      CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
        & 'referenceTetrahedron element added' )

    ELSE IF( isHexahedron( obj%elemType ) ) THEN
      CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'reference element = referenceHexahedron' )
      IF( elemOrder .NE. 1 ) THEN
        refelem => ReferenceHexahedron_Pointer(nsd=obj%nsd)
        ALLOCATE( ReferenceHexahedron_ :: obj%refelem )
        CALL refelem%LagrangeElement( order=elemOrder, &
          & HighOrderObj=obj%refelem )
        CALL DeallocateData( refelem )
        DEALLOCATE( refelem )
        refelem => NULL()
      ELSE
        obj%refelem => ReferenceHexahedron_Pointer(nsd=obj%nsd)
      END IF
      CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'referenceTetrahedron element added' )

    ELSE IF( isPrism( obj%elemType ) ) THEN
      CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'reference element = referencePrism' )
      IF( elemOrder .NE. 1 ) THEN
        refelem => ReferencePrism_Pointer(nsd=obj%nsd)
        ALLOCATE( ReferencePrism_ :: obj%refelem )
        CALL refelem%LagrangeElement( order=elemOrder, &
          & HighOrderObj=obj%refelem )
        CALL DeallocateData( refelem )
        DEALLOCATE( refelem )
        refelem => NULL()
      ELSE
        obj%refelem => ReferencePrism_Pointer(nsd=obj%nsd)
      END IF
      CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'referencePrism element added' )

    ELSE IF( isPyramid( obj%elemType ) ) THEN
      CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'reference element = referencePyramid' )
      IF( elemOrder .NE. 1 ) THEN
        refelem => ReferencePyramid_Pointer(nsd=obj%nsd)
        ALLOCATE( ReferencePyramid_ :: obj%refelem )
        CALL refelem%LagrangeElement( order=elemOrder, &
          & HighOrderObj=obj%refelem )
        CALL DeallocateData( refelem )
        DEALLOCATE( refelem )
        refelem => NULL()
      ELSE
        obj%refelem => ReferencePyramid_Pointer(nsd=obj%nsd)
      END IF
      CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'referencePyramid element added' )
    END IF
  END SELECT
END PROCEDURE mesh_Import

!----------------------------------------------------------------------------
!                                                                    Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_initiate
  CHARACTER( LEN = * ), PARAMETER :: myName="mesh_initiate"
  CLASS( ExceptionHandler_ ), POINTER :: surr
  LOGICAL( LGT ) :: exist, opened

  !> setting up exception messages output settings
  surr => NULL()
  CALL eMesh%getSurrogate(surr)
  IF( .NOT. ASSOCIATED( surr ) ) THEN
    CALL eMesh%setQuietMode( .TRUE. )
    CALL eMesh%setStopOnError( .TRUE. )
    INQUIRE(file=eLogFile, exist=exist, opened=opened )
    IF( exist ) THEN
      IF( .NOT. opened ) OPEN( Unit=eUnitNo, FILE=eLogFile, &
        & POSITION='APPEND',STATUS='OLD', &
        & ACTION="WRITE" )
      CALL eMesh%setLogFileUnit( eUnitNo )
      CALL eMesh%setLogActive( .TRUE. )
    ELSE
      OPEN( Unit=eUnitNo, FILE=eLogFile, &
        & ACCESS='SEQUENTIAL',FORM='FORMATTED',STATUS='REPLACE' )
      CALL eMesh%setLogFileUnit( eUnitNo )
      CALL eMesh%setLogActive( .TRUE. )
    END IF
  END IF
  surr => NULL()

  ! CALL obj%list%Initiate()
  ! CALL obj%list%Reserve(obj%tElements)
  obj%readFromFile = .TRUE.
  obj%isInitiated = .TRUE.

  CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
    & 'Importing mesh' )
  CALL obj%Import(meshFile, xidim, id)
  CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
    & 'Mesh imported' )

  IF( obj%elemType .NE. Point1 .AND. obj%elemType .NE. 0 ) THEN
    CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'Initiating Local Nptrs' )
    CALL obj%InitiateLocalNptrs()
    CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'Local Nptrs Initiated' )

    CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'Initiating Local Element Numbers' )
    CALL obj%InitiateLocalElementNumbers()
    CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'Local Element Numbers initiated' )

    CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'Initiating node to elements mapping' )
    CALL obj%InitiateNodeToElements()
    CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'Node to elements mapping initiated' )

    CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'Initiating node to nodes mapping' )
    CALL obj%InitiateNodeToNodes()
    CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'Node to nodes mapping initiated' )

    CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'Initiating element to elements mapping' )
    CALL obj%InitiateElementToElements()
    CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'Element to elements mapping initiated' )

    CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'Initiating boundary data' )
    CALL obj%InitiateBoundaryData()
    CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'Boundary data initiated' )
  END IF

END PROCEDURE mesh_initiate

!----------------------------------------------------------------------------
!                                                                      Mesh
!----------------------------------------------------------------------------

MODULE PROCEDURE Mesh_Constructor1
  CALL ans%Initiate(meshFile, xidim, id)
END PROCEDURE Mesh_Constructor1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Mesh_Constructor_1
  ALLOCATE(Mesh_::ans)
  CALL ans%Initiate(meshfile, xidim, id)
END PROCEDURE Mesh_Constructor_1

!----------------------------------------------------------------------------
!                                                            DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_DeallocateData
  obj%readFromFile = .FALSE.
  obj%uid = 0
  obj%xidim = 0
  obj%elemType = 0
  obj%nsd = 0
  obj%maxNptrs = 0
  obj%minNptrs = 0
  obj%maxElemNum = 0
  obj%minElemNum = 0
  obj%tNodes = 0
  obj%tIntNodes = 0
  obj%minX = 0.0_DFP
  obj%maxX = 0.0_DFP
  obj%minY = 0.0_DFP
  obj%maxY = 0.0_DFP
  obj%minZ = 0.0_DFP
  obj%maxZ = 0.0_DFP
  obj%X=0.0_DFP
  obj%Y=0.0_DFP
  obj%Z=0.0_DFP
  obj%isInitiated = .FALSE.
  obj%refelem => NULL()
  IF( ALLOCATED( obj%FacetElements ) ) DEALLOCATE( obj%FacetElements )
  IF( ALLOCATED( obj%local_elemNumber ) ) DEALLOCATE( obj%local_elemNumber )
  IF( ALLOCATED( obj%nodeCoord ) ) DEALLOCATE( obj%nodeCoord )
  IF( ALLOCATED( obj%nodeVelocity ) ) DEALLOCATE( obj%nodeVelocity )
  IF( ALLOCATED( obj%nodeAcc ) ) DEALLOCATE( obj%nodeAcc )
  IF( ALLOCATED( obj%physicalTag ) ) DEALLOCATE( obj%physicalTag )
  IF( ALLOCATED( obj%elemNumber ) ) DEALLOCATE( obj%elemNumber )
  IF( ALLOCATED( obj%boundingEntity ) ) DEALLOCATE( obj%boundingEntity )
  IF( ALLOCATED( obj%connectivity ) ) DEALLOCATE( obj%connectivity )
  IF( ALLOCATED( obj%LBndyIndex ) ) DEALLOCATE( obj%LBndyIndex )
  IF( ALLOCATED( obj%Nptrs ) ) DEALLOCATE( obj%Nptrs )
  IF( ALLOCATED( obj%Local_Nptrs ) ) DEALLOCATE( obj%Local_Nptrs )
  IF( ALLOCATED( obj%BoundaryNptrs ) ) DEALLOCATE( obj%BoundaryNptrs )
  IF( ALLOCATED( obj%InternalNptrs ) ) DEALLOCATE( obj%InternalNptrs )
  IF( ALLOCATED( obj%NodeToElem ) ) DEALLOCATE( obj%NodeToElem )
  IF( ALLOCATED( obj%ElemToElem ) ) DEALLOCATE( obj%ElemToElem )
  IF( ALLOCATED( obj%NTN ) ) DEALLOCATE( obj%NTN )
  IF( ALLOCATED( obj%BoundaryData ) ) DEALLOCATE( obj%BoundaryData )
  IF( ALLOCATED( obj%InternalBndyElemNum ) ) DEALLOCATE( obj%InternalBndyElemNum )
  IF( ALLOCATED( obj%InternalBoundaryData ) ) DEALLOCATE( obj%InternalBoundaryData )
  CALL eMesh%reset()
END PROCEDURE mesh_DeallocateData

!----------------------------------------------------------------------------
!                                                                    Final
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_final
  CALL obj%DeallocateData()
END PROCEDURE mesh_final

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_display
  CHARACTER( LEN = * ), PARAMETER :: myName = "mesh_display"
  CALL eMesh%raiseError(modName//"::"//myName//" - "// &
      & "This routine has not been implemented yet.")
END PROCEDURE mesh_display

!----------------------------------------------------------------------------
!                                                                      SIZE
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_size
  ans = obj%tElements
END PROCEDURE mesh_size

!----------------------------------------------------------------------------
!                                                         getBoundingEntity
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getBoundingEntity
  IF( ALLOCATED( obj%boundingEntity ) ) THEN
    ans = obj%boundingEntity
  ELSE
    ALLOCATE( ans(0) )
  END IF
END PROCEDURE mesh_getBoundingEntity

!----------------------------------------------------------------------------
!                                                             getNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getNodeCoord_1
  IF( obj%isNodePresent( GlobalNode ) ) THEN
    ans = obj%nodeCoord( 1:3, obj%getLocalNptrs( GlobalNode ) )
  ELSE
    ans = 0.0_DFP
  END IF
END PROCEDURE mesh_getNodeCoord_1

!----------------------------------------------------------------------------
!                                                             getNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getNodeCoord_2
  INTEGER( I4B ) :: ii
  DO ii = 1, SIZE( GlobalNode )
    ans( 1:3, ii ) = mesh_getNodeCoord_1( obj, GlobalNode( ii ) )
  END DO
END PROCEDURE mesh_getNodeCoord_2

!----------------------------------------------------------------------------
!                                                             getNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getNodeCoord_3
  INTEGER( I4B ) :: ii
  IF( ALLOCATED( obj%nodeCoord ) ) THEN
    ans = obj%nodeCoord
  ELSE
    ALLOCATE( ans( 0, 0 ) )
  END IF
END PROCEDURE mesh_getNodeCoord_3

!----------------------------------------------------------------------------
!                                                              setNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_setNodeCoord_1
  INTEGER( I4B ) :: ii
  IF( obj%isNodePresent( GlobalNode ) ) THEN
    ii = obj%getLocalNptrs( GlobalNode )
    obj%nodeCoord( 1:3, ii ) = obj%nodeCoord( 1:3, ii ) + nodeCoord(1:3)
  END IF
END PROCEDURE mesh_setNodeCoord_1

!----------------------------------------------------------------------------
!                                                              setNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_setNodeCoord_2
  INTEGER( I4B ) :: ii
  DO ii = 1, SIZE( GlobalNode )
    CALL mesh_setNodeCoord_1(obj, GlobalNode=GlobalNode( ii ), &
      & nodeCoord=nodeCoord( 1:3, ii ) )
  END DO
END PROCEDURE mesh_setNodeCoord_2

!----------------------------------------------------------------------------
!                                                              setNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_setNodeCoord_3
  obj%nodeCoord = nodeCoord
END PROCEDURE mesh_setNodeCoord_3

!----------------------------------------------------------------------------
!                                                               getNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getNptrs
  IF( ALLOCATED( obj%nptrs ) ) THEN
    ans = obj%nptrs
  ELSE
    ALLOCATE( ans( 0, 0 ) )
  END IF
END PROCEDURE mesh_getNptrs

END SUBMODULE MeshMethods
