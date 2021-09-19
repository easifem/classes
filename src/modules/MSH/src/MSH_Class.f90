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

MODULE MSH_Class
USE BaseType
USE GlobalData
USE TxtFile_Class
USE ExceptionHandler_Class
USE mshFormat_Class
USE mshPhysicalNames_Class
USE mshEntity_Class
USE mshNodes_Class
USE mshElements_Class
! USE Mesh_Class
! USE FE
! USE Domain_Class
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName = "MSH_CLASS"
TYPE( ExceptionHandler_ ) :: e

!----------------------------------------------------------------------------
!                                                                       MSH_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	11 June 2021
! summary: 	This class handles the mesh generation by gmsh

TYPE :: MSH_
  PRIVATE
  TYPE( String ), POINTER :: buffer => NULL()
    !! buffer to recoord coommands
  INTEGER( I4B ) :: nsd = 0
    !! Spatial dimension
  TYPE( TxtFile_ ) :: mshFile
    !! mesh file
  TYPE( mshFormat_ ) :: format
    !! mesh format
  TYPE( mshPhysicalNames_ ) :: physicalNames
    !! mesh physical groups
  TYPE( mshNodes_ ) :: nodes
    !! nodes
  TYPE( mshElements_ ) :: elements
    !! elements
  TYPE( mshEntity_ ), ALLOCATABLE :: pointEntities( : )
    !! point entities
  TYPE( mshEntity_ ), ALLOCATABLE :: curveEntities( : )
    !! curve entities
  TYPE( mshEntity_ ), ALLOCATABLE :: surfaceEntities( : )
    !! surface entities
  TYPE( mshEntity_ ), ALLOCATABLE :: volumeEntities( : )
    !! volume entities
  CONTAINS
    PRIVATE
    FINAL :: msh_Final
      !! Finalizer
    PROCEDURE, PUBLIC, PASS( obj ) :: DeallocateData => msh_DeallocateData
      !! deallocate the data
    PROCEDURE, PUBLIC, PASS( obj ) :: Initiate => msh_initiate
      !! initiate the object
    PROCEDURE, PUBLIC, PASS( obj ) :: ExportMesh => msh_ExportMesh
END TYPE MSH_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PUBLIC :: MSH_

TYPE( MSH_ ), PUBLIC, PARAMETER :: TypegmshMesh = MSH_( &
  & mshFile = TypeTxtFile, Format = TypemshFormat, &
  & PhysicalNames = TypeMSHPhysicalNames, &
  & PointEntities = NULL( ), CurveEntities = NULL( ), &
  & SurfaceEntities = NULL( ), VolumeEntities = NULL( ), &
  & Nodes = TypeMshNodes, Elements = TypemshElements )

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: gmshMeshPointer_
  CLASS( MSH_ ), POINTER :: Ptr => NULL()
END TYPE gmshMeshPointer_

PUBLIC :: gmshMeshPointer_

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE msh_Final( obj )
  TYPE( MSH_ ), INTENT( INOUT ) :: obj
END SUBROUTINE msh_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                          DeallocateData@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	11 June 2021
! summary:  This will deallocate data

INTERFACE
MODULE SUBROUTINE msh_DeallocateData( obj )
  CLASS( MSH_ ), INTENT( INOUT ) :: obj
END SUBROUTINE msh_DeallocateData
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE msh_DeallocateData
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	11 June 2021
! summary: This subroutine generates the MSH_ object

INTERFACE
MODULE SUBROUTINE msh_initiate( obj, file, NSD )
  CLASS( MSH_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: file
  INTEGER( I4B ), INTENT( IN ) :: NSD
END SUBROUTINE msh_initiate
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE msh_initiate
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                                ExportMesh
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE msh_ExportMesh( obj, file )
  CLASS( MSH_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: file
END SUBROUTINE msh_ExportMesh
END INTERFACE

!----------------------------------------------------------------------------
! !                                                     Generate@BufferMethods
! !----------------------------------------------------------------------------

! INTERFACE
! !! This will add mesh generation command to .geo file

! !> authors: Dr. Vikas Sharma
! !
! ! This will add mesh generation command to .geo file

! MODULE FUNCTION mesh_generate( obj, dim ) RESULT( ans )
!   CLASS( MSH_ ), INTENT( INOUT) :: obj
!   INTEGER( I4B ), INTENT( IN ) :: dim
!   INTEGER( I4B ) :: ans
! END FUNCTION mesh_generate
! END INTERFACE

! !----------------------------------------------------------------------------
! !                                                        Write@BufferMethods
! !----------------------------------------------------------------------------

! INTERFACE
! !! This function will dump the buffer content in to a file

! !> authors: Dr. Vikas Sharma
! !
! ! This function will dump the buffer content in to a file

! MODULE FUNCTION mesh_write( obj, UnitNo ) RESULT( ans )
!   CLASS( MSH_ ), INTENT( INOUT) :: obj
!   INTEGER( I4B ), INTENT( IN ) :: UnitNo
!   INTEGER( I4B ) :: ans
! END FUNCTION mesh_write
! END INTERFACE

! !----------------------------------------------------------------------------
! !                                               gmshMesh@ConstructorMethods
! !----------------------------------------------------------------------------

! INTERFACE
! !! This function will create the [[MSH_]] object

! !> authors: Dr. Vikas Sharma
! !
! ! This function will create the [[MSH_]] object

! MODULE FUNCTION msh_constuctor1(Path,FileName,Extension,NSD) RESULT(ans)
!   TYPE( MSH_ ) :: ans
!   CHARACTER( LEN = * ), INTENT( IN ) :: FileName, Extension, Path
!   INTEGER( I4B ), INTENT( IN ) :: NSD
! END FUNCTION msh_constuctor1
! END INTERFACE

! INTERFACE gmshMesh
!   MODULE PROCEDURE msh_constuctor1
! END INTERFACE gmshMesh

! PUBLIC :: gmshMesh

! !----------------------------------------------------------------------------
! !                                                 Display@ConstructorMethods
! !----------------------------------------------------------------------------

! INTERFACE
! !! This subroutine display the content of gmshmesh object

! !> authors: Dr. Vikas Sharma
! !
! ! This subroutine display the content of obj

! MODULE SUBROUTINE msh_display( obj, Msg, UnitNo )
!   CLASS( MSH_ ), INTENT( IN ) :: obj
!   CHARACTER( LEN = * ), INTENT( IN ) :: msg
!   INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
! END SUBROUTINE msh_display
! END INTERFACE

! INTERFACE Display
!   MODULE PROCEDURE msh_display
! END INTERFACE Display

! PUBLIC :: Display
!
! !----------------------------------------------------------------------------
!                                                   TotalNodes@NodesMethods
! !----------------------------------------------------------------------------

! INTERFACE
! !! This function will return the total number of nodes in mesh

! !> authors: Dr. Vikas Sharma
! !
! ! This function will return the total number of nodes in mesh

! MODULE PURE FUNCTION msh_totalnodes( obj ) RESULT( ans )
!   CLASS( MSH_ ), INTENT( IN ) :: obj
!   INTEGER( I4B ) :: ans
! END FUNCTION msh_totalnodes
! END INTERFACE

! !----------------------------------------------------------------------------
! !                                                      getNodes@NodesMethods
! !----------------------------------------------------------------------------

! INTERFACE
! !! This subroutine retunr the nodal coordinates

! !> authors: Dr. Vikas Sharma
! !
! !  This subroutine returns the nodal coordinates
! MODULE PURE SUBROUTINE msh_getnodes_array( obj, Nodes )
!   CLASS( MSH_ ), INTENT( IN ) :: obj
!   REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Nodes( :, : )
! END SUBROUTINE msh_getnodes_array
! END INTERFACE

! !----------------------------------------------------------------------------
! !                                                      getNodes@NodesMethods
! !----------------------------------------------------------------------------

! INTERFACE
! !! This subroutine retunr the nodal coordinates

! !> authors: Dr. Vikas Sharma
! !
! !  This subroutine returns the nodal coordinates
! MODULE SUBROUTINE msh_getnodes_file( obj, UnitNo, Str, EndStr )
!   CLASS( MSH_ ), INTENT( IN ) :: obj
!   INTEGER( I4B ), INTENT( IN ), OPTIONAL :: UnitNo
!   CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: Str, EndStr
! END SUBROUTINE msh_getnodes_file
! END INTERFACE

! !----------------------------------------------------------------------------
! !                                              TotalElements@ElementsMethods
! !----------------------------------------------------------------------------

! INTERFACE
! !! This function returns the total element in the mesh

! !> authors: Dr. Vikas Sharma
! !
! ! This function returns the total element in the mesh

! MODULE PURE FUNCTION msh_telements_1( obj ) RESULT( ans )
!   CLASS( MSH_ ), INTENT( IN ) :: obj
!   INTEGER( I4B ) :: ans
! END FUNCTION msh_telements_1
! END INTERFACE

! !----------------------------------------------------------------------------
! !                                                TotalElements@ElementMethods
! !----------------------------------------------------------------------------

! INTERFACE
! !! This function returns the total element in the mesh

! !> authors: Dr. Vikas Sharma
! !
! ! This function returns the total element in the mesh
! ! Xidim is a codimension based filter
! ! Xidim=0 => Point
! ! Xidim=1 => Curve
! ! Xidim=2 => Surface
! ! Xidim=3 => Volume

! MODULE PURE FUNCTION msh_telements_2( obj, XiDim ) RESULT( ans )
!   CLASS( MSH_ ), INTENT( IN ) :: obj
!   INTEGER( I4B ), INTENT( IN ) :: XiDim
!   INTEGER( I4B ) :: ans
! END FUNCTION msh_telements_2
! END INTERFACE

! !----------------------------------------------------------------------------
! !                                                TotalElements@ElementMethods
! !----------------------------------------------------------------------------

! INTERFACE
! !! This function returns the total element in the mesh

! !> authors: Dr. Vikas Sharma
! !
! ! This function returns the total element in the mesh
! ! We can filter elements based on `Xidim` and `tag`

! MODULE PURE FUNCTION msh_telements_3( obj, XiDim, Tag ) RESULT( ans )
!   CLASS( MSH_ ), INTENT( IN ) :: obj
!   INTEGER( I4B ), INTENT( IN ) :: XiDim
!   INTEGER( I4B ), INTENT( IN ) :: Tag( : )
!   INTEGER( I4B ) :: ans
! END FUNCTION msh_telements_3
! END INTERFACE

! !----------------------------------------------------------------------------
! !                                                getElements@ElementsMethods
! !----------------------------------------------------------------------------

! INTERFACE
! !! This subroutine returns a mesh of elements;

! !> authors: Dr. Vikas Sharma
! !
! ! This subroutine returns a single [[mesh_]] object containing all elements

! MODULE SUBROUTINE msh_getelements_1( obj, Meshobj, FEobj )
!   CLASS( MSH_ ), INTENT( IN ) :: obj
!   CLASS( Mesh_ ), INTENT( INOUT), TARGET :: Meshobj
!   CLASS( Element_ ), INTENT( IN ) :: FEobj
! END SUBROUTINE msh_getelements_1
! END INTERFACE

! !----------------------------------------------------------------------------
! !                                                getElements@ElementsMethods
! !----------------------------------------------------------------------------

! INTERFACE
! !! This subroutine builds a mesh of elements with same co-dimensions

! !> This subroutine builds a mesh of elements with same co-dimensions
! !
! ! - For `Xidim=nsd` it returns all cell elements
! ! - For `Xidim=nsd-1` it returns all facet elements
! ! - For `Xidim=nsd-2` it returns all line elements
! ! - For `Xidim=nsd-3` it returns all the point elements
! !
! ! @note
! ! If `offset` is present then `Meshobj` should be allocated, in that case
! ! first element will be placed at `Meshobj % elem( offset + 1 )`. Therefore,
! ! there should be sufficient space in `Meshobj` to accomodate all new
! ! coming elements
! ! @endnote

! MODULE SUBROUTINE msh_getelements_2( obj,Meshobj, XiDim, FEobj, Offset )
!   CLASS( MSH_ ), INTENT( IN ) :: obj
!   CLASS( Mesh_ ), INTENT( INOUT), TARGET :: Meshobj
!   INTEGER( I4B ), INTENT( IN ) :: XiDim
!   CLASS( Element_ ), INTENT( IN ) :: FEobj
!   INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: Offset
! END SUBROUTINE msh_getelements_2
! END INTERFACE

! !----------------------------------------------------------------------------
! !                                                getElements@ElementsMethods
! !----------------------------------------------------------------------------

! INTERFACE
! !! This subroutine build a [[mesh_]] in [[domain_]] by using [[MSH_]]

! !> This subroutine builds a mesh of elements with same co-dimensions
! !
! ! - For `Xidim=nsd` it returns all cell elements
! ! - For `Xidim=nsd-1` it returns all facet elements
! ! - For `Xidim=nsd-2` it returns all line elements
! ! - For `Xidim=nsd-3` it returns all the point elements
! !
! ! If `offset` is present then `Meshobj` should be allocated, in that case
! ! first element will be placed at `Meshobj % elem( offset + 1 )`. Therefore,
! ! there should be sufficient space in `Meshobj` to accomodate all new
! ! coming elements
! !
! ! Note that this is just a wrapper for a method defined in
! ! [[MSH_::getelements]]

! MODULE SUBROUTINE msh_getelements_2c( obj, Dom,indx,XiDim,FEobj,Offset )
!   CLASS( MSH_ ), INTENT( IN ) :: obj
!   CLASS( Domain_ ), INTENT( INOUT), TARGET :: Dom
!   INTEGER( I4B ), INTENT( IN ) :: indx
!   INTEGER( I4B ), INTENT( IN ) :: XiDim
!   CLASS( Element_ ), INTENT( IN ) :: FEobj
!   INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: Offset
! END SUBROUTINE msh_getelements_2c
! END INTERFACE

! !----------------------------------------------------------------------------
! !                                                getElements@ElementsMethods
! !----------------------------------------------------------------------------

! INTERFACE
! !! This subroutine returns the mesh of elements

! !> authors: Dr. Vikas Sharma
! !
! ! This subroutine returns the mesh of elements; it applies two levels of
! ! filter
! !
! ! - For `Xidim=nsd` it returns all cell elements
! ! - For `Xidim=nsd-1` it returns all facet elements
! ! - For `Xidim=nsd-2` it returns all line elements
! ! - For `Xidim=nsd-3` it returns all the point elements
! !
! ! @note
! ! If `offset` is present then `Meshobj` should be allocated, in that case
! ! first element will be placed at `Meshobj % elem( offset + 1 )`. Therefore,
! ! there should be sufficient space in `Meshobj` to accomodate all new
! ! coming elements
! ! @endnote

! MODULE SUBROUTINE msh_getelements_3( obj, Meshobj, XiDim, Tag, FEobj,&
!   & Offset )
!   CLASS( MSH_ ), INTENT( IN ) :: obj
!   CLASS( Mesh_ ), INTENT( INOUT), TARGET :: Meshobj
!   INTEGER( I4B ), INTENT( IN ) :: XiDim, Tag( : )
!   CLASS( Element_ ), INTENT( IN ) :: FEobj
!   INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: Offset
! END SUBROUTINE msh_getelements_3
! END INTERFACE


! !----------------------------------------------------------------------------
! !                                                getElements@ElementsMethods
! !----------------------------------------------------------------------------

! INTERFACE
! !! This subroutine build a [[mesh_]] in [[domain_]] by using [[MSH_]]

! !> authors: Dr. Vikas Sharma
! !
! ! This subroutine returns the mesh of elements; it applies two levels of
! ! filter
! !
! ! - For `Xidim=nsd` it returns all cell elements
! ! - For `Xidim=nsd-1` it returns all facet elements
! ! - For `Xidim=nsd-2` it returns all line elements
! ! - For `Xidim=nsd-3` it returns all the point elements
! !
! ! @note
! ! If `offset` is present then `Meshobj` should be allocated, in that case
! ! first element will be placed at `Meshobj % elem( offset + 1 )`. Therefore,
! ! there should be sufficient space in `Meshobj` to accomodate all new
! ! coming elements
! ! @endnote
! !
! ! Note that this is just a wrapper for a method defined in
! ! [[MSH_::getelements]]

! MODULE SUBROUTINE msh_getelements_3c( obj, Dom, Indx, XiDim, Tag, &
!   & FEobj, Offset )
!   CLASS( MSH_ ), INTENT( IN ) :: obj
!   CLASS( Domain_ ), INTENT( INOUT), TARGET :: Dom
!   INTEGER( I4B ), INTENT( IN ) :: Indx
!   INTEGER( I4B ), INTENT( IN ) :: XiDim, Tag( : )
!   CLASS( Element_ ), INTENT( IN ) :: FEobj
!   INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: Offset
! END SUBROUTINE msh_getelements_3c
! END INTERFACE

! !----------------------------------------------------------------------------
! !                                                getElements@ElementsMethods
! !----------------------------------------------------------------------------

! INTERFACE
! !! This subroutine returns the mesh of elements

! !> authors: Dr. Vikas Sharma
! !
! ! This subroutine returns the mesh of elements; it applies two levels of
! ! filter
! !
! ! - For `Xidim=nsd` it returns all cell elements
! ! - For `Xidim=nsd-1` it returns all facet elements
! ! - For `Xidim=nsd-2` it returns all line elements
! ! - For `Xidim=nsd-3` it returns all the point elements
! !
! ! @note
! ! If `offset` is present then `Meshobj` should be allocated, in that case
! ! first element will be placed at `Meshobj % elem( offset + 1 )`. Therefore,
! ! there should be sufficient space in `Meshobj` to accomodate all new
! ! coming elements
! ! @endnote

! MODULE SUBROUTINE msh_getelements_4( obj, Meshobj, XiDim, TagNames, &
!   & FEobj, Offset )
!   CLASS( MSH_ ), INTENT( IN ) :: obj
!   CLASS( Mesh_ ), INTENT( INOUT), TARGET :: Meshobj
!   INTEGER( I4B ), INTENT( IN ) :: XiDim
!   TYPE( String ), INTENT( IN ) :: TagNames( : )
!   CLASS( Element_ ), INTENT( IN ) :: FEobj
!   INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: Offset
! END SUBROUTINE msh_getelements_4
! END INTERFACE

! !----------------------------------------------------------------------------
! !                                             getElements@ElementsMethods
! !----------------------------------------------------------------------------

! INTERFACE
! !! This subroutine build a [[mesh_]] in [[domain_]] by using [[MSH_]]

! !> authors: Dr. Vikas Sharma
! !
! ! This subroutine returns the mesh of elements; it applies two levels of
! ! filter
! !
! ! - For `Xidim=nsd` it returns all cell elements
! ! - For `Xidim=nsd-1` it returns all facet elements
! ! - For `Xidim=nsd-2` it returns all line elements
! ! - For `Xidim=nsd-3` it returns all the point elements
! !
! ! @note
! ! If `offset` is present then `Meshobj` should be allocated, in that case
! ! first element will be placed at `Meshobj % elem( offset + 1 )`. Therefore,
! ! there should be sufficient space in `Meshobj` to accomodate all new
! ! coming elements
! ! @endnote
! !
! ! Note that this is just a wrapper for a method defined in
! ! [[MSH_::getelements]]

! MODULE SUBROUTINE msh_getelements_4c( obj, Dom, Indx, XiDim, TagNames, &
!   & FEobj, Offset )
!   CLASS( MSH_ ), INTENT( IN ) :: obj
!   CLASS( Domain_ ), INTENT( INOUT), TARGET :: Dom
!   INTEGER( I4B ), INTENT( IN ) :: Indx
!   INTEGER( I4B ), INTENT( IN ) :: XiDim
!   TYPE( String ), INTENT( IN ) :: TagNames( : )
!   CLASS( Element_ ), INTENT( IN ) :: FEobj
!   INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: Offset
! END SUBROUTINE msh_getelements_4c
! END INTERFACE

! !----------------------------------------------------------------------------
! !                                              getElements@ElementsMethods
! !----------------------------------------------------------------------------

! INTERFACE
! !! This subroutine initiate [[domain_]] by reading gmshMesh file

! !> authors: Dr. Vikas Sharma
! !
! ! This subroutine initiate [[domain_]] by reading gmshMesh file
! ! This is a high level routine
! !
! ! - It gets all informatio from [[MSH_]] and allocate `obj %  omega`
! ! and `obj  % boundary`

! MODULE SUBROUTINE dom_init_from_gmshMesh( mshobj, obj, facetmesh )
!   CLASS( MSH_ ), INTENT( IN ) :: mshobj
!   CLASS( Domain_ ), INTENT( INOUT) :: obj
!   TYPE( String ), OPTIONAL, INTENT( IN ) ::  facetmesh( :, : )
! END SUBROUTINE dom_init_from_gmshMesh
! END INTERFACE

! !----------------------------------------------------------------------------
! !                                                      WriteMesh@Postprocess
! !----------------------------------------------------------------------------

! INTERFACE
! MODULE SUBROUTINE msh_write_mesh( obj, Path, Filename, Extension, &
!   & Nodes )
!   CLASS( MSH_ ), INTENT( IN ) :: obj
!   CHARACTER( LEN = * ), INTENT( IN ) :: Path
!   CHARACTER( LEN = * ), INTENT( IN ) :: FileName
!   CHARACTER( LEN = * ), INTENT( IN ) :: Extension
!   REAL( DFP ), INTENT( IN ) :: Nodes(:, :)
! END SUBROUTINE msh_write_mesh
! END INTERFACE

! !----------------------------------------------------------------------------
! !                                                  WriteNodeData@PostProcess
! !----------------------------------------------------------------------------

! INTERFACE
! !! This subroutine writes the node data information in a msh file format

! !> authors: Dr. Vikas Sharma
! !
! ! This subroutine writes the nodal values of a variable `x` into msh file
! !
! ! - `name` is the name of physical variable whose dof will be printed
! ! - The subroutine will decided the nature of physical variable and
! ! and write in msh file accordingly
! ! - The subroutine search for this name in [[dof_]] and if this name is
! ! is not present the subroutine writes nothing

! MODULE SUBROUTINE msh_write_nodedata_1( obj, x, dofobj, name, indx, &
!   & local_nptrs, nodes )
!   CLASS( MSH_ ), INTENT( IN ) :: obj
!   REAL( DFP ), INTENT( IN ) :: x( : )
!   TYPE( DOF_ ), INTENT( IN ) :: dofobj
!   INTEGER( I4B ), INTENT( IN ) :: indx( : )
!   INTEGER( I4B ), INTENT( IN ) :: local_nptrs( : )
!   CHARACTER( LEN = 1 ), INTENT( IN ) :: name
!   REAL( DFP ), OPTIONAL, INTENT( IN ) :: nodes( :, : )
! END SUBROUTINE msh_write_nodedata_1
! END INTERFACE

! !----------------------------------------------------------------------------
! !                                                  WriteNodeData@PostProcess
! !----------------------------------------------------------------------------

! INTERFACE
! !! This subroutine writes the node data information in a gmshMesh file format

! !> authors: Dr. Vikas Sharma
! !
! ! This subroutine writes the nodal values of a variable `x` into gmshMesh file
! !
! ! - It will write all the physical variables in vtk file
! ! - It calls [[msh_write_nodedata_2]] subroutine internally

! MODULE SUBROUTINE msh_write_nodedata_2( obj, x, dofobj, indx, &
!   & local_nptrs, nodes )
!   CLASS( MSH_ ), INTENT( IN ) :: obj
!   REAL( DFP ), INTENT( IN ) :: x( : )
!   TYPE( DOF_ ), INTENT( IN ) :: dofobj
!   INTEGER( I4B ), INTENT( IN ) :: indx( : )
!   INTEGER( I4B ), INTENT( IN ) :: local_nptrs( : )
!   REAL( DFP ), OPTIONAL, INTENT( IN ) :: nodes( :, : )
! END SUBROUTINE msh_write_nodedata_2
! END INTERFACE

! !----------------------------------------------------------------------------
! !
! !----------------------------------------------------------------------------

! INTERFACE msh_Pointer
!   MODULE PROCEDURE msh_constructor_1
! END INTERFACE msh_Pointer

! PUBLIC :: msh_Pointer

! !----------------------------------------------------------------------------
! !
! !----------------------------------------------------------------------------

! CONTAINS
! !-----------------------------------------------------------------------------
! !                                                      msh_constructor_1
! !-----------------------------------------------------------------------------

! FUNCTION msh_constructor_1( Path, FileName, Extension, NSD) RESULT(obj)
!     ! Define intent of dummy variables
!     CLASS( MSH_ ), POINTER :: obj
!     CHARACTER( LEN = * ), INTENT( IN ) :: FileName
!     CHARACTER( LEN = * ), INTENT( IN ), OPTIONAL :: Extension, Path
!     INTEGER( I4B ), INTENT( IN ) :: NSD

!     ALLOCATE( obj )
!     CALL obj % Initiate( Path, FileName, Extension, NSD )

! END FUNCTION msh_constructor_1

END MODULE MSH_Class