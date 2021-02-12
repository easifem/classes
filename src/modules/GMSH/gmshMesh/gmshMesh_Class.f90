MODULE gmshMesh_Class
USE BaseType
USE GlobalData
USE mshFormat_Class
USE mshPhysicalNames_Class
USE mshEntity_Class
USE mshNodes_Class
USE mshElements_Class
USE Mesh_Class
USE FE
USE Domain_Class
IMPLICIT NONE

PRIVATE

!----------------------------------------------------------------------------
!                                                                 gmshMesh_
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This class handles the mesh generation by gmsh
! This class can generate the mesh and reads the mesh file

TYPE :: gmshMesh_

  TYPE( Buffer_ ), POINTER :: buffer => NULL( )
    !! buffer to recoord coommands
  INTEGER( I4B ) :: nsd = 0
    !! Spatial dimension
  TYPE( File_ ) :: mshFile
    !! mesh file
  TYPE( mshFormat_ ) :: Format
    !! mesh format
  TYPE( mshPhysicalNames_ ) :: PhysicalNames
    !! mesh physical groups
  TYPE( mshEntity_ ), ALLOCATABLE :: PointEntities( : )
    !! point entities
  TYPE( mshEntity_ ), ALLOCATABLE :: CurveEntities( : )
    !! curve entities
  TYPE( mshEntity_ ), ALLOCATABLE :: SurfaceEntities( : )
    !! surface entities
  TYPE( mshEntity_ ), ALLOCATABLE :: VolumeEntities( : )
    !! volume entities
  TYPE( mshNodes_ ) :: Nodes
    !! nodes
  TYPE( mshElements_ ) :: Elements
    !! elements
  CONTAINS
    PROCEDURE, PUBLIC, PASS( Obj ) :: generate => mesh_generate
    PROCEDURE, PUBLIC, PASS( Obj ) :: write => mesh_write

    PROCEDURE, PUBLIC, PASS( Obj ) :: Finalize => gmsh_mesh_deallocateData
      !! deallocate the data
    PROCEDURE, PUBLIC, PASS( Obj ) :: Initiate => gmsh_mesh_initiate
      !! initiate the object
    PROCEDURE, PUBLIC, PASS( Obj ) :: TotalNodes => gmsh_mesh_totalnodes
      !! returns total nodes in mesh

    GENERIC, PUBLIC :: getNodes => gmsh_mesh_getnodes_array, &
      & gmsh_mesh_getnodes_file
    PROCEDURE, PRIVATE, PASS( Obj ) :: gmsh_mesh_getnodes_array
      !! returns nodes in an array
    PROCEDURE, PRIVATE, PASS( Obj ) :: gmsh_mesh_getnodes_file
      !! writes notes to a file

    GENERIC, PUBLIC :: TotalElements => gmsh_mesh_telements_1, &
      & gmsh_mesh_telements_2, &
      & gmsh_mesh_telements_3
    PROCEDURE, PRIVATE, PASS( Obj ) :: gmsh_mesh_telements_1
      !! returns total elements in mesh
    PROCEDURE, PRIVATE, PASS( Obj ) :: gmsh_mesh_telements_2
      !! returns total elements in mesh
    PROCEDURE, PRIVATE, PASS( Obj ) :: gmsh_mesh_telements_3
      !! returns total elements in mesh

    GENERIC, PUBLIC :: getElements => &
      & gmsh_mesh_getelements_1,  &
      & gmsh_mesh_getelements_2,  &
      & gmsh_mesh_getelements_2c, &
      & gmsh_mesh_getelements_3,  &
      & gmsh_mesh_getelements_3c, &
      & gmsh_mesh_getelements_4,  &
      & gmsh_mesh_getelements_4c, &
      & dom_init_from_gmshMesh

    PROCEDURE, PUBLIC, PASS( Obj ) :: gmsh_mesh_getelements_1
      !! return finite element mesh
    PROCEDURE, PUBLIC, PASS( Obj ) :: gmsh_mesh_getelements_2
    PROCEDURE, PUBLIC, PASS( Obj ) :: gmsh_mesh_getelements_2c
    PROCEDURE, PUBLIC, PASS( Obj ) :: gmsh_mesh_getelements_3
    PROCEDURE, PUBLIC, PASS( Obj ) :: gmsh_mesh_getelements_3c
    PROCEDURE, PUBLIC, PASS( Obj ) :: gmsh_mesh_getelements_4
    PROCEDURE, PUBLIC, PASS( Obj ) :: gmsh_mesh_getelements_4c
    PROCEDURE, PUBLIC, PASS( mshobj ) :: dom_init_from_gmshMesh

    PROCEDURE, PUBLIC, PASS( Obj ) :: WriteMesh => gmsh_mesh_write_mesh
    GENERIC, PUBLIC :: WriteNodeData => &
      & gmsh_mesh_write_nodedata_1, &
      & gmsh_mesh_write_nodedata_2
    PROCEDURE, PUBLIC, PASS( Obj ) ::  gmsh_mesh_write_nodedata_1
    PROCEDURE, PUBLIC, PASS( Obj ) ::  gmsh_mesh_write_nodedata_2

END TYPE gmshMesh_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PUBLIC :: gmshMesh_

TYPE( gmshMesh_ ), PUBLIC, PARAMETER :: TypegmshMesh = gmshMesh_( &
  & mshFile = TypeFile, Format = TypemshFormat, &
  & PhysicalNames = TypeMSHPhysicalNames, &
  & PointEntities = NULL( ), CurveEntities = NULL( ), &
  & SurfaceEntities = NULL( ), VolumeEntities = NULL( ), &
  & Nodes = TypeMshNodes, Elements = TypemshElements )

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: gmshMeshPointer_
  CLASS( gmshMesh_ ), POINTER :: Ptr => NULL()
END TYPE gmshMeshPointer_

PUBLIC :: gmshMeshPointer_

!----------------------------------------------------------------------------
!                                                     Generate@BufferMethods
!----------------------------------------------------------------------------

INTERFACE
!! This will add mesh generation command to .geo file

!> authors: Dr. Vikas Sharma
!
! This will add mesh generation command to .geo file

MODULE FUNCTION mesh_generate( Obj, dim ) RESULT( Ans )
  CLASS( gmshMesh_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: dim
  INTEGER( I4B ) :: Ans
END FUNCTION mesh_generate
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Write@BufferMethods
!----------------------------------------------------------------------------

INTERFACE
!! This function will dump the buffer content in to a file

!> authors: Dr. Vikas Sharma
!
! This function will dump the buffer content in to a file

MODULE FUNCTION mesh_write( Obj, UnitNo ) RESULT( Ans )
  CLASS( gmshMesh_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: UnitNo
  INTEGER( I4B ) :: Ans
END FUNCTION mesh_write
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine generates the gmshMesh_ object

!> authors: Dr. Vikas Sharma
!
! This subroutine reads the .msh file and creates the object

MODULE SUBROUTINE gmsh_mesh_initiate( obj, Path, FileName, Extension, NSD )
  CLASS( gmshMesh_ ), INTENT( INOUT ) :: obj
    CHARACTER( LEN = * ), INTENT( IN ) :: FileName, Extension, Path
    INTEGER( I4B ), INTENT( IN ) :: NSD
END SUBROUTINE gmsh_mesh_initiate
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE gmsh_mesh_initiate
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                          DeallocateData@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
!! This will deallocate data

!> authors: Dr. Vikas Sharma
!
! This subroutine deallocate data

MODULE SUBROUTINE gmsh_mesh_deallocatedata( obj )
  CLASS( gmshMesh_ ), INTENT( INOUT) :: obj
END SUBROUTINE gmsh_mesh_deallocatedata
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE gmsh_mesh_deallocatedata
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                               gmshMesh@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
!! This function will create the [[gmshMesh_]] object

!> authors: Dr. Vikas Sharma
!
! This function will create the [[gmshMesh_]] object

MODULE FUNCTION gmsh_mesh_constuctor1(Path,FileName,Extension,NSD) RESULT(Ans)
  TYPE( gmshMesh_ ) :: Ans
  CHARACTER( LEN = * ), INTENT( IN ) :: FileName, Extension, Path
  INTEGER( I4B ), INTENT( IN ) :: NSD
END FUNCTION gmsh_mesh_constuctor1
END INTERFACE

INTERFACE gmshMesh
  MODULE PROCEDURE gmsh_mesh_constuctor1
END INTERFACE gmshMesh

PUBLIC :: gmshMesh

!----------------------------------------------------------------------------
!                                                 Display@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine display the content of gmshmesh object

!> authors: Dr. Vikas Sharma
!
! This subroutine display the content of obj

MODULE SUBROUTINE gmsh_mesh_display( obj, Msg, UnitNo )
  CLASS( gmshMesh_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
END SUBROUTINE gmsh_mesh_display
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE gmsh_mesh_display
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                   TotalNodes@NodesMethods
!----------------------------------------------------------------------------

INTERFACE
!! This function will return the total number of nodes in mesh

!> authors: Dr. Vikas Sharma
!
! This function will return the total number of nodes in mesh

MODULE PURE FUNCTION gmsh_mesh_totalnodes( obj ) RESULT( Ans )
  CLASS( gmshMesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: Ans
END FUNCTION gmsh_mesh_totalnodes
END INTERFACE

!----------------------------------------------------------------------------
!                                                      getNodes@NodesMethods
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine retunr the nodal coordinates

!> authors: Dr. Vikas Sharma
!
!  This subroutine returns the nodal coordinates
MODULE PURE SUBROUTINE gmsh_mesh_getnodes_array( obj, Nodes )
  CLASS( gmshMesh_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Nodes( :, : )
END SUBROUTINE gmsh_mesh_getnodes_array
END INTERFACE

!----------------------------------------------------------------------------
!                                                      getNodes@NodesMethods
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine retunr the nodal coordinates

!> authors: Dr. Vikas Sharma
!
!  This subroutine returns the nodal coordinates
MODULE SUBROUTINE gmsh_mesh_getnodes_file( obj, UnitNo, Str, EndStr )
  CLASS( gmshMesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: UnitNo
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: Str, EndStr
END SUBROUTINE gmsh_mesh_getnodes_file
END INTERFACE

!----------------------------------------------------------------------------
!                                              TotalElements@ElementsMethods
!----------------------------------------------------------------------------

INTERFACE
!! This function returns the total element in the mesh

!> authors: Dr. Vikas Sharma
!
! This function returns the total element in the mesh

MODULE PURE FUNCTION gmsh_mesh_telements_1( obj ) RESULT( Ans )
  CLASS( gmshMesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: Ans
END FUNCTION gmsh_mesh_telements_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                TotalElements@ElementMethods
!----------------------------------------------------------------------------

INTERFACE
!! This function returns the total element in the mesh

!> authors: Dr. Vikas Sharma
!
! This function returns the total element in the mesh
! Xidim is a codimension based filter
! Xidim=0 => Point
! Xidim=1 => Curve
! Xidim=2 => Surface
! Xidim=3 => Volume

MODULE PURE FUNCTION gmsh_mesh_telements_2( obj, XiDim ) RESULT( Ans )
  CLASS( gmshMesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: XiDim
  INTEGER( I4B ) :: Ans
END FUNCTION gmsh_mesh_telements_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                TotalElements@ElementMethods
!----------------------------------------------------------------------------

INTERFACE
!! This function returns the total element in the mesh

!> authors: Dr. Vikas Sharma
!
! This function returns the total element in the mesh
! We can filter elements based on `Xidim` and `tag`

MODULE PURE FUNCTION gmsh_mesh_telements_3( obj, XiDim, Tag ) RESULT( Ans )
  CLASS( gmshMesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: XiDim
  INTEGER( I4B ), INTENT( IN ) :: Tag( : )
  INTEGER( I4B ) :: Ans
END FUNCTION gmsh_mesh_telements_3
END INTERFACE

!----------------------------------------------------------------------------
!                                                getElements@ElementsMethods
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine returns a mesh of elements;

!> authors: Dr. Vikas Sharma
!
! This subroutine returns a single [[mesh_]] object containing all elements

MODULE SUBROUTINE gmsh_mesh_getelements_1( obj, MeshObj, FEObj )
  CLASS( gmshMesh_ ), INTENT( IN ) :: obj
  CLASS( Mesh_ ), INTENT( INOUT), TARGET :: MeshObj
  CLASS( Element_ ), INTENT( IN ) :: FEObj
END SUBROUTINE gmsh_mesh_getelements_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                getElements@ElementsMethods
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine builds a mesh of elements with same co-dimensions

!> This subroutine builds a mesh of elements with same co-dimensions
!
! - For `Xidim=nsd` it returns all cell elements
! - For `Xidim=nsd-1` it returns all facet elements
! - For `Xidim=nsd-2` it returns all line elements
! - For `Xidim=nsd-3` it returns all the point elements
!
! @note
! If `offset` is present then `MeshObj` should be allocated, in that case
! first element will be placed at `MeshObj % elem( offset + 1 )`. Therefore,
! there should be sufficient space in `MeshObj` to accomodate all new
! coming elements
! @endnote

MODULE SUBROUTINE gmsh_mesh_getelements_2( obj,MeshObj, XiDim, FEObj, Offset )
  CLASS( gmshMesh_ ), INTENT( IN ) :: obj
  CLASS( Mesh_ ), INTENT( INOUT), TARGET :: MeshObj
  INTEGER( I4B ), INTENT( IN ) :: XiDim
  CLASS( Element_ ), INTENT( IN ) :: FEObj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: Offset
END SUBROUTINE gmsh_mesh_getelements_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                getElements@ElementsMethods
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine build a [[mesh_]] in [[domain_]] by using [[gmshMesh_]]

!> This subroutine builds a mesh of elements with same co-dimensions
!
! - For `Xidim=nsd` it returns all cell elements
! - For `Xidim=nsd-1` it returns all facet elements
! - For `Xidim=nsd-2` it returns all line elements
! - For `Xidim=nsd-3` it returns all the point elements
!
! If `offset` is present then `MeshObj` should be allocated, in that case
! first element will be placed at `MeshObj % elem( offset + 1 )`. Therefore,
! there should be sufficient space in `MeshObj` to accomodate all new
! coming elements
!
! Note that this is just a wrapper for a method defined in
! [[gmshMesh_::getelements]]

MODULE SUBROUTINE gmsh_mesh_getelements_2c( Obj, Dom,indx,XiDim,FEObj,Offset )
  CLASS( gmshMesh_ ), INTENT( IN ) :: Obj
  CLASS( Domain_ ), INTENT( INOUT), TARGET :: Dom
  INTEGER( I4B ), INTENT( IN ) :: indx
  INTEGER( I4B ), INTENT( IN ) :: XiDim
  CLASS( Element_ ), INTENT( IN ) :: FEObj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: Offset
END SUBROUTINE gmsh_mesh_getelements_2c
END INTERFACE

!----------------------------------------------------------------------------
!                                                getElements@ElementsMethods
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine returns the mesh of elements

!> authors: Dr. Vikas Sharma
!
! This subroutine returns the mesh of elements; it applies two levels of
! filter
!
! - For `Xidim=nsd` it returns all cell elements
! - For `Xidim=nsd-1` it returns all facet elements
! - For `Xidim=nsd-2` it returns all line elements
! - For `Xidim=nsd-3` it returns all the point elements
!
! @note
! If `offset` is present then `MeshObj` should be allocated, in that case
! first element will be placed at `MeshObj % elem( offset + 1 )`. Therefore,
! there should be sufficient space in `MeshObj` to accomodate all new
! coming elements
! @endnote

MODULE SUBROUTINE gmsh_mesh_getelements_3( obj, MeshObj, XiDim, Tag, FEObj,&
  & Offset )
  CLASS( gmshMesh_ ), INTENT( IN ) :: obj
  CLASS( Mesh_ ), INTENT( INOUT), TARGET :: MeshObj
  INTEGER( I4B ), INTENT( IN ) :: XiDim, Tag( : )
  CLASS( Element_ ), INTENT( IN ) :: FEObj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: Offset
END SUBROUTINE gmsh_mesh_getelements_3
END INTERFACE


!----------------------------------------------------------------------------
!                                                getElements@ElementsMethods
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine build a [[mesh_]] in [[domain_]] by using [[gmshMesh_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine returns the mesh of elements; it applies two levels of
! filter
!
! - For `Xidim=nsd` it returns all cell elements
! - For `Xidim=nsd-1` it returns all facet elements
! - For `Xidim=nsd-2` it returns all line elements
! - For `Xidim=nsd-3` it returns all the point elements
!
! @note
! If `offset` is present then `MeshObj` should be allocated, in that case
! first element will be placed at `MeshObj % elem( offset + 1 )`. Therefore,
! there should be sufficient space in `MeshObj` to accomodate all new
! coming elements
! @endnote
!
! Note that this is just a wrapper for a method defined in
! [[gmshMesh_::getelements]]

MODULE SUBROUTINE gmsh_mesh_getelements_3c( Obj, Dom, Indx, XiDim, Tag, &
  & FEObj, Offset )
  CLASS( gmshMesh_ ), INTENT( IN ) :: Obj
  CLASS( Domain_ ), INTENT( INOUT), TARGET :: Dom
  INTEGER( I4B ), INTENT( IN ) :: Indx
  INTEGER( I4B ), INTENT( IN ) :: XiDim, Tag( : )
  CLASS( Element_ ), INTENT( IN ) :: FEObj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: Offset
END SUBROUTINE gmsh_mesh_getelements_3c
END INTERFACE

!----------------------------------------------------------------------------
!                                                getElements@ElementsMethods
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine returns the mesh of elements

!> authors: Dr. Vikas Sharma
!
! This subroutine returns the mesh of elements; it applies two levels of
! filter
!
! - For `Xidim=nsd` it returns all cell elements
! - For `Xidim=nsd-1` it returns all facet elements
! - For `Xidim=nsd-2` it returns all line elements
! - For `Xidim=nsd-3` it returns all the point elements
!
! @note
! If `offset` is present then `MeshObj` should be allocated, in that case
! first element will be placed at `MeshObj % elem( offset + 1 )`. Therefore,
! there should be sufficient space in `MeshObj` to accomodate all new
! coming elements
! @endnote

MODULE SUBROUTINE gmsh_mesh_getelements_4( obj, MeshObj, XiDim, TagNames, &
  & FEObj, Offset )
  CLASS( gmshMesh_ ), INTENT( IN ) :: obj
  CLASS( Mesh_ ), INTENT( INOUT), TARGET :: MeshObj
  INTEGER( I4B ), INTENT( IN ) :: XiDim
  TYPE( String ), INTENT( IN ) :: TagNames( : )
  CLASS( Element_ ), INTENT( IN ) :: FEObj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: Offset
END SUBROUTINE gmsh_mesh_getelements_4
END INTERFACE

!----------------------------------------------------------------------------
!                                                getElements@ElementsMethods
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine build a [[mesh_]] in [[domain_]] by using [[gmshMesh_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine returns the mesh of elements; it applies two levels of
! filter
!
! - For `Xidim=nsd` it returns all cell elements
! - For `Xidim=nsd-1` it returns all facet elements
! - For `Xidim=nsd-2` it returns all line elements
! - For `Xidim=nsd-3` it returns all the point elements
!
! @note
! If `offset` is present then `MeshObj` should be allocated, in that case
! first element will be placed at `MeshObj % elem( offset + 1 )`. Therefore,
! there should be sufficient space in `MeshObj` to accomodate all new
! coming elements
! @endnote
!
! Note that this is just a wrapper for a method defined in
! [[gmshMesh_::getelements]]

MODULE SUBROUTINE gmsh_mesh_getelements_4c( Obj, Dom, Indx, XiDim, TagNames, &
  & FEObj, Offset )
  CLASS( gmshMesh_ ), INTENT( IN ) :: Obj
  CLASS( Domain_ ), INTENT( INOUT), TARGET :: Dom
  INTEGER( I4B ), INTENT( IN ) :: Indx
  INTEGER( I4B ), INTENT( IN ) :: XiDim
  TYPE( String ), INTENT( IN ) :: TagNames( : )
  CLASS( Element_ ), INTENT( IN ) :: FEObj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: Offset
END SUBROUTINE gmsh_mesh_getelements_4c
END INTERFACE

!----------------------------------------------------------------------------
!                                                getElements@ElementsMethods
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine initiate [[domain_]] by reading gmshMesh file

!> authors: Dr. Vikas Sharma
!
! This subroutine initiate [[domain_]] by reading gmshMesh file
! This is a high level routine
!
! - It gets all informatio from [[gmshMesh_]] and allocate `obj %  omega`
! and `obj  % boundary`

MODULE SUBROUTINE dom_init_from_gmshMesh( mshobj, obj, facetmesh )
  CLASS( gmshMesh_ ), INTENT( IN ) :: mshobj
  CLASS( Domain_ ), INTENT( INOUT) :: obj
  TYPE( String ), OPTIONAL, INTENT( IN ) ::  facetmesh( :, : )
END SUBROUTINE dom_init_from_gmshMesh
END INTERFACE

!----------------------------------------------------------------------------
!                                                      WriteMesh@Postprocess
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE gmsh_mesh_write_mesh( obj, Path, Filename, Extension, &
  & Nodes )
  CLASS( gmshMesh_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Path
  CHARACTER( LEN = * ), INTENT( IN ) :: FileName
  CHARACTER( LEN = * ), INTENT( IN ) :: Extension
  REAL( DFP ), INTENT( IN ) :: Nodes(:, :)
END SUBROUTINE gmsh_mesh_write_mesh
END INTERFACE

!----------------------------------------------------------------------------
!                                                  WriteNodeData@PostProcess
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine writes the node data information in a msh file format

!> authors: Dr. Vikas Sharma
!
! This subroutine writes the nodal values of a variable `x` into msh file
!
! - `name` is the name of physical variable whose dof will be printed
! - The subroutine will decided the nature of physical variable and
! and write in msh file accordingly
! - The subroutine search for this name in [[dof_]] and if this name is
! is not present the subroutine writes nothing

MODULE SUBROUTINE gmsh_mesh_write_nodedata_1( obj, x, dofobj, name, indx, &
  & local_nptrs, nodes )
  CLASS( gmshMesh_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: x( : )
  TYPE( DOF_ ), INTENT( IN ) :: dofobj
  INTEGER( I4B ), INTENT( IN ) :: indx( : )
  INTEGER( I4B ), INTENT( IN ) :: local_nptrs( : )
  CHARACTER( LEN = 1 ), INTENT( IN ) :: name
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: nodes( :, : )
END SUBROUTINE gmsh_mesh_write_nodedata_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                  WriteNodeData@PostProcess
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine writes the node data information in a gmshMesh file format

!> authors: Dr. Vikas Sharma
!
! This subroutine writes the nodal values of a variable `x` into gmshMesh file
!
! - It will write all the physical variables in vtk file
! - It calls [[gmsh_mesh_write_nodedata_2]] subroutine internally

MODULE SUBROUTINE gmsh_mesh_write_nodedata_2( obj, x, dofobj, indx, &
  & local_nptrs, nodes )
  CLASS( gmshMesh_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: x( : )
  TYPE( DOF_ ), INTENT( IN ) :: dofobj
  INTEGER( I4B ), INTENT( IN ) :: indx( : )
  INTEGER( I4B ), INTENT( IN ) :: local_nptrs( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: nodes( :, : )
END SUBROUTINE gmsh_mesh_write_nodedata_2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE gmsh_mesh_Pointer
  MODULE PROCEDURE gmsh_mesh_constructor_1
END INTERFACE gmsh_mesh_Pointer

PUBLIC :: gmsh_mesh_Pointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS
!-----------------------------------------------------------------------------
!                                                      gmsh_mesh_constructor_1
!-----------------------------------------------------------------------------

FUNCTION gmsh_mesh_constructor_1( Path, FileName, Extension, NSD) RESULT(Obj)
    ! Define intent of dummy variables
    CLASS( gmshMesh_ ), POINTER :: Obj
    CHARACTER( LEN = * ), INTENT( IN ) :: FileName
    CHARACTER( LEN = * ), INTENT( IN ), OPTIONAL :: Extension, Path
    INTEGER( I4B ), INTENT( IN ) :: NSD

    ALLOCATE( Obj )
    CALL Obj % Initiate( Path, FileName, Extension, NSD )

END FUNCTION gmsh_mesh_constructor_1

END MODULE gmshMesh_Class