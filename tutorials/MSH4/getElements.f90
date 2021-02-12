program main
  use basetype
  use basemethod
  use mshType
  use Mesh_Class
  use FE
  USE StringiFor

  type( msh4_ ) :: obj
  type( mesh_ ) :: meshobj
  type( string ), allocatable :: tagnames( : )
  
  block
    call obj % initiate( "./", "mesh", ".msh" , 2 )
    call meshobj % initiate( nsd, Obj % TotalElements( ) )
    call obj % getelements( meshobj, TypeElement )
    call display( meshobj, "meshobj" )
  end block

  ! block
  !   allocate( tagnames( 1 ) )
  !   tagnames( 1 ) = String( "domain" )
  !   call obj % getelements( meshobj = meshobj, XiDim = 2, &
  !     & FEObj = TypeFacetElement, TagNames = tagnames )
  !   call display( meshobj, "## Mesh object" )
  ! end block

  !
  ! block
  !   type( mesh_ ) :: SurfaceMesh
  !   !
  !   call obj % getelements( meshobj = meshobj, XiDim = 1, &
  !     & FEObj = TypeFacetElement )
  !   !
  !   ! call display( meshobj, "## Mesh object" )
  !   !
  !   call obj % getelements( meshobj = SurfaceMesh, XiDim = 2, &
  !     & FEObj = TypeFacetElement )
  !   !
  !   call display( SurfaceMesh, "## Surface mesh object" )
  ! end block
  !  
  ! block
  !   integer( i4b ), allocatable :: indices( : )
  !   indices = Obj % PhysicalNames % IndexOFPhysicalCurve( [1,2,3] )
  !   call display( indices, "indices" )
  !   call display( TotalElements( Obj, 1, [1,2,3]), "total elements" )
  ! end block
  !
  ! block
  !   integer( I4B ), allocatable :: entities( : ), indices( : )
  !   integer( i4b ) :: tElements
  !
  !   Indices = Obj % PhysicalNames % IndexOfPhysicalCurve( [1] )
  !   call display( indices, "indices" )
  !
  !   Entities = ArrayValues( Obj % PhysicalNames % Entities( indices( 1 ) ), &
  !     & TypeIntI4B )
  !   call display( indices, "indices" )
  !
  !   tElements = Obj % CurveEntities( 1 ) % TotalElements( )
  !   call display( tElements, "telements")
  !
  !   call obj % getelements( meshobj = meshobj, FEObj = TypeFacetElement, &
  !     & XiDim = 1, Tag = [1,2] )
  !   call display( meshobj, "## Mesh object" )
  !
  ! end block
  !
  ! block
  !   type( string ), allocatable :: TagNames( : )
  !   TagNames = [String("bottom"), String("left")]
  !   call obj % getelements( meshobj = meshobj, FEObj = TypeFacetElement, &
  !     & XiDim = 1, TagNames = TagNames )
  !   call display( meshobj, "## Mesh object" )
  ! end block
  !
end program main
