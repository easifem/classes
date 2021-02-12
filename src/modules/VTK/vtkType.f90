MODULE vtkType
!! This module contains methods to write mesh data in vtk file format

!> authors: Dr. Vikas Sharma
!
! It is a wrapper around Vtk-fortran library of
! [https://github.com/szaghi/VTKFortran](Szaghi)
!

USE GlobalData
USE BaseType
USE vtk_fortran
USE Mesh_Class
USE MeshData_Class
IMPLICIT NONE

PRIVATE

!----------------------------------------------------------------------------
!                                                                       vtk_
!----------------------------------------------------------------------------

TYPE :: vtk_
  TYPE( vtk_file ) :: afile
  TYPE( String ) :: path
  TYPE( String ) :: filename
  TYPE( String ) :: extension
  CHARACTER( LEN = 10 ) :: fmt = "ascii"
  CHARACTER( LEN = 10 ) :: meshTopology = "UnstructuredGrid"
  INTEGER( I4B ) :: nx1 = 0, nx2 = 0, ny1 = 0, ny2 = 0, nz1 = 0, nz2 = 0
  LOGICAL( LGT ) :: inPiece = .FALSE.

  CONTAINS

  PROCEDURE, PUBLIC, PASS( obj ) :: Initiate => vtk_init
  PROCEDURE, PUBLIC, PASS( obj ) :: Finalize => vtk_final
  PROCEDURE, PUBLIC, PASS( obj ) :: openPiece => vtk_start_write_geo
  PROCEDURE, PUBLIC, PASS( obj ) :: closePiece => vtk_stop_write_geo

  GENERIC, PUBLIC :: WriteGeometry => vtk_write_mesh_data_1,&
    & vtk_write_mesh_data_2, vtk_write_mesh_data_3
  PROCEDURE, PUBLIC, PASS( obj ) :: vtk_write_mesh_data_1
  PROCEDURE, PUBLIC, PASS( obj ) :: vtk_write_mesh_data_2
  PROCEDURE, PUBLIC, PASS( obj ) :: vtk_write_mesh_data_3

  PROCEDURE, PUBLIC, PASS( Obj ) :: openNodeData &
    & => vtk_start_write_node_data
  PROCEDURE, PUBLIC, PASS( Obj ) :: closeNodeData &
    & => vtk_stop_write_node_data

  GENERIC, PUBLIC :: WriteNodeData => &
    & vtk_write_node_data_1, &
    & vtk_write_node_data_2
  PROCEDURE, PUBLIC, PASS( obj ) :: vtk_write_node_data_1
  PROCEDURE, PUBLIC, PASS( obj ) :: vtk_write_node_data_2

  PROCEDURE, PUBLIC, PASS( Obj ) :: openElementData &
    & => vtk_start_write_elem_data
  PROCEDURE, PUBLIC, PASS( Obj ) :: closeElementData &
    & => vtk_stop_write_elem_data

  GENERIC, PUBLIC :: WriteQuadratureData => &
    & vtk_write_cell_data_1, &
    & vtk_write_cell_data_2

  PROCEDURE, PUBLIC, PASS( Obj ) :: vtk_write_cell_data_1, &
    & vtk_write_cell_data_2

END TYPE vtk_

PUBLIC :: vtk_

TYPE( vtk_ ), PUBLIC, PARAMETER :: TypeVTK = vtk_( )

!----------------------------------------------------------------------------
!                                                                vtkPointer_
!----------------------------------------------------------------------------

TYPE :: vtkPointer_
  CLASS( vtk_ ), POINTER :: Ptr => NULL( )
END TYPE vtkPointer_

PUBLIC :: vtkPointer_

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine initiate the vtk object

!> authors: Dr. Vikas Sharma
!
! This subroutine initate the vtk file object
!

MODULE SUBROUTINE vtk_init( obj, path, filename, extension, fmt, &
  & meshTopology, indx, nx1, nx2, ny1, ny2, nz1, nz2 )
  CLASS( vtk_ ), INTENT( INOUT) :: obj
    !! [[vtk_]] filename object
  CHARACTER( LEN = * ),  INTENT( IN ) :: path
    !! path of the filename to be created
  CHARACTER( LEN = * ),  INTENT( IN ) :: filename
    !! name of the file to be created
  CHARACTER( LEN = * ),  INTENT( IN ) :: extension
    !! extension of the file `.vtu`
  CHARACTER( LEN = * ),  INTENT( IN ) :: fmt
    !! storage format `ascii, binary_append, binary`
  CHARACTER( LEN = * ),  INTENT( IN ) :: meshTopology
    !! mesh topology `UnstructuredGrid`
  INTEGER( I4B ), INTENT( IN ) :: indx( : )
    !! `its(1)` = time-step
    !! `its(2)` = iteration number, if present
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: nx1, nx2
    !! for structured grid only
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: ny1, ny2
    !! for structured grid only
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: nz1, nz2
    !! for structured grid only
END SUBROUTINE vtk_init
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   Finalize
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine closes the opened vtk file

!> authors: Dr. Vikas Sharma
!
! This subroutine close the open vtk file

MODULE SUBROUTINE vtk_final( obj )
  CLASS( vtk_ ), INTENT( INOUT) :: obj
END SUBROUTINE vtk_final
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE vtk_final
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE vtk_display( obj, msg, unitno )
  CLASS( vtk_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE vtk_display
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE vtk_display
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                                 openPiece
!----------------------------------------------------------------------------

INTERFACE
!! Should be called before [[vtk_:writegeomery]] call

!> authors: Dr. Vikas Sharma
!
! This subroutine should be called before start writing the geometry
! in the `<piece>` tag
!
! - This subroutine indicates that we are going to write the mesh information
MODULE SUBROUTINE vtk_start_write_geo( Obj )
  CLASS( vtk_ ), INTENT( INOUT) ::  obj
END SUBROUTINE vtk_start_write_geo
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 closePiece
!----------------------------------------------------------------------------

INTERFACE
!! Should be called before [[vtk_:writegeomery]] call

!> authors: Dr. Vikas Sharma
!
! This subroutine should be called after we are done writing the mesh
! information, point data and cell data.
!
! This subroutine will write `</piece>` tag
!
MODULE SUBROUTINE vtk_stop_write_geo( Obj )
  CLASS( vtk_ ), INTENT( INOUT) ::  obj
END SUBROUTINE vtk_stop_write_geo
END INTERFACE

!----------------------------------------------------------------------------
!                                                              WriteGeometry
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine writes the mesh data information in the

!> authors: Dr. Vikas Sharma
!
! This subroutine writes the information of the mesh in vtkfile
! In this interface use must provide the [[meshdata_]] object.
!
! - In the vtk file local node numbers are written
! - Only those points (coords) are written which are included in meshobj
! - Note that `nodes` can represent a super set of nodes; nodes present in
! mesh are subset of `nodes`

MODULE SUBROUTINE vtk_write_mesh_data_1( obj, meshobj, mdObj, nodes )
  CLASS( vtk_ ), INTENT( INOUT) :: obj
    !! [[vtk_]] file
  CLASS( Mesh_ ), INTENT( INOUT ), TARGET :: meshobj
    !! [[mesh_]] instance
  CLASS( MeshData_ ), INTENT( INOUT ) :: mdobj
    !! [[meshData_]] instance
  REAL( DFP ), INTENT( IN ) :: nodes( :, : )
    !! Nodal coordinates in xij format
END SUBROUTINE vtk_write_mesh_data_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                             WriteGeometry
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine writes the mesh data information in the

!> authors: Dr. Vikas Sharma
!
! This subroutine writes the information of the mesh in vtkfile
! This subroutine should be used carefully
! We advise you to avoide using this subroutine as much as possible

MODULE SUBROUTINE vtk_write_mesh_data_2( obj, meshobj, nodes )
  CLASS( vtk_ ), INTENT( INOUT) :: obj
    !! [[vtk_]] file
  CLASS( Mesh_ ), INTENT( INOUT ), TARGET :: meshobj
    !! [[mesh_]] instance
  REAL( DFP ), INTENT( IN ) :: nodes( :, : )
    !! Nodal coordinates in xij format
END SUBROUTINE vtk_write_mesh_data_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                              WriteGeometry
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine writes the mesh data information in the

!> authors: Dr. Vikas Sharma
!
! This subroutine writes the information of the mesh in vtkfile
! In this interface use must provide the [[meshdata_]] object.
!
! - In the vtk file local node numbers are written
! - Only those points (coords) are written which are included in meshobj
! - Note that `nodes` can represent a super set of nodes; nodes present in
! mesh are subset of `nodes`

MODULE SUBROUTINE vtk_write_mesh_data_3(obj, meshobj, nodes, tag, &
  & local2global, map, lb, ub)
  CLASS( vtk_ ), INTENT( INOUT) :: obj
    !! [[vtk_]] file
  CLASS( MeshPointer_ ), INTENT( INOUT ), TARGET :: meshobj(:)
    !! [[mesh_]] instance
  REAL( DFP ), INTENT( IN ) :: nodes( :, : )
    !! Nodal coordinates in xij format
  INTEGER( I4B ), INTENT( IN ) :: lb, ub
  INTEGER( I4B ), INTENT( IN ) :: local2global(:), map(lb:ub)
  INTEGER( I4B ), INTENT( IN ) :: tag(:)
END SUBROUTINE vtk_write_mesh_data_3
END INTERFACE

!----------------------------------------------------------------------------
!                                                               openNodeData
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE vtk_start_write_node_data( obj )
  CLASS( vtk_ ), INTENT( INOUT) :: obj
END SUBROUTINE vtk_start_write_node_data
END INTERFACE

!----------------------------------------------------------------------------
!                                                              closeNodeData
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE vtk_stop_write_node_data( obj )
  CLASS( vtk_ ), INTENT( INOUT) :: obj
END SUBROUTINE vtk_stop_write_node_data
END INTERFACE

!----------------------------------------------------------------------------
!                                                              WriteNodeData
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine writes the node data information in a vtk file format

!> authors: Dr. Vikas Sharma
!
! This subroutine writes the nodal values of a variable `x` into vtk file
!
! - `name` is the name of physical variable whose dof will be printed
! - The subroutine will decided the nature of physical variable and
! and write in vtk file accordingly
! - The subroutine search for this name in [[dof_]] and if this name is
! is not present the subroutine writes nothing

MODULE SUBROUTINE vtk_write_node_data_1( obj, x, dofobj, name, prefix )
  CLASS( vtk_ ), INTENT( INOUT) :: obj
  REAL( DFP ), INTENT( IN ) :: x( : )
  TYPE( DOF_ ), INTENT( IN ) :: dofobj
  CHARACTER( LEN = 1 ), INTENT( IN ) :: name
  CHARACTER( LEN = * ), INTENT( IN ) :: prefix
    !! It is the name of a DOF present inside dofobj; we will find this name
    !! and write data corresponding to that physical name
END SUBROUTINE vtk_write_node_data_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                              WriteNodeData
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine writes the node data information in a vtk file format

!> authors: Dr. Vikas Sharma
!
! This subroutine writes the nodal values of a variable `x` into vtk file
!
! - It will write all the physical variables in vtk file
! - It calls [[vtk_write_node_data_1]] subroutine internally

MODULE SUBROUTINE vtk_write_node_data_2( obj, x, dofobj, prefix )
  CLASS( vtk_ ), INTENT( INOUT) :: obj
  REAL( DFP ), INTENT( IN ) :: x( : )
  TYPE( DOF_ ), INTENT( IN ) :: dofobj
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: prefix
END SUBROUTINE vtk_write_node_data_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      startWriteElementData
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE vtk_start_write_elem_data( obj )
  CLASS( vtk_ ), INTENT( INOUT) :: obj
END SUBROUTINE vtk_start_write_elem_data
END INTERFACE

!----------------------------------------------------------------------------
!                                                       stopWriteElementData
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE vtk_stop_write_elem_data( obj )
  CLASS( vtk_ ), INTENT( INOUT) :: obj
END SUBROUTINE vtk_stop_write_elem_data
END INTERFACE

!----------------------------------------------------------------------------
!                                                              WriteElemData
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine writes the cell data information in a vtk file format

!> authors: Dr. Vikas Sharma
!
! This subroutine writes the cell data (single scalars) into a vtk file

MODULE SUBROUTINE vtk_write_cell_data_1( obj, val, name )
  CLASS( vtk_ ), INTENT( INOUT) :: obj
  REAL( DFP ), INTENT( IN ) :: val(:)
    !! size(val) should be equal to totol number of cell
  CHARACTER(LEN=*), INTENT( IN ) :: name
END SUBROUTINE vtk_write_cell_data_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                              WriteElemData
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine writes the cell data information in a vtk file format

!> authors: Dr. Vikas Sharma
!
! This subroutine writes the cell data (single vector) into a vtk file

MODULE SUBROUTINE vtk_write_cell_data_2( obj, val, name )
  CLASS( vtk_ ), INTENT( INOUT) :: obj
  REAL( DFP ), INTENT( IN ) :: val(:, :)
    !! size(val,2) should be equal to totol number of cell
    !! size(val,1) number of components
  CHARACTER(LEN=*), INTENT( IN ) :: name
END SUBROUTINE vtk_write_cell_data_2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE vtkType