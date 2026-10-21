!> author: Vikas Sharma, Ph. D.
! summary: This example shows how to get the bounding box of the mesh.
!
! In the previous example, we do not specify the nodal coordinates.
!
! However, in this case we specify the nodal coordinates
!
! Note: It is often the case when the mesh coordinates are changing during the
! simulation.
!
! ## Usage

PROGRAM main
  USE easifemBase
  USE easifemClasses
  IMPLICIT NONE
  TYPE(Mesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  TYPE(Domain_) :: dom
  TYPE(BoundingBox_) :: box
  REAL(DFP), POINTER :: nodeCoord(:, :)
  INTEGER(I4B), POINTER :: local_nptrs(:)

  ! Open the mesh file which is [[HDF5File_]] format.

  CALL meshfile%Initiate(FileName="./mesh.h5", MODE="READ")
  CALL meshfile%Open()

  ! Initiate an instance of [[Mesh_]] and [[Domain_]] by reading the mesh file.

  CALL dom%Initiate(hdf5=meshfile, group="")
  CALL obj%Initiate(hdf5=meshfile, group="/surfaceEntities_1")

  ! Get the pointer to nodal coordinates from the [[Domain_]]

  nodeCoord => dom%GetNodeCoordPointer()
  local_nptrs => dom%GetGlobalToLocalNodeNumPointer()

  ! Get the bounding box of the mesh by specifying the nodal coordinates and local nptrs.

  box = obj%GetBoundingBox(nodes=nodeCoord, local_nptrs=local_nptrs)
  CALL Display(box, "")

  CALL obj%Deallocate()
  CALL meshfile%Close()
  CALL meshfile%Deallocate()
END PROGRAM main
