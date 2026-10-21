PROGRAM main
USE easifemBase
USE easifemClasses
IMPLICIT NONE

TYPE(GmshStructuredMesh_) :: obj
TYPE(Gmsh_) :: gmsh
TYPE(ParameterList_) :: param
CHARACTER(*), PARAMETER :: title = "small_tri3_mesh_two_region"
REAL(DFP), PARAMETER :: pointsOnAxis1(3) = [0.0, 1.0, 2.0]
REAL(DFP), PARAMETER :: pointsOnAxis2(2) = [0.0, 1.0]

INTEGER(I4B), PARAMETER :: transfinitePointsOnAxis1(2) = [5, 5]
INTEGER(I4B), PARAMETER :: transfinitePointsOnAxis2(1) = [5]
LOGICAL(LGT), PARAMETER :: recombineAll = .FALSE.

INTEGER(I4B) :: ierr

TYPE(MSHFile_) :: mshFile
TYPE(HDF5File_) :: hdf5file

CALL FPL_Init()
CALL param%Initiate()

CALL SetGmshStructuredMeshParam( &
  & param=param,  &
  & filename=title//".msh", &
  & pointsOnAxis1=pointsOnAxis1,  &
  & pointsOnAxis2=pointsOnAxis2,  &
  & transfinitePointsOnAxis1=transfinitePointsOnAxis1,  &
  & transfinitePointsOnAxis2=transfinitePointsOnAxis2,  &
  & recombineAll=recombineAll)

CALL obj%Initiate(param)

ierr = gmsh%initialize()
ierr = gmsh%model%add("GmshStructuredMesh2D")
CALL obj%Generate(gmsh)
! ierr = gmsh%fltk%run()
ierr = gmsh%finalize()

CALL param%DEALLOCATE()
CALL obj%DEALLOCATE()

CALL mshFile%Initiate(filename=title//'.msh', STATUS="OLD", ACTION="READ")
CALL mshFile%OPEN()
CALL mshFile%READ()
CALL hdf5file%Initiate(title//'.h5', MODE="NEW")
CALL hdf5file%OPEN()
CALL mshFile%Export(hdf5=hdf5file, group="")
CALL mshFile%DEALLOCATE()
CALL hdf5file%DEALLOCATE()

END PROGRAM main
