!#define SMALL_TRI3_MESH
#define SMALL_TRI6_MESH
! #define BIG_TRI3_MESH
!#define BIG_BIG_TRI3_MESH
! #define BIG_TRI6_MESH
!#define BIG_BIG_TRI6_MESH

PROGRAM main
USE easifemBase
USE Gmsh_Class
USE HDF5File_Class
USE MSHFile_Class

TYPE(Gmsh_) :: gmsh

#ifdef SMALL_TRI3_MESH
CHARACTER(LEN=*), PARAMETER :: title = "small_tri3_mesh"
REAL(DFP), PARAMETER :: lx = 2.0
REAL(DFP), PARAMETER :: ly = 2.0
INTEGER(I4B), PARAMETER :: order = 1
#endif

#ifdef SMALL_TRI6_MESH
CHARACTER(LEN=*), PARAMETER :: title = "small_tri6_mesh"
REAL(DFP), PARAMETER :: lx = 2.0
REAL(DFP), PARAMETER :: ly = 2.0
INTEGER(I4B), PARAMETER :: order = 2
#endif

#ifdef BIG_TRI3_MESH
CHARACTER(LEN=*), PARAMETER :: title = "big_tri3_mesh"
REAL(DFP), PARAMETER :: lx = 100.0
REAL(DFP), PARAMETER :: ly = 100.0
INTEGER(I4B), PARAMETER :: order = 1
#endif

#ifdef BIG_BIG_TRI3_MESH
CHARACTER(LEN=*), PARAMETER :: title = "big_big_tri3_mesh"
REAL(DFP), PARAMETER :: lx = 500.0
REAL(DFP), PARAMETER :: ly = 200.0
INTEGER(I4B), PARAMETER :: order = 1
#endif

#ifdef BIG_TRI6_MESH
CHARACTER(LEN=*), PARAMETER :: title = "big_tri6_mesh"
REAL(DFP), PARAMETER :: lx = 100.0
REAL(DFP), PARAMETER :: ly = 100.0
INTEGER(I4B), PARAMETER :: order = 2
#endif

#ifdef BIG_BIG_TRI6_MESH
CHARACTER(LEN=*), PARAMETER :: title = "big_big_tri6_mesh"
REAL(DFP), PARAMETER :: lx = 500.0
REAL(DFP), PARAMETER :: ly = 200.0
INTEGER(I4B), PARAMETER :: order = 2
#endif

REAL(DFP), PARAMETER :: meshSize = 1.0
REAL(DFP), PARAMETER :: corner(3) = 0.0_DFP
INTEGER(I4B) :: ierr
REAL(DFP) :: x, y, z, lc

TYPE(MSHFile_) :: mshFile
TYPE(HDF5File_) :: hdf5file

ierr = gmsh%Initialize()
ierr = gmsh%model%add(title)

x = corner(1)
y = corner(2)
z = corner(3)
lc = meshSize
ierr = gmsh%model%geo%addPoint(x=x, y=y, z=z, meshSize=lc, tag=1)

x = x + lx
y = y
z = z
lc = lc
ierr = gmsh%model%geo%addPoint(x=x, y=y, z=z, meshSize=lc, tag=2)

x = x
y = y + ly
z = z
lc = lc
ierr = gmsh%model%geo%addPoint(x=x, y=y, z=z, meshSize=lc, tag=3)

x = corner(1)
y = y
z = z
lc = lc
ierr = gmsh%model%geo%addPoint(x=x, y=y, z=z, meshSize=lc, tag=4)

ierr = gmsh%model%geo%addLine(1, 2, 1)
ierr = gmsh%model%geo%addLine(2, 3, 2)
ierr = gmsh%model%geo%addLine(3, 4, 3)
ierr = gmsh%model%geo%addLine(4, 1, 4)

ierr = gmsh%model%geo%addCurveLoop([1, 2, 3, 4], tag=1)

ierr = gmsh%model%geo%addPlaneSurface([1], 1)

ierr = gmsh%model%geo%synchronize()

ierr = gmsh%option%setNumber(VALUE=1, name="Mesh.SaveAll")
ierr = gmsh%model%mesh%generate(2)

ierr = gmsh%model%mesh%setOrder(order)

ierr = gmsh%WRITE(title//'.msh')

ierr = gmsh%Finalize()

CALL mshFile%Initiate(filename=title//'.msh', STATUS="OLD", ACTION="READ")
CALL mshFile%OPEN()
CALL mshFile%READ()
CALL hdf5file%Initiate(title//'.h5', MODE="NEW")
CALL hdf5file%OPEN()
CALL mshFile%Export(hdf5=hdf5file, group="")
CALL mshFile%DEALLOCATE()
CALL hdf5file%DEALLOCATE()

END PROGRAM main
