!> author: Vikas Sharma, Ph. D.
! summary: This example shows how to convert a mesh from Gmsh format to HDF5
!          format which is necessary to initiate the FEMesh object.

PROGRAM main
USE MSHFile_Class
USE HDF5File_Class
USE GlobalData

IMPLICIT NONE

TYPE(MSHFile_) :: mshFile
TYPE(HDF5File_) :: hdf5file
CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh"

CALL mshFile%Initiate(filename=filename//".msh", STATUS="OLD", ACTION="READ")

CALL mshFile%OPEN()

CALL mshFile%READ()

CALL hdf5file%Initiate(filename//".h5", MODE="NEW")

CALL hdf5file%OPEN()

CALL mshFile%Export(hdf5=hdf5file, group="")

CALL mshFile%DEALLOCATE()
CALL hdf5file%DEALLOCATE()

END PROGRAM main
