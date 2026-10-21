PROGRAM main
USE FEMesh_Class
USE HDF5File_Class
USE GlobalData
USE Test_Method
USE VTKFile_Class

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER :: filename = &
                       "../../Mesh/examples/meshdata/small_mesh_two_region.h5"
CHARACTER(*), PARAMETER :: vtumeshfile = "./mesh.vtu"

TYPE(FEMesh_) :: obj
TYPE(HDF5File_) :: meshfile
TYPE(VTKFile_) :: vtk
INTEGER(I4B), PARAMETER :: nsd = 2
INTEGER(I4B), PARAMETER :: entities(2) = [1, 2]

CALL meshfile%Initiate(FileName=filename, MODE="READ")

CALL meshfile%OPEN()

CALL obj%Initiate(hdf5=meshfile, dim=nsd, entities=entities)

CALL OK(.TRUE., "Initiate")

CALL vtk%InitiateVTKFile(filename=vtumeshfile, mode="NEW", &
       DataFormat=VTK_BINARY_APPENDED, DataStructureType=VTK_UnStructuredGrid)

CALL obj%ExportToVTK(vtk=vtk)
CALL vtk%CLOSE()

CALL obj%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
