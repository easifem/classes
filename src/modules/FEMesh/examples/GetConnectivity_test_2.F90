PROGRAM main
USE FEMesh_Class, ONLY: FEMesh_
USE HDF5File_Class, ONLY: HDF5File_
USE GlobalData, ONLY: I4B
USE ReallocateUtility, ONLY: Reallocate
USE Display_Method, ONLY: Display

IMPLICIT NONE

TYPE(FEMesh_) :: obj
TYPE(HDF5File_) :: meshfile
CHARACTER(LEN=*), PARAMETER :: filename = "../../Mesh/examples/meshdata/small_quad4_mesh.h5"
INTEGER(I4B), PARAMETER :: nsd = 2

INTEGER(I4B) :: cellCon(100), faceCon(100), edgeCon(100), &
                nodeCon(100), tcellcon, tfacecon, tedgecon, tnodecon

! Initiate and open the mesh file which is in `HDF5File_` format.
CALL meshfile%Initiate(FileName=filename, MODE="READ")
! Open the mesh file
CALL meshfile%OPEN()
! Initiate an instance of `Mesh_`
CALL obj%Initiate(hdf5=meshfile, dim=nsd)
CALL meshfile%DEALLOCATE()

CALL obj%GetConnectivity_(globalElement=1, islocal=.TRUE., cellCon=cellCon, &
       faceCon=faceCon, edgeCon=edgeCon, nodeCon=nodeCon, tCellCon=tCellCon, &
                      tFaceCon=tFaceCon, tEdgeCon=tEdgeCon, tNodeCon=tNodeCon)

CALL Display(cellCon(1:tCellCon), "cellCon:")
CALL Display(faceCon(1:tFaceCon), "faceCon:")
CALL Display(edgeCon(1:tEdgeCon), "edgeCon:")
CALL Display(nodeCon(1:tNodeCon), "nodeCon:")

! Display the content of mesh.
CALL obj%DEALLOCATE()
END PROGRAM main
