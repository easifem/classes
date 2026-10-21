PROGRAM main
USE FEMesh_Class, ONLY: FEMesh_
USE HDF5File_Class, ONLY: HDF5File_
USE GlobalData, ONLY: I4B, LGT, Quadrangle
USE ReallocateUtility, ONLY: Reallocate
USE Display_Method, ONLY: Display
USE Test_Method

IMPLICIT NONE

TYPE(FEMesh_) :: obj
TYPE(HDF5File_) :: meshfile
CHARACTER(LEN=*), PARAMETER :: filename = "../../Mesh/examples/meshdata/small_quad4_mesh.h5"
INTEGER(I4B), PARAMETER :: nsd = 2
INTEGER(I4B) :: found(8), want(8)
LOGICAL(LGT) :: isok
CHARACTER(*), PARAMETER :: testname = "GetTotalElementsTopologyWise"

! Initiate and open the mesh file which is in `HDF5File_` format.
CALL meshfile%Initiate(FileName=filename, MODE="READ")
! Open the mesh file
CALL meshfile%OPEN()
! Initiate an instance of `Mesh_`
CALL obj%Initiate(hdf5=meshfile, dim=nsd)
CALL meshfile%DEALLOCATE()

found = obj%GetTotalElementsTopologyWise()
CALL Display("Get total elements topology wise")
! CALL Display(found, "Get total elements topologywise:")
isok = found(4) .EQ. 16
CALL OK(isok, testname)

found(1) = obj%GetTotalTopology()
want(1) = 1
isok = found(1) .EQ. want(1)
CALL ok(isok, testname)

found = obj%GetElemTopology()
want(1) = Quadrangle
isok = found(1) .EQ. want(1)
CALL ok(isok, testname)

CALL obj%DEALLOCATE()
END PROGRAM main

! total nodes = 25
! total elements = 16
! total faces = 40
! total edges = 0
