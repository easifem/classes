PROGRAM main
USE GlobalData, ONLY: DFP, I4B, LGT
USE FEMesh_Class
USE HDF5File_Class
USE Test_Method
USE ReallocateUtility
USE ExceptionHandler_Class, ONLY: e
USE Display_Method
USE ApproxUtility

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER :: filename = &
                       "../../Mesh/examples/meshdata/small_mesh_two_region.h5"
INTEGER(I4B), PARAMETER :: nsd = 2

TYPE(FEMesh_) :: obj
TYPE(HDF5File_) :: meshfile
INTEGER(I4B) :: tnodes, nrow, ncol, ii
REAL(DFP), ALLOCATABLE :: xij(:, :), xij2(:, :)
LOGICAL(LGT) :: isok

CALL e%SetQuietMode(.TRUE.)

CALL meshfile%Initiate(FileName=filename, MODE="READ")
CALL meshfile%OPEN()

CALL obj%Initiate(hdf5=meshfile, dim=nsd)
CALL obj%DisplayMeshInfo("Mesh Info")

tnodes = obj%GetTotalNodes()

! CALL Reallocate(xij, 3, tnodes)
CALL obj%GetNodeCoord(hdf5=meshfile, group="/nodeCoord", nodeCoord=xij)

nrow = SIZE(xij, 1)
ncol = SIZE(xij, 2)
CALL OK(nrow .EQ. 3, "GetNodeCoord (1)")
CALL OK(ncol .EQ. tnodes, "GetNodeCoord (2)")

CALL obj%GetNodeCoord(hdf5=meshfile, group="", nodeCoord=xij2)

nrow = SIZE(xij2, 1)
ncol = SIZE(xij2, 2)
CALL OK(nrow .EQ. 3, "GetNodeCoord (3)")
CALL OK(ncol .EQ. tnodes, "GetNodeCoord (4)")

DO ii = 1, ncol
  isok = ALL(xij(:, ii) .approxeq.xij2(:, ii))
  IF (.NOT. isok) EXIT
END DO

CALL OK(isok, "GetNodeCoord (5)")

CALL obj%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
