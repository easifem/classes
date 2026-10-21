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
INTEGER(I4B), ALLOCATABLE :: nptrs(:)
REAL(DFP), ALLOCATABLE :: found(:, :), want(:, :)
LOGICAL(LGT) :: isok

CALL e%SetQuietMode(.TRUE.)

CALL meshfile%Initiate(FileName=filename, MODE="READ")
CALL meshfile%OPEN()

CALL obj%Initiate(hdf5=meshfile, dim=nsd)
CALL obj%DisplayMeshInfo("Mesh Info")

CALL obj%GetNodeCoord(nodeCoord=want, group="", hdf5=meshfile)

tnodes = obj%GetTotalNodes()
CALL Reallocate(found, 3, tnodes)
CALL Reallocate(nptrs, tnodes)
CALL obj%GetNodeCoord(nodeCoord=found, nrow=nrow, ncol=ncol, &
                      globalElement=1, islocal=.TRUE.)

tnodes = obj%GetNNE(globalElement=1, islocal=.TRUE.)
CALL obj%GetConnectivity_(globalElement=1, islocal=.TRUE., ans=nptrs, &
                          tsize=ncol)

CALL OK(nrow .EQ. 3, "GetNodeCoord (1)")
CALL OK(ncol .EQ. tnodes, "GetNodeCoord (2)")
DO ii = 1, ncol
  isok = ALL(found(:, ii) .approxeq.want(:, nptrs(ii)))
  IF (.NOT. isok) EXIT
END DO
CALL OK(isok, "GetNodeCoord (3)")

CALL obj%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
