PROGRAM main
USE FEMesh_Class
USE HDF5File_Class
USE GlobalData, ONLY: DFP, I4B, LGT
USE Display_Method
USE ExceptionHandler_Class, ONLY: e
USE ApproxUtility
USE Test_Method

IMPLICIT NONE

CHARACTER(*), PARAMETER :: filename = &
                  "../../Mesh/examples/meshdata/small_tri3_mesh_two_region.h5"
INTEGER(I4B), PARAMETER :: nsd = 2

TYPE(FEMesh_) :: obj
TYPE(HDF5File_) :: meshfile
INTEGER(I4B) :: globalNode
REAL(DFP) :: qv(3), x(3)
LOGICAL(LGT) :: isok

CALL e%SetQuietMode(.TRUE.)

CALL meshfile%Initiate(filename, "READ")
CALL meshfile%OPEN()
CALL obj%Initiate(hdf5=meshfile, dim=nsd)

qv = [1.0, 0.0, 0.0]
CALL obj%GetNearestNode(qv=qv, x=x, globalNode=globalNode)

isok = globalNode == 2
CALL ok(isok, "GetNearestNode1: ")
IF (.NOT. isok) THEN
  CALL Display(msg="found: ", val=globalNode)
  CALL Display(msg="want: ", val=2)
END IF

isok = ALL(x.approxeq.qv)
CALL ok(isok, "GetNearestNode1: ")
IF (.NOT. isok) THEN
  CALL Display(msg="found: ", val=x)
  CALL Display(msg="want: ", val=qv)
END IF

qv = [1.5, 0.3, 0.0]
CALL obj%GetNearestNode(qv=qv, x=x, globalNode=globalNode)

isok = globalNode == 40
CALL ok(isok, "GetNearestNode1: ")
IF (.NOT. isok) THEN
  CALL Display(msg="found: ", val=globalNode)
  CALL Display(msg="want: ", val=2)
END IF

qv = [1.5, 0.25, 0.0]
isok = ALL(x.approxeq.qv)
CALL ok(isok, "GetNearestNode1: ")
IF (.NOT. isok) THEN
  CALL Display(msg="found: ", val=x)
  CALL Display(msg="want: ", val=qv)
END IF

CALL meshfile%DEALLOCATE()
CALL obj%DEALLOCATE()

END PROGRAM main
