!> author: Vikas Sharma, Ph. D.
! date: 2025-06-05
! summary:  Test for SetSparsity method of FEMesh class
!

PROGRAM main
USE GlobalData
USE FEMesh_Class
USE HDF5File_Class
USE BaseType, ONLY: DOF_, CSRMatrix_
USE DOF_Method
USE CSRMatrix_Method

IMPLICIT NONE
TYPE(FEMesh_) :: obj
TYPE(HDF5File_) :: meshfile
TYPE(DOF_) :: dofobj
TYPE(CSRMatrix_) :: mat
INTEGER(I4B) :: tNodes(1)

INTEGER(I4B), PARAMETER :: spaceCompo(1) = [1], &
                           timeCompo(1) = [1], &
                           storageFMT = NODES_FMT
CHARACTER(1), PARAMETER :: names(1) = ["K"]

CHARACTER(LEN=*), PARAMETER :: filename = &
                               "./meshdata/small_tri3_mesh.h5"

CALL meshfile%Initiate(FileName=filename, MODE="READ")

CALL meshfile%OPEN()

CALL obj%Initiate(hdf5=meshfile, dim=2)

tNodes = obj%GetTotalNodes()
CALL Initiate(obj=dofobj, tNodes=tNodes, &
              spaceCompo=spaceCompo, timeCompo=timeCompo, &
              names=names, storageFMT=storageFMT)

CALL Initiate(mat, ncol=SIZE(dofobj), nrow=SIZE(dofobj), &
              idof=dofobj, jdof=dofobj)

CALL obj%SetSparsity(mat=mat)

CALL obj%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
