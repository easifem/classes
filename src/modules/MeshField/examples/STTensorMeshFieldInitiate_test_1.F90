!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Initiate a space-time tensor (matrix) mesh field
!
! For varType = spaceTime the data in each element has the shape
! (dim1, dim2, nns, nnt). For varType = constant it has the shape
! (dim1, dim2).

PROGRAM main
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: math => TypeMathOpt
USE EngineOpt_Class, ONLY: engineopt => TypeEngineOpt
USE ExceptionHandler_Class, ONLY: e
USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
USE FEMesh_Class, ONLY: FEMesh_
USE FieldOpt_Class, ONLY: fieldopt => TypeFieldOpt
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE HDF5File_Class, ONLY: HDF5File_
USE MeshField_Class, ONLY: MeshField_
USE MeshField_Class, ONLY: STTensorMeshFieldInitiate
USE Test_Method, ONLY: OK

IMPLICIT NONE

CHARACTER(*), PARAMETER :: &
  filename = "./meshdata/small_tri3_mesh.h5"
INTEGER(I4B), PARAMETER :: nsd = 2, dim1 = 2, dim2 = 2, nnt = 2

TYPE(HDF5File_) :: meshfile
TYPE(FEMesh_) :: amesh
TYPE(MeshField_) :: obj
INTEGER(I4B) :: nns

CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

CALL meshfile%Initiate(fileName=filename, MODE="READ")
CALL meshfile%OPEN()
CALL amesh%Initiate(hdf5=meshfile, dim=nsd)

nns = amesh%GetMaxNNE()

CALL STTensorMeshFieldInitiate( &
  obj=obj, name="stTensor", fieldType=fieldopt%normal, &
  varType=fevaropt%spaceTime, engine=engineopt%native_serial, &
  defineOn=fevaropt%nodal, dim1=dim1, dim2=dim2, nns=nns, nnt=nnt, &
  mesh=amesh)

CALL OK(obj%IsInitiated(), "IsInitiated")
CALL OK(obj%GetRank() .EQ. fevaropt%matrix, "GetRank")
CALL OK(obj%GetVarType() .EQ. fevaropt%spaceTime, "GetVarType")
CALL OK(obj%GetTotalShape() .EQ. 4, "GetTotalShape")
CALL OK(obj%GetMaxNNE() .EQ. nns, "GetMaxNNE")

CALL STTensorMeshFieldInitiate( &
  obj=obj, name="constTensor", fieldType=fieldopt%normal, &
  varType=fevaropt%constant, engine=engineopt%native_serial, &
  defineOn=fevaropt%nodal, dim1=dim1, dim2=dim2, nns=nns, nnt=nnt, &
  mesh=amesh)
CALL OK(obj%GetTotalShape() .EQ. 2, "GetTotalShape (constant)")

CALL obj%DEALLOCATE()
CALL amesh%DEALLOCATE()
CALL meshfile%CLOSE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
