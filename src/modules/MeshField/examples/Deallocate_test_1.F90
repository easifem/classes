!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Deallocate a vector of mesh fields
!
! MeshFieldDeallocate deallocates every mesh field in the vector, and
! then deallocates the vector itself.

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
USE HDF5File_Class, ONLY: HDF5File_
USE MeshField_Class, ONLY: MeshField_
USE MeshField_Class, ONLY: MeshFieldDeallocate
USE MeshField_Class, ONLY: ScalarMeshFieldInitiate
USE Test_Method, ONLY: OK

IMPLICIT NONE

CHARACTER(*), PARAMETER :: &
  filename = "./meshdata/small_tri3_mesh.h5"
INTEGER(I4B), PARAMETER :: nsd = 2, tfields = 3

TYPE(HDF5File_) :: meshfile
TYPE(FEMesh_) :: amesh
TYPE(MeshField_), ALLOCATABLE :: obj(:)
INTEGER(I4B) :: ii

CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

CALL meshfile%Initiate(fileName=filename, MODE="READ")
CALL meshfile%OPEN()
CALL amesh%Initiate(hdf5=meshfile, dim=nsd)

ALLOCATE (obj(tfields))
DO ii = 1, tfields
  CALL ScalarMeshFieldInitiate( &
    obj=obj(ii), name="field", fieldType=fieldopt%normal, &
    varType=fevaropt%constant, engine=engineopt%native_serial, &
    defineOn=fevaropt%nodal, nns=1, mesh=amesh)
END DO

CALL OK(obj(tfields)%IsInitiated(), "IsInitiated")

CALL MeshFieldDeallocate(obj)
CALL OK(.NOT. ALLOCATED(obj), "MeshFieldDeallocate")

CALL amesh%DEALLOCATE()
CALL meshfile%CLOSE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
