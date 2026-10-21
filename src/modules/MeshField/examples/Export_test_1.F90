!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Export a mesh field to a HDF5 file
!
! The HDF5 file must be open with write permission (mode NEW, WRITE or
! OVERWRITE). The data is written inside the given group.

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeFEVariableConstant
USE BaseType, ONLY: TypeFEVariableScalar
USE EngineOpt_Class, ONLY: engineopt => TypeEngineOpt
USE ExceptionHandler_Class, ONLY: e
USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
USE FEMesh_Class, ONLY: FEMesh_
USE FEVariable_Method, ONLY: NodalVariable
USE FieldOpt_Class, ONLY: fieldopt => TypeFieldOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE HDF5File_Class, ONLY: HDF5File_
USE MeshField_Class, ONLY: MeshField_
USE Test_Method, ONLY: OK

IMPLICIT NONE

CHARACTER(*), PARAMETER :: &
  filename = "./meshdata/small_tri3_mesh.h5", &
  outfilename = "Export_test_1.h5"
INTEGER(I4B), PARAMETER :: nsd = 2

TYPE(HDF5File_) :: meshfile, outfile
TYPE(FEMesh_) :: amesh
TYPE(MeshField_) :: obj
TYPE(FEVariable_) :: fevar
INTEGER(I4B) :: iel, telements
LOGICAL(LGT) :: isok

CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

CALL meshfile%Initiate(fileName=filename, MODE="READ")
CALL meshfile%OPEN()
CALL amesh%Initiate(hdf5=meshfile, dim=nsd)
telements = amesh%GetTotalElements()

CALL obj%Initiate( &
  name="scalarMeshField", fieldType=fieldopt%normal, &
  engine=engineopt%native_serial, defineOn=fevaropt%nodal, &
  varType=fevaropt%constant, rank=fevaropt%scalar, s=[1], mesh=amesh)

DO iel = 1, telements
  fevar = NodalVariable(REAL(iel, DFP), TypeFEVariableScalar, &
                        TypeFEVariableConstant)
  CALL obj%Insert(globalElement=iel, islocal=math%yes, fevar=fevar)
END DO

CALL outfile%Initiate(fileName=outfilename, MODE="NEW")
CALL outfile%OPEN()
CALL obj%Export(hdf5=outfile, group="/meshField")
CALL outfile%CLOSE()
CALL outfile%DEALLOCATE()

INQUIRE (FILE=outfilename, EXIST=isok)
CALL OK(isok, "Export")

CALL obj%DEALLOCATE()
CALL amesh%DEALLOCATE()
CALL meshfile%CLOSE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
