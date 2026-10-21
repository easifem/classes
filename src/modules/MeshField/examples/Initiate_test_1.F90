!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Initiate a constant scalar mesh field (fieldType=constant)
!
! The field has the same value in every element, so only one entry is
! stored (tSize = 1). The data is constant inside the element
! (varType = constant), therefore s = [1].

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
  filename = "./meshdata/small_tri3_mesh.h5"
INTEGER(I4B), PARAMETER :: nsd = 2
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP

TYPE(HDF5File_) :: meshfile
TYPE(FEMesh_) :: amesh
TYPE(MeshField_) :: obj
TYPE(FEVariable_) :: fevar, found
LOGICAL(LGT) :: isok

CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

CALL meshfile%Initiate(fileName=filename, MODE="READ")
CALL meshfile%OPEN()
CALL amesh%Initiate(hdf5=meshfile, dim=nsd)

CALL obj%Initiate( &
  name="scalarMeshField", fieldType=fieldopt%constant, &
  engine=engineopt%native_serial, defineOn=fevaropt%nodal, &
  varType=fevaropt%constant, rank=fevaropt%scalar, s=[1], mesh=amesh)

CALL OK(obj%IsInitiated(), "IsInitiated")
CALL OK(obj%GetTotalElements() .EQ. 1, "GetTotalElements (constant)")
CALL OK(obj%GetRank() .EQ. fevaropt%scalar, "GetRank")
CALL OK(obj%GetVarType() .EQ. fevaropt%constant, "GetVarType")
CALL OK(obj%GetDefineOn() .EQ. fevaropt%nodal, "GetDefineOn")
CALL OK(obj%GetTotalShape() .EQ. 1, "GetTotalShape")
CALL OK(obj%GetMaxNNE() .EQ. 1, "GetMaxNNE")

fevar = NodalVariable(10.0_DFP, TypeFEVariableScalar, &
                      TypeFEVariableConstant)
CALL obj%Insert(globalElement=1, islocal=math%yes, fevar=fevar)

! For a constant field every element returns the same value
CALL obj%Get(globalElement=5, islocal=math%yes, fevar=found)
isok = (found%len .EQ. 1) .AND. &
       (ABS(found%val(1) - 10.0_DFP) .LT. tol)
CALL OK(isok, "Get value of constant field")

CALL obj%DEALLOCATE()
CALL OK(.NOT. obj%IsInitiated(), "Deallocate")

CALL amesh%DEALLOCATE()
CALL meshfile%CLOSE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
