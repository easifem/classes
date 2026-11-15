!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Initiate a mesh field by copying another mesh field
!
! Initiate(obj2=...) copies all the data of obj2 into obj. The mesh
! pointer of obj points to the same mesh as obj2.

PROGRAM main
USE AbstractMesh_Class, ONLY: AbstractMesh_
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
TYPE(FEMesh_), TARGET :: amesh
TYPE(MeshField_) :: obj, obj2
TYPE(FEVariable_) :: fevar, found
CLASS(AbstractMesh_), POINTER :: meshptr
INTEGER(I4B) :: iel, telements
LOGICAL(LGT) :: isok

CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

CALL meshfile%Initiate(fileName=filename, MODE="READ")
CALL meshfile%OPEN()
CALL amesh%Initiate(hdf5=meshfile, dim=nsd)
telements = amesh%GetTotalElements()

CALL obj2%Initiate( &
  name="scalarMeshField", fieldType=fieldopt%normal, &
  engine=engineopt%native_serial, defineOn=fevaropt%nodal, &
  varType=fevaropt%constant, rank=fevaropt%scalar, s=[1], mesh=amesh)

DO iel = 1, telements
  fevar = NodalVariable(REAL(iel, DFP), TypeFEVariableScalar, &
                        TypeFEVariableConstant)
  CALL obj2%Insert(globalElement=iel, islocal=math%yes, fevar=fevar)
END DO

CALL obj%Initiate(obj2=obj2)

CALL OK(obj%IsInitiated(), "IsInitiated")
CALL OK(obj%GetTotalElements() .EQ. telements, "GetTotalElements")

meshptr => obj%GetMeshPointer()
CALL OK(ASSOCIATED(meshptr, amesh), "GetMeshPointer")

isok = .TRUE.
DO iel = 1, telements
  CALL obj%Get(globalElement=iel, islocal=math%yes, fevar=found)
  isok = isok .AND. (ABS(found%val(1) - REAL(iel, DFP)) .LT. tol)
END DO
CALL OK(isok, "copied values")

NULLIFY (meshptr)
CALL obj%DEALLOCATE()
CALL obj2%DEALLOCATE()
CALL amesh%DEALLOCATE()
CALL meshfile%CLOSE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
