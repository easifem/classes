!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Add values to a mesh field
!
! Add performs obj = obj + scale * fevar.
!
! - Add(scale, fevar) adds to all the elements.
! - Add(globalElement, islocal, scale, fevar) adds to one element.

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
INTEGER(I4B) :: iel, telements
REAL(DFP) :: want
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

fevar = NodalVariable(1.0_DFP, TypeFEVariableScalar, &
                      TypeFEVariableConstant)

! obj = 1 in all elements
CALL obj%Insert(fevar=fevar)

! obj = 1 + 2 * 1 = 3 in all elements
CALL obj%Add(scale=2.0_DFP, fevar=fevar)

! obj = 3 + 1 * 1 = 4 in element 1
CALL obj%Add(globalElement=1, islocal=math%yes, scale=1.0_DFP, &
             fevar=fevar)

isok = .TRUE.
DO iel = 1, telements
  want = 3.0_DFP
  IF (iel .EQ. 1) want = 4.0_DFP
  CALL obj%Get(globalElement=iel, islocal=math%yes, fevar=found)
  isok = isok .AND. (ABS(found%val(1) - want) .LT. tol)
END DO
CALL OK(isok, "Add")

CALL obj%DEALLOCATE()
CALL amesh%DEALLOCATE()
CALL meshfile%CLOSE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
