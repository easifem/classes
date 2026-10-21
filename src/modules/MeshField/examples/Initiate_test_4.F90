!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Initiate a scalar mesh field from a user function
!
! The rank and varType of the mesh field are taken from the return type
! and argument type of the user function. Initiate only allocates the
! data. Insert(func=func) fills every element, and Set(func=func)
! updates the values of an already filled mesh field.

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: math => TypeMathOpt
USE EngineOpt_Class, ONLY: engineopt => TypeEngineOpt
USE ExceptionHandler_Class, ONLY: e
USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
USE FEMesh_Class, ONLY: FEMesh_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE HDF5File_Class, ONLY: HDF5File_
USE MeshField_Class, ONLY: MeshField_
USE Test_Method, ONLY: OK
USE UserFunction_Class, ONLY: UserFunction_

IMPLICIT NONE

CHARACTER(*), PARAMETER :: &
  filename = "./meshdata/small_tri3_mesh.h5"
INTEGER(I4B), PARAMETER :: nsd = 2
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP

TYPE(HDF5File_) :: meshfile
TYPE(FEMesh_) :: amesh
TYPE(MeshField_) :: obj
TYPE(UserFunction_) :: func
TYPE(FEVariable_) :: found
INTEGER(I4B) :: iel, telements
LOGICAL(LGT) :: isok

CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

CALL meshfile%Initiate(fileName=filename, MODE="READ")
CALL meshfile%OPEN()
CALL amesh%Initiate(hdf5=meshfile, dim=nsd)
telements = amesh%GetTotalElements()

CALL func%Initiate(name="func", returnType=fevaropt%scalar, &
                   argType=fevaropt%constant)
CALL func%SetScalarConstantVal(val=2.0_DFP)

CALL obj%Initiate(mesh=amesh, func=func, name="func", &
                  engine=engineopt%native_serial)

CALL OK(obj%GetRank() .EQ. fevaropt%scalar, "GetRank")
CALL OK(obj%GetVarType() .EQ. fevaropt%constant, "GetVarType")
CALL OK(obj%GetDefineOn() .EQ. fevaropt%nodal, "GetDefineOn")
CALL OK(obj%GetTotalElements() .EQ. telements, "GetTotalElements")

! Fill all the elements
CALL obj%Insert(func=func)

isok = .TRUE.
DO iel = 1, telements
  CALL obj%Get(globalElement=iel, islocal=math%yes, fevar=found)
  isok = isok .AND. (ABS(found%val(1) - 2.0_DFP) .LT. tol)
END DO
CALL OK(isok, "Insert(func)")

! Update all the elements
CALL func%SetScalarConstantVal(val=3.0_DFP)
CALL obj%Set(func=func)

isok = .TRUE.
DO iel = 1, telements
  CALL obj%Get(globalElement=iel, islocal=math%yes, fevar=found)
  isok = isok .AND. (ABS(found%val(1) - 3.0_DFP) .LT. tol)
END DO
CALL OK(isok, "Set(func)")

CALL obj%DEALLOCATE()
CALL func%DEALLOCATE()
CALL amesh%DEALLOCATE()
CALL meshfile%CLOSE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
