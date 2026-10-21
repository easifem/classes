!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Set values in a mesh field by using FEVariable
!
! Set updates the values of a mesh field which is already filled by
! Insert. The size of data in an element must not change.
!
! - Insert(fevar) fills all the elements with the same FEVariable.
! - Set(globalElement, islocal, fevar) updates one element.
! - Set(fevar) updates all the elements.

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeFEVariableScalar
USE BaseType, ONLY: TypeFEVariableSpace
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
INTEGER(I4B) :: telements, nns
REAL(DFP), ALLOCATABLE :: val(:)

CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

CALL meshfile%Initiate(fileName=filename, MODE="READ")
CALL meshfile%OPEN()
CALL amesh%Initiate(hdf5=meshfile, dim=nsd)

nns = amesh%GetMaxNNE()
telements = amesh%GetTotalElements()

CALL obj%Initiate( &
  name="scalarMeshField", fieldType=fieldopt%normal, &
  engine=engineopt%native_serial, defineOn=fevaropt%nodal, &
  varType=fevaropt%space, rank=fevaropt%scalar, s=[nns], mesh=amesh)

ALLOCATE (val(nns))

! Fill all elements with 1.0
val = 1.0_DFP
fevar = NodalVariable(val, TypeFEVariableScalar, TypeFEVariableSpace)
CALL obj%Insert(fevar=fevar)
CALL OK(Check(1.0_DFP, 1.0_DFP), "Insert(fevar)")

! Update element 1 with 5.0
val = 5.0_DFP
fevar = NodalVariable(val, TypeFEVariableScalar, TypeFEVariableSpace)
CALL obj%Set(globalElement=1, islocal=math%yes, fevar=fevar)
CALL OK(Check(5.0_DFP, 1.0_DFP), "Set(globalElement, fevar)")

! Update all elements with 7.0
val = 7.0_DFP
fevar = NodalVariable(val, TypeFEVariableScalar, TypeFEVariableSpace)
CALL obj%Set(fevar=fevar)
CALL OK(Check(7.0_DFP, 7.0_DFP), "Set(fevar)")

CALL obj%DEALLOCATE()
CALL amesh%DEALLOCATE()
CALL meshfile%CLOSE()
CALL meshfile%DEALLOCATE()

CONTAINS

! Element 1 should contain first, all other elements should contain rest
FUNCTION Check(first, rest) RESULT(ans)
  REAL(DFP), INTENT(IN) :: first, rest
  LOGICAL(LGT) :: ans
  INTEGER(I4B) :: jj
  REAL(DFP) :: want

  ans = .TRUE.
  DO jj = 1, telements
    want = rest
    IF (jj .EQ. 1) want = first
    CALL obj%Get(globalElement=jj, islocal=math%yes, fevar=found)
    ans = ans .AND. (found%len .EQ. nns)
    ans = ans .AND. ALL(ABS(found%val(1:found%len) - want) .LT. tol)
  END DO
END FUNCTION Check

END PROGRAM main
