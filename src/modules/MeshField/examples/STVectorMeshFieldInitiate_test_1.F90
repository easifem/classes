!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Initiate a space-time vector mesh field
!
! For varType = spaceTime the data in each element has the shape
! (spaceCompo, nns, nnt).

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeFEVariableSpaceTime
USE BaseType, ONLY: TypeFEVariableVector
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
USE MeshField_Class, ONLY: STVectorMeshFieldInitiate
USE Test_Method, ONLY: OK

IMPLICIT NONE

CHARACTER(*), PARAMETER :: &
  filename = "./meshdata/small_tri3_mesh.h5"
INTEGER(I4B), PARAMETER :: nsd = 2, nnt = 2
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP

TYPE(HDF5File_) :: meshfile
TYPE(FEMesh_) :: amesh
TYPE(MeshField_) :: obj
TYPE(FEVariable_) :: fevar, found
INTEGER(I4B) :: iel, telements, nns
INTEGER(I4B), ALLOCATABLE :: s(:)
REAL(DFP), ALLOCATABLE :: val(:, :, :)
LOGICAL(LGT) :: isok

CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

CALL meshfile%Initiate(fileName=filename, MODE="READ")
CALL meshfile%OPEN()
CALL amesh%Initiate(hdf5=meshfile, dim=nsd)

nns = amesh%GetMaxNNE()
telements = amesh%GetTotalElements()

CALL STVectorMeshFieldInitiate( &
  obj=obj, name="stVector", fieldType=fieldopt%normal, &
  varType=fevaropt%spaceTime, engine=engineopt%native_serial, &
  defineOn=fevaropt%nodal, spaceCompo=nsd, nns=nns, nnt=nnt, &
  mesh=amesh)

CALL OK(obj%GetRank() .EQ. fevaropt%vector, "GetRank")
CALL OK(obj%GetTotalShape() .EQ. 3, "GetTotalShape")
CALL OK(obj%GetMaxNNE() .EQ. nns, "GetMaxNNE")

ALLOCATE (val(nsd, nns, nnt))
DO iel = 1, telements
  val = REAL(iel, DFP)
  fevar = NodalVariable(val, TypeFEVariableVector, &
                        TypeFEVariableSpaceTime)
  CALL obj%Insert(globalElement=iel, islocal=math%yes, fevar=fevar)
END DO

isok = .TRUE.
DO iel = 1, telements
  CALL obj%Get(globalElement=iel, islocal=math%yes, fevar=found)
  isok = isok .AND. (found%len .EQ. nsd * nns * nnt)
  isok = isok .AND. &
         ALL(ABS(found%val(1:found%len) - REAL(iel, DFP)) .LT. tol)
END DO
CALL OK(isok, "Get")

s = obj%Shape(globalElement=1, islocal=math%yes)
isok = (SIZE(s) .EQ. 3) .AND. (s(1) .EQ. nsd) .AND. &
       (s(2) .EQ. nns) .AND. (s(3) .EQ. nnt)
CALL OK(isok, "Shape")

CALL obj%DEALLOCATE()
CALL amesh%DEALLOCATE()
CALL meshfile%CLOSE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
