!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Import a mesh field from a HDF5 file
!
! We export a mesh field to a HDF5 file, then we import it in another
! mesh field and compare the data. The mesh is not stored in the HDF5
! file, so it is given to Import.

PROGRAM main
USE AbstractMesh_Class, ONLY: AbstractMesh_
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
  filename = "./meshdata/small_tri3_mesh.h5", &
  outfilename = "Import_test_1.h5"
INTEGER(I4B), PARAMETER :: nsd = 2
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP

TYPE(HDF5File_) :: meshfile, outfile
TYPE(FEMesh_), TARGET :: amesh
TYPE(MeshField_) :: obj, obj2
TYPE(FEVariable_) :: fevar, found1, found2
CLASS(AbstractMesh_), POINTER :: meshptr
INTEGER(I4B) :: iel, telements, nns, nrow, ncol
REAL(DFP), ALLOCATABLE :: xij(:, :)
LOGICAL(LGT) :: isok

CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

CALL meshfile%Initiate(fileName=filename, MODE="READ")
CALL meshfile%OPEN()
CALL amesh%Initiate(hdf5=meshfile, dim=nsd)

nns = amesh%GetMaxNNE()
telements = amesh%GetTotalElements()

! A scalar field which varies in space, value = x coordinate of node
CALL obj%Initiate( &
  name="scalarMeshField", fieldType=fieldopt%normal, &
  engine=engineopt%native_serial, defineOn=fevaropt%nodal, &
  varType=fevaropt%space, rank=fevaropt%scalar, s=[nns], mesh=amesh)

ALLOCATE (xij(3, nns))
xij = 0.0_DFP
DO iel = 1, telements
  CALL amesh%GetNodeCoord(nodeCoord=xij, nrow=nrow, ncol=ncol, &
                          globalElement=iel, islocal=math%yes)
  fevar = NodalVariable(xij(1, 1:ncol), TypeFEVariableScalar, &
                        TypeFEVariableSpace)
  CALL obj%Insert(globalElement=iel, islocal=math%yes, fevar=fevar)
END DO

! Export
CALL outfile%Initiate(fileName=outfilename, MODE="NEW")
CALL outfile%OPEN()
CALL obj%Export(hdf5=outfile, group="/meshField")
CALL outfile%CLOSE()
CALL outfile%DEALLOCATE()

! Import
CALL outfile%Initiate(fileName=outfilename, MODE="READ")
CALL outfile%OPEN()
CALL obj2%IMPORT(hdf5=outfile, group="/meshField", mesh=amesh)
CALL outfile%CLOSE()
CALL outfile%DEALLOCATE()

CALL OK(obj2%IsInitiated(), "IsInitiated")
CALL OK(obj2%GetTotalElements() .EQ. obj%GetTotalElements(), &
        "GetTotalElements")
CALL OK(obj2%GetRank() .EQ. obj%GetRank(), "GetRank")
CALL OK(obj2%GetVarType() .EQ. obj%GetVarType(), "GetVarType")
CALL OK(obj2%GetDefineOn() .EQ. obj%GetDefineOn(), "GetDefineOn")
CALL OK(obj2%GetTotalShape() .EQ. obj%GetTotalShape(), "GetTotalShape")
CALL OK(obj2%GetMaxNNE() .EQ. obj%GetMaxNNE(), "GetMaxNNE")

meshptr => obj2%GetMeshPointer()
CALL OK(ASSOCIATED(meshptr, amesh), "GetMeshPointer")

isok = .TRUE.
DO iel = 1, telements
  CALL obj%Get(globalElement=iel, islocal=math%yes, fevar=found1)
  CALL obj2%Get(globalElement=iel, islocal=math%yes, fevar=found2)
  isok = isok .AND. (found1%len .EQ. found2%len)
  isok = isok .AND. ALL(ABS(found1%val(1:found1%len) - &
                            found2%val(1:found2%len)) .LT. tol)
END DO
CALL OK(isok, "imported values")

NULLIFY (meshptr)
CALL obj%DEALLOCATE()
CALL obj2%DEALLOCATE()
CALL amesh%DEALLOCATE()
CALL meshfile%CLOSE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
