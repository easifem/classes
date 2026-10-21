!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Initiate a vector mesh field which varies in space
!
! The data in each element is a matrix of shape (spaceCompo, nns).
! We store the nodal coordinates of each element and read them back
! by using GetSpaceVectorField_ and Shape.

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeFEVariableSpace
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
USE MeshField_Class, ONLY: VectorMeshFieldInitiate
USE Test_Method, ONLY: OK

IMPLICIT NONE

CHARACTER(*), PARAMETER :: &
  filename = "./meshdata/small_tri3_mesh.h5"
INTEGER(I4B), PARAMETER :: nsd = 2
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP

TYPE(HDF5File_) :: meshfile
TYPE(FEMesh_) :: amesh
TYPE(MeshField_) :: obj
TYPE(FEVariable_) :: fevar
INTEGER(I4B) :: iel, telements, nns, nrow, ncol, mrow, mcol
INTEGER(I4B), ALLOCATABLE :: s(:)
REAL(DFP), ALLOCATABLE :: xij(:, :), found(:, :)
LOGICAL(LGT) :: isok

CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

CALL meshfile%Initiate(fileName=filename, MODE="READ")
CALL meshfile%OPEN()
CALL amesh%Initiate(hdf5=meshfile, dim=nsd)

nns = amesh%GetMaxNNE()
telements = amesh%GetTotalElements()

CALL VectorMeshFieldInitiate( &
  obj=obj, name="xij", fieldType=fieldopt%normal, &
  varType=fevaropt%space, engine=engineopt%native_serial, &
  defineOn=fevaropt%nodal, spaceCompo=nsd, nns=nns, mesh=amesh)

CALL OK(obj%GetRank() .EQ. fevaropt%vector, "GetRank")
CALL OK(obj%GetTotalShape() .EQ. 2, "GetTotalShape")
CALL OK(obj%GetMaxNNE() .EQ. nns, "GetMaxNNE")

ALLOCATE (xij(3, nns), found(nsd, nns))
xij = 0.0_DFP

DO iel = 1, telements
  CALL amesh%GetNodeCoord(nodeCoord=xij, nrow=nrow, ncol=ncol, &
                          globalElement=iel, islocal=math%yes)
  fevar = NodalVariable(xij(1:nsd, 1:ncol), TypeFEVariableVector, &
                        TypeFEVariableSpace)
  CALL obj%Insert(globalElement=iel, islocal=math%yes, fevar=fevar)
END DO

isok = .TRUE.
DO iel = 1, telements
  CALL amesh%GetNodeCoord(nodeCoord=xij, nrow=nrow, ncol=ncol, &
                          globalElement=iel, islocal=math%yes)
  CALL obj%GetSpaceVectorField_(globalElement=iel, ans=found, &
                                nrow=mrow, ncol=mcol, islocal=math%yes)
  isok = isok .AND. (mrow .EQ. nsd) .AND. (mcol .EQ. ncol)
  isok = isok .AND. ALL(ABS(found(1:mrow, 1:mcol) - &
                            xij(1:nsd, 1:ncol)) .LT. tol)
END DO
CALL OK(isok, "GetSpaceVectorField_")

s = obj%Shape(globalElement=1, islocal=math%yes)
isok = (SIZE(s) .EQ. 2) .AND. (s(1) .EQ. nsd) .AND. (s(2) .EQ. nns)
CALL OK(isok, "Shape")

! A constant vector field stores spaceCompo values in each element
CALL VectorMeshFieldInitiate( &
  obj=obj, name="constVec", fieldType=fieldopt%normal, &
  varType=fevaropt%constant, engine=engineopt%native_serial, &
  defineOn=fevaropt%nodal, spaceCompo=nsd, nns=nns, mesh=amesh)
CALL OK(obj%GetTotalShape() .EQ. 1, "GetTotalShape (constant)")

CALL obj%DEALLOCATE()
CALL amesh%DEALLOCATE()
CALL meshfile%CLOSE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
