!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Initiate a scalar mesh field which varies in space
!
! varType = space, so each element stores one value per node.
! s = [nns] gives the maximum number of nodes in an element.
! We insert the x coordinate of each node, and read it back by using
! GetSpaceScalarField_.

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
TYPE(FEVariable_) :: fevar
INTEGER(I4B) :: iel, telements, nns, nrow, ncol, tsize, s(4)
REAL(DFP), ALLOCATABLE :: xij(:, :), found(:)
LOGICAL(LGT) :: isok

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

CALL OK(obj%GetMaxNNE() .EQ. nns, "GetMaxNNE")
CALL OK(obj%GetTotalShape() .EQ. 1, "GetTotalShape")

ALLOCATE (xij(3, nns), found(nns))
xij = 0.0_DFP

DO iel = 1, telements
  CALL amesh%GetNodeCoord(nodeCoord=xij, nrow=nrow, ncol=ncol, &
                          globalElement=iel, islocal=math%yes)
  fevar = NodalVariable(xij(1, 1:ncol), TypeFEVariableScalar, &
                        TypeFEVariableSpace)
  CALL obj%Insert(globalElement=iel, islocal=math%yes, fevar=fevar)
END DO

isok = .TRUE.
DO iel = 1, telements
  CALL amesh%GetNodeCoord(nodeCoord=xij, nrow=nrow, ncol=ncol, &
                          globalElement=iel, islocal=math%yes)
  CALL obj%GetSpaceScalarField_(globalElement=iel, ans=found, &
                                tsize=tsize, islocal=math%yes)
  isok = isok .AND. (tsize .EQ. ncol)
  isok = isok .AND. ALL(ABS(found(1:tsize) - xij(1, 1:ncol)) .LT. tol)
END DO
CALL OK(isok, "GetSpaceScalarField_")

CALL obj%GetShape(globalElement=1, islocal=math%yes, ans=s, tsize=tsize)
CALL OK((tsize .EQ. 1) .AND. (s(1) .EQ. nns), "GetShape")

CALL obj%DEALLOCATE()
CALL amesh%DEALLOCATE()
CALL meshfile%CLOSE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
