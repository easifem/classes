!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Fill a vector mesh field with quadrature points
!
! SetQuadraturePoints fills an initiated (but empty) vector mesh field
! with the physical coordinates of quadrature points. The mesh field
! must be a vector field with spaceCompo = 3, varType = space, and
! nns = fedof%GetMaxTotalQuadraturePoints(). The result is the same as
! InitiateQuadraturePoints.

PROGRAM main
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: math => TypeMathOpt
USE EngineOpt_Class, ONLY: engineopt => TypeEngineOpt
USE ExceptionHandler_Class, ONLY: e
USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE FieldOpt_Class, ONLY: fieldopt => TypeFieldOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE MeshField_Class, ONLY: InitiateQuadraturePoints
USE MeshField_Class, ONLY: MeshField_
USE MeshField_Class, ONLY: SetQuadraturePoints
USE MeshField_Class, ONLY: VectorMeshFieldInitiate
USE ScalarField_Class, ONLY: ScalarField_
USE Test_Method, ONLY: OK

IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "MeshField_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP

TYPE(FEDomain_) :: dom
TYPE(FEDOF_) :: fedof, geofedof
TYPE(ScalarField_) :: u
TYPE(MeshField_) :: obj, want
CLASS(AbstractMesh_), POINTER :: mesh
INTEGER(I4B) :: iel, telements, maxpts, nrow, ncol, mrow, mcol
REAL(DFP), ALLOCATABLE :: x1(:, :), x2(:, :)
LOGICAL(LGT) :: isok

CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)

CALL dom%ImportFromToml(fileName=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      fileName=tomlFileName, tomlName="u")
mesh => dom%GetMeshPointer()
telements = mesh%GetTotalElements()
maxpts = fedof%GetMaxTotalQuadraturePoints()

CALL VectorMeshFieldInitiate( &
  obj=obj, name="xij", fieldType=fieldopt%normal, &
  varType=fevaropt%space, engine=engineopt%native_serial, &
  defineOn=fevaropt%quadrature, spaceCompo=3, nns=maxpts, mesh=mesh)

CALL SetQuadraturePoints(obj=obj, fedof=fedof, geofedof=geofedof, &
                         mesh=mesh)

CALL InitiateQuadraturePoints(obj=want, fedof=fedof, mesh=mesh, &
                              geofedof=geofedof, &
                              engine=engineopt%native_serial)

ALLOCATE (x1(3, maxpts), x2(3, maxpts))
isok = .TRUE.
DO iel = 1, telements
  CALL obj%GetSpaceVectorField_(globalElement=iel, ans=x1, nrow=nrow, &
                                ncol=ncol, islocal=math%yes)
  CALL want%GetSpaceVectorField_(globalElement=iel, ans=x2, nrow=mrow, &
                                 ncol=mcol, islocal=math%yes)
  isok = isok .AND. (nrow .EQ. mrow) .AND. (ncol .EQ. mcol)
  isok = isok .AND. &
         ALL(ABS(x1(1:nrow, 1:ncol) - x2(1:nrow, 1:ncol)) .LT. tol)
END DO
CALL OK(isok, "SetQuadraturePoints")

NULLIFY (mesh)
CALL obj%DEALLOCATE()
CALL want%DEALLOCATE()
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
