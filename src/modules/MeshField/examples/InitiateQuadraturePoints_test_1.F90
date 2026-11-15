!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Vector mesh field of quadrature points
!
! InitiateQuadraturePoints creates a vector mesh field (spaceCompo=3,
! varType=space, defineOn=quadrature) and stores the physical
! coordinates of the quadrature points of each element. The quadrature
! points are taken from fedof, and they are mapped to the physical
! element by using geofedof. Every quadrature point must lie inside the
! bounding box of its element.

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
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE MeshField_Class, ONLY: InitiateQuadraturePoints
USE MeshField_Class, ONLY: MeshField_
USE ScalarField_Class, ONLY: ScalarField_
USE Test_Method, ONLY: OK

IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "MeshField_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP

TYPE(FEDomain_) :: dom
TYPE(FEDOF_) :: fedof, geofedof
TYPE(ScalarField_) :: u
TYPE(MeshField_) :: obj
CLASS(AbstractMesh_), POINTER :: mesh
INTEGER(I4B) :: iel, telements, nsd, nrow, ncol, mrow, mcol, ii
REAL(DFP), ALLOCATABLE :: xij(:, :), elemCoord(:, :)
LOGICAL(LGT) :: isok

CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)

CALL dom%ImportFromToml(fileName=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      fileName=tomlFileName, tomlName="u")
mesh => dom%GetMeshPointer()
telements = mesh%GetTotalElements()
nsd = mesh%GetNSD()

CALL InitiateQuadraturePoints(obj=obj, fedof=fedof, mesh=mesh, &
                              geofedof=geofedof, &
                              engine=engineopt%native_serial)

CALL OK(obj%GetRank() .EQ. fevaropt%vector, "GetRank")
CALL OK(obj%GetDefineOn() .EQ. fevaropt%quadrature, "GetDefineOn")
CALL OK(obj%GetMaxNNE() .EQ. fedof%GetMaxTotalQuadraturePoints(), &
        "GetMaxNNE")

ALLOCATE (xij(3, obj%GetMaxNNE()), elemCoord(3, mesh%GetMaxNNE()))
elemCoord = 0.0_DFP

isok = .TRUE.
DO iel = 1, telements
  CALL mesh%GetNodeCoord(nodeCoord=elemCoord, nrow=nrow, ncol=ncol, &
                         globalElement=iel, islocal=math%yes)
  CALL obj%GetSpaceVectorField_(globalElement=iel, ans=xij, nrow=mrow, &
                                ncol=mcol, islocal=math%yes)
  DO ii = 1, nsd
    isok = isok .AND. &
           ALL(xij(ii, 1:mcol) .GE. MINVAL(elemCoord(ii, 1:ncol)) - tol)
    isok = isok .AND. &
           ALL(xij(ii, 1:mcol) .LE. MAXVAL(elemCoord(ii, 1:ncol)) + tol)
  END DO
END DO
CALL OK(isok, "quadrature points inside the element")

NULLIFY (mesh)
CALL obj%DEALLOCATE()
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
