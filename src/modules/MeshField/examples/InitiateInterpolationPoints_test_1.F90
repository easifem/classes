!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Vector mesh field of interpolation points
!
! InitiateInterpolationPoints creates a vector mesh field (spaceCompo=3,
! varType=space, defineOn=nodal) and stores the interpolation points
! of each element in it. For Lagrange equidistance points the first
! points of an element are its vertices.

PROGRAM main
USE AbstractFE_Class, ONLY: AbstractFE_
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE EngineOpt_Class, ONLY: engineopt => TypeEngineOpt
USE ExceptionHandler_Class, ONLY: e
USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE MeshField_Class, ONLY: InitiateInterpolationPoints
USE MeshField_Class, ONLY: MeshField_
USE ScalarField_Class, ONLY: ScalarField_
USE Test_Method, ONLY: OK

IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "MeshField_test.toml"
INTEGER(I4B), PARAMETER :: order(3) = 2, &
                           ipType(3) = TypeInterpolationOpt%Equidistance
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP

TYPE(FEDomain_) :: dom
TYPE(FEDOF_) :: fedof, geofedof
TYPE(ScalarField_) :: u
TYPE(MeshField_) :: obj
CLASS(AbstractMesh_), POINTER :: mesh
CLASS(AbstractFE_), POINTER :: feptr
INTEGER(I4B) :: npts, nrow, ncol, mrow, mcol, nsd
REAL(DFP), ALLOCATABLE :: xij(:, :), elemCoord(:, :)
LOGICAL(LGT) :: isok

CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)

CALL dom%ImportFromToml(fileName=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      fileName=tomlFileName, tomlName="u")
mesh => dom%GetMeshPointer()
nsd = mesh%GetNSD()

CALL InitiateInterpolationPoints( &
  obj=obj, order=order, ipType=ipType, fedof=fedof, mesh=mesh, &
  engine=engineopt%native_serial)

CALL OK(obj%GetRank() .EQ. fevaropt%vector, "GetRank")
CALL OK(obj%GetVarType() .EQ. fevaropt%space, "GetVarType")
CALL OK(obj%GetDefineOn() .EQ. fevaropt%nodal, "GetDefineOn")
CALL OK(obj%GetTotalElements() .EQ. mesh%GetTotalElements(), &
        "GetTotalElements")

feptr => fedof%GetFEPointer(globalElement=1, islocal=math%yes)
npts = feptr%GetTotalInterpolationPoints(order=order, ipType=ipType)

ALLOCATE (xij(3, obj%GetMaxNNE()), elemCoord(3, mesh%GetMaxNNE()))
elemCoord = 0.0_DFP

CALL obj%GetSpaceVectorField_(globalElement=1, ans=xij, nrow=mrow, &
                              ncol=mcol, islocal=math%yes)
CALL OK((mrow .EQ. 3) .AND. (mcol .EQ. npts), "shape of element data")

! The first points are the vertices of the element
CALL mesh%GetNodeCoord(nodeCoord=elemCoord, nrow=nrow, ncol=ncol, &
                       globalElement=1, islocal=math%yes)
isok = ALL(ABS(xij(1:nsd, 1:ncol) - elemCoord(1:nsd, 1:ncol)) .LT. tol)
CALL OK(isok, "vertices are interpolation points")

NULLIFY (mesh, feptr)
CALL obj%DEALLOCATE()
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
