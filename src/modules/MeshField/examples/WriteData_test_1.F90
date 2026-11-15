!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Write a mesh field to a VTK file
!
! WriteData writes the mesh field to a VTK PolyData (.vtp) file.
! nodeCoordField is a vector mesh field which contains the coordinates
! of the points where the data is defined. Here we write the
! interpolation points, so the field is its own nodeCoordField.

PROGRAM main
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE EngineOpt_Class, ONLY: engineopt => TypeEngineOpt
USE ExceptionHandler_Class, ONLY: e
USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE MeshField_Class, ONLY: InitiateInterpolationPoints
USE MeshField_Class, ONLY: MeshField_
USE ScalarField_Class, ONLY: ScalarField_
USE Test_Method, ONLY: OK

IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "MeshField_test.toml", &
                           vtkFileName = "WriteData_test_1.vtp"
INTEGER(I4B), PARAMETER :: order(3) = 2, &
                           ipType(3) = TypeInterpolationOpt%Equidistance

TYPE(FEDomain_) :: dom
TYPE(FEDOF_) :: fedof, geofedof
TYPE(ScalarField_) :: u
TYPE(MeshField_) :: obj
CLASS(AbstractMesh_), POINTER :: mesh
LOGICAL(LGT) :: isok

CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)

CALL dom%ImportFromToml(fileName=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      fileName=tomlFileName, tomlName="u")
mesh => dom%GetMeshPointer()

CALL InitiateInterpolationPoints( &
  obj=obj, order=order, ipType=ipType, fedof=fedof, mesh=mesh, &
  engine=engineopt%native_serial)

CALL obj%WriteData(filename=vtkFileName, nodeCoordField=obj)

INQUIRE (FILE=vtkFileName, EXIST=isok)
CALL OK(isok, "WriteData")

NULLIFY (mesh)
CALL obj%DEALLOCATE()
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
