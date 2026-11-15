!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Export a scalar field to VTKHDF5File
! Every thing is specified via toml config.
! You can define lua function which will be projected to finite element
! space.
PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeFileOpt
USE BaseType, ONLY: TypeFEVariableOpt
USE BaseType, ONLY: InterfaceScalarSubroutine
USE ExceptionHandler_Class, ONLY: e
USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: INT8
USE GlobalData, ONLY: LGT
USE HDF5File_Class, ONLY: HDF5File_
USE ScalarField_Class, ONLY: ScalarField_
USE Test_Method, ONLY: OK
USE UserFunction_Class, ONLY: UserFunction_
USE VtkHDF5File_Class, ONLY: VtkHDF5File_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = &
                           "./ExportToVtkHDF5File_test_1.toml", &
                           vtkhdfFileName = &
                           "ExportToVtkHDF5File_test_1.vtkhdf", &
                           funcTomlName = "ScalarFunc", &
                           domainTomlName = "domain", &
                           scalarFieldTomlName = "u"
TYPE(FEDomain_) :: dom
TYPE(FEDOF_) :: fedof
TYPE(FEDOF_) :: geofedof
TYPE(ScalarField_) :: u
TYPE(VtkHDF5File_) :: obj
TYPE(UserFunction_) :: func
PROCEDURE(InterfaceScalarSubroutine), POINTER :: procptr
REAL(DFP), ALLOCATABLE :: points(:, :), VALUE(:)
INTEGER(I4B), ALLOCATABLE :: connectivity(:), offsets(:)
INTEGER(INT8), ALLOCATABLE :: types(:)

CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

! user function f(x, y)
CALL func%ImportFromToml(tomlName=funcTomlName, filename=tomlFileName)

! domain and scalar field u = f
CALL dom%ImportFromToml(filename=tomlFileName, tomlName=domainTomlName)
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName=scalarFieldTomlName)
CALL u%SetByFunction(func=func)

! Lagrange grid and values of u
CALL u%GetVtkUnstructuredGridData(points=points, &
                                  connectivity=connectivity, &
                                  offsets=offsets, types=types)
CALL u%GetVtkUnstructuredValue(VALUE=VALUE)
! write the grid and u
CALL obj%InitiateVTKHDF5(filename=vtkhdfFileName, mode=TypeFileOpt%new, &
                         vtkType="UnstructuredGrid")
CALL obj%WriteUnstructuredGrid(points, connectivity, offsets, types)
CALL obj%WritePointData(NAME=scalarFieldTomlName, VALUE=VALUE)

CALL obj%DEALLOCATE()

! clean up
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL func%DEALLOCATE()
END PROGRAM main
