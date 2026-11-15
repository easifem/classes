!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Testing ScalarField GetVtkUnstructuredValue on triangles
!
! The scalar field u of order 3 (H1 Lagrange, equidistance points) on
! small_tri3_mesh.h5 (Triangle3 elements with different orientations) is
! set to the x coordinate of its DOF: the vector field v with the same
! space is set by SetHigherOrderNodeCoord, and its first component is
! copied to u. The local shape functions are computed once and reused
! for all elements; the orientation of each element is fixed in its
! connectivity (FixOrientOfNptrs). GetVtkUnstructuredValue gives the
! value of u at the 10 points of each cell, which must be equal to the x
! coordinate of the points given by GetVtkUnstructuredGridData.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: INT8
USE ScalarField_Class, ONLY: ScalarField_
USE Test_Method, ONLY: OK
USE VectorField_Class, ONLY: VectorField_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = &
                           "GetVtkUnstructuredGridValue_test_2.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-8_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof, vfedof, vgeofedof
TYPE(ScalarField_) :: u
TYPE(VectorField_) :: v
INTEGER(I4B), ALLOCATABLE :: connectivity(:), offsets(:)
INTEGER(INT8), ALLOCATABLE :: types(:)
REAL(DFP), ALLOCATABLE :: points(:, :), VALUE(:), xdof(:)
INTEGER(I4B) :: tsize

CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL v%ImportFromToml(fedof=vfedof, geofedof=vgeofedof, dom=dom, &
                      filename=tomlFileName, tomlName="v")

! u = x at the DOF
CALL v%SetHigherOrderNodeCoord()
ALLOCATE (xdof(fedof%GetTotalDOF()))
CALL v%Get(VALUE=xdof, tsize=tsize, spaceCompo=1)
CALL OK(tsize .EQ. SIZE(xdof), "same number of DOF in u and v")
CALL u%Set(VALUE=xdof(1:tsize))

! grid and values
CALL u%GetVtkUnstructuredGridData(points=points, &
                                  connectivity=connectivity, &
                                  offsets=offsets, types=types)
CALL u%GetVtkUnstructuredValue(VALUE=VALUE)

CALL OK(SIZE(VALUE) .EQ. 10 * (SIZE(offsets) - 1), &
        "ten points in each cell")
CALL OK(SIZE(VALUE) .EQ. SIZE(points, 2), "one value for each point")
CALL OK(MAXVAL(ABS(VALUE - points(1, :))) .LT. tol, &
        "value = x coordinate of the points")

CALL u%DEALLOCATE()
CALL v%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL vfedof%DEALLOCATE()
CALL vgeofedof%DEALLOCATE()
CALL dom%DEALLOCATE()

END PROGRAM main
