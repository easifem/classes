!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Testing VectorField GetVtkUnstructuredValue on triangles
!
! The field u of order 3 (H1 Lagrange, equidistance points, two space
! components) on small_tri3_mesh.h5 (Triangle3 elements with different
! orientations) is set to the coordinates of its DOF by
! SetHigherOrderNodeCoord. The local shape functions are computed once
! and reused for all elements; the orientation of each element is fixed
! in its connectivity (FixOrientOfNptrs). GetVtkUnstructuredValue gives
! the vector of u at the 10 points of each cell, which must be equal to
! the points given by GetVtkUnstructuredGridData.

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
USE Test_Method, ONLY: OK
USE VectorField_Class, ONLY: VectorField_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = &
                           "GetVtkUnstructuredValue_test_2.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-8_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(VectorField_) :: u
INTEGER(I4B), ALLOCATABLE :: connectivity(:), offsets(:)
INTEGER(INT8), ALLOCATABLE :: types(:)
REAL(DFP), ALLOCATABLE :: points(:, :), VALUE(:, :)

CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

! domain and u = x
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL u%SetHigherOrderNodeCoord()

! grid and values
CALL u%GetVtkUnstructuredGridData(points=points, &
                                  connectivity=connectivity, &
                                  offsets=offsets, types=types)
CALL u%GetVtkUnstructuredValue(VALUE=VALUE)

CALL OK(SIZE(VALUE, 1) .EQ. 3, "three rows")
CALL OK(SIZE(VALUE, 2) .EQ. 10 * (SIZE(offsets) - 1), &
        "ten points in each cell")
CALL OK(SIZE(VALUE, 2) .EQ. SIZE(points, 2), "one vector for each point")
CALL OK(ALL(VALUE(3, :) .EQ. math%zero), "third row is zero")
CALL OK(MAXVAL(ABS(VALUE(1:2, :) - points(1:2, :))) .LT. tol, &
        "value = coordinates of the points")

CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()

END PROGRAM main
