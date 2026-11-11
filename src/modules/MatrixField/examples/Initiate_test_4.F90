!> author: Vikas Sharma, Ph. D.
! date: 2026-11-11
! summary: Initiate a rectangle matrix field from a row and a column field
!
! obj%Initiate(rowObj, colObj) makes the matrix of y = A * x, where
! y = rowObj and x = colObj are node fields. The rows are the DOFs of
! rowObj, the columns are the DOFs of colObj, and the sparsity pattern
! is set by the FEDOF of rowObj and colObj on the same mesh.
!
! The mesh square_3x2.h5 has two Quadrangle4 elements and six nodes.
! The scalar fields u1 and u2 are imported from Initiate_test_4.toml:
!
! - u1 (rows): broken space of order 1; each element owns its 4 DOFs,
!   so u1 has 8 DOFs;
! - u2 (columns): continuous space of order 1, 6 DOFs.
!
! So obj is an 8 x 6 matrix. Only u1 is broken, so there are no
! neighbour-cell couplings: each element gives a 4 x 4 block, and
! nnz = 2 * 4 * 4 = 32. obj takes the name of colObj ("u"), and its row
! and column FEDOF pointers point to the fedof of u1 and u2.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE MatrixField_Class, ONLY: MatrixField_
USE ScalarField_Class, ONLY: ScalarField_
USE Test_Method, ONLY: OK
USE Test_Planning, ONLY: done_testing
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "Initiate_test_4.toml"
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: u1fedof, u2fedof, u1geofedof, u2geofedof
TYPE(ScalarField_) :: u1, u2
TYPE(MatrixField_) :: obj

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u1%ImportFromToml(fedof=u1fedof, geofedof=u1geofedof, dom=dom, &
                       filename=tomlFileName, tomlName="u1")

CALL u2%ImportFromToml(fedof=u2fedof, geofedof=u2geofedof, dom=dom, &
                       filename=tomlFileName, tomlName="u2")

! Initiate from the row field u1 and the column field u2
CALL obj%Initiate(rowObj=u1, colObj=u2)
CALL OK(obj%GetName() .EQ. "u", "Initiate2: name")
CALL OK(ALL(obj%SHAPE() .EQ. [8, 6]), "Initiate2: shape = [8, 6]")
CALL OK(obj%mat%csr%nnz .EQ. 32, "Initiate2: nnz = 32")
CALL OK(ASSOCIATED(obj%rowFedof, u1fedof), "Initiate2: rowFedof is u1 fedof")
CALL OK(ASSOCIATED(obj%colFedof, u2fedof), "Initiate2: colFedof is u2 fedof")

CALL done_testing()
CALL obj%DEALLOCATE()
CALL u1%DEALLOCATE()
CALL u2%DEALLOCATE()
CALL u1geofedof%DEALLOCATE()
CALL u2geofedof%DEALLOCATE()
CALL u1fedof%DEALLOCATE()
CALL u2fedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
