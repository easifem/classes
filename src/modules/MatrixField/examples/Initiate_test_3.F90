!> author: Vikas Sharma, Ph. D.
! date: 2026-11-11
! summary: Initiate a matrix field twice, from different node fields
!
! The scalar field u (6 DOFs) and the vector field v (two space
! components, 12 DOFs) are imported from MatrixField_test.toml. Each has
! its own fedof and geofedof.
!
! The matrix field obj is initiated from u. It is a 6 x 6 matrix named
! "u", and its row and column FEDOF pointers point to the fedof of u.
!
! The same obj is then initiated from v. Initiate deallocates obj first,
! so obj becomes a 12 x 12 matrix named "v", and its pointers now point
! to the fedof and geofedof of v.
!
! Changes:
!
! - 2026-11-11: rewritten. The old test initiated the matrix from
!   arguments (fedof, geofedof, spaceCompo, ...). That method now
!   belongs to AbstractNodeField_ only.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE MatrixField_Class, ONLY: MatrixField_
USE ScalarField_Class, ONLY: ScalarField_
USE Test_Method, ONLY: OK
USE VectorField_Class, ONLY: VectorField_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "Initiate_test_3.toml"
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: ufedof, ugeofedof, vfedof, vgeofedof
TYPE(ScalarField_) :: u
TYPE(VectorField_) :: v
TYPE(MatrixField_) :: obj

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=ufedof, geofedof=ugeofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL v%ImportFromToml(fedof=vfedof, geofedof=vgeofedof, dom=dom, &
                      filename=tomlFileName, tomlName="v")

! Test 1: initiate from the scalar field u
CALL obj%Initiate(obj2=u)
CALL OK(obj%GetName() .EQ. "u", "from u: name")
CALL OK(ALL(obj%SHAPE() .EQ. [6, 6]), "from u: Shape = [6, 6]")
CALL OK(obj%mat%csr%nnz .EQ. 28, "from u: nnz = 28")
CALL OK(ASSOCIATED(obj%rowFedof, ufedof), "from u: rowFedof")
CALL OK(ASSOCIATED(obj%colFedof, ufedof), "from u: colFedof")

! Test 2: initiate the same obj from the vector field v
CALL obj%Initiate(obj2=v)
CALL OK(obj%IsInitiated(), "from v: IsInitiated")
CALL OK(obj%GetName() .EQ. "v", "from v: name")
CALL OK(ALL(obj%SHAPE() .EQ. [12, 12]), "from v: Shape = [12, 12]")
CALL OK(obj%mat%csr%nnz .EQ. 112, "from v: nnz = 112")
CALL OK(ASSOCIATED(obj%rowFedof, vfedof), "from v: rowFedof")
CALL OK(ASSOCIATED(obj%colFedof, vfedof), "from v: colFedof")
CALL OK(ASSOCIATED(obj%rowGeoFedof, vgeofedof), "from v: rowGeoFedof")
CALL OK(ASSOCIATED(obj%colGeoFedof, vgeofedof), "from v: colGeoFedof")

CALL obj%DEALLOCATE()
CALL u%DEALLOCATE()
CALL v%DEALLOCATE()
CALL ufedof%DEALLOCATE()
CALL ugeofedof%DEALLOCATE()
CALL vfedof%DEALLOCATE()
CALL vgeofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
