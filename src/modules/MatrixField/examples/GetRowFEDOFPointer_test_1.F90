!> author: Vikas Sharma, Ph. D.
! date: 2026-11-11
! summary: Row and column FEDOF pointers of a matrix field
!
! The vector field v (two space components on square_3x2.h5) is imported
! from MatrixField_test.toml. The matrix field obj initiated from v is a
! square matrix, so its row and column FEDOF pointers both point to the
! fedof of v, and its geometric FEDOF pointers to the geofedof of v.
! A copy obj2 of obj has the same pointers. After DEALLOCATE the
! pointers are nullified, but the FEDOF objects are kept.
!
! AbstractMatrixFieldSetFEDOF sets the column FEDOF to the row FEDOF
! when colFedof is absent, and to colFedof when it is present.

PROGRAM main
USE AbstractMatrixField_Class, ONLY: AbstractMatrixFieldSetFEDOF
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDOF_Class, ONLY: FEDOFPointer_
USE FEDomain_Class, ONLY: FEDomain_
USE MatrixField_Class, ONLY: MatrixField_
USE Test_Method, ONLY: OK
USE VectorField_Class, ONLY: VectorField_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "MatrixField_test.toml"
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(VectorField_) :: v
TYPE(MatrixField_) :: obj, obj2
CLASS(FEDOF_), POINTER :: fedofPtr
TYPE(FEDOFPointer_), ALLOCATABLE :: fedofPtrs(:), vPtrs(:)

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL v%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="v")

! Test 1: initiate from a vector field (square matrix)
CALL obj%Initiate(obj2=v)
fedofPtr => obj%GetRowFEDOFPointer()
CALL OK(ASSOCIATED(fedofPtr, fedof), "from v: GetRowFEDOFPointer()")
fedofPtr => obj%GetColFEDOFPointer()
CALL OK(ASSOCIATED(fedofPtr, fedof), "from v: GetColFEDOFPointer()")
fedofPtr => obj%GetRowFEDOFPointer(indx=1)
CALL OK(ASSOCIATED(fedofPtr, fedof), "from v: GetRowFEDOFPointer(1)")
CALL OK(ASSOCIATED(obj%rowGeoFedof, geofedof), "from v: rowGeoFedof")
CALL OK(ASSOCIATED(obj%colGeoFedof, geofedof), "from v: colGeoFedof")
fedofPtrs = obj%GetRowFEDOFPointer(indx=[1])
vPtrs = v%GetFEDOFPointer(indx=[1])
CALL OK(SIZE(fedofPtrs) .EQ. SIZE(vPtrs), "from v: SIZE(rowFedofs)")

! Test 2: a copy has the same pointers
CALL obj2%Initiate(obj2=obj)
CALL OK(ASSOCIATED(obj2%rowFedof, fedof), "copy: rowFedof")
CALL OK(ASSOCIATED(obj2%colFedof, fedof), "copy: colFedof")

! Test 3: DEALLOCATE nullifies the pointers, the FEDOF is kept
CALL obj2%DEALLOCATE()
CALL OK(.NOT. ASSOCIATED(obj2%rowFedof), "DEALLOCATE: rowFedof")
CALL OK(.NOT. ASSOCIATED(obj2%colFedof), "DEALLOCATE: colFedof")
CALL OK(.NOT. ALLOCATED(obj2%rowFedofs), "DEALLOCATE: rowFedofs")
CALL OK(ASSOCIATED(obj%rowFedof, fedof), "DEALLOCATE: obj not changed")

! Test 4: colFedof can differ from rowFedof
CALL AbstractMatrixFieldSetFEDOF(obj=obj, rowFedof=fedof, &
                                 colFedof=geofedof)
CALL OK(ASSOCIATED(obj%rowFedof, fedof), "SetFEDOF: rowFedof")
CALL OK(ASSOCIATED(obj%colFedof, geofedof), "SetFEDOF: colFedof")
CALL OK(.NOT. ASSOCIATED(obj%rowGeoFedof), "SetFEDOF: rowGeoFedof reset")

CALL obj%DEALLOCATE()
CALL v%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
