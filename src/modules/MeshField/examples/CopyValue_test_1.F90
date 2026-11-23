!> author: Shion Shimizu
! date: 2026-11-23
! summary: Copy the values of a mesh field with CopyValue
!
! CopyValue copies the values of obj2 into obj. Both fields must have the
! same blocks. Same blocks means the same number of elements and the same
! data shape in each element.

PROGRAM main
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeFEVariableConstant
USE BaseType, ONLY: TypeFEVariableScalar
USE BaseType, ONLY: TypeFEVariableSpace
USE BaseType, ONLY: TypeFEVariableVector
USE EngineOpt_Class, ONLY: engineopt => TypeEngineOpt
USE ExceptionHandler_Class, ONLY: e
USE FEMesh_Class, ONLY: FEMesh_
USE FEVariable_Method, ONLY: NodalVariable
USE FEVariable_Method, ONLY: QuadratureVariable
USE FieldOpt_Class, ONLY: fieldopt => TypeFieldOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE HDF5File_Class, ONLY: HDF5File_
USE MeshField_Class, ONLY: MeshField_
USE Test_Method, ONLY: OK

IMPLICIT NONE

CHARACTER(*), PARAMETER :: &
  filename = "./meshdata/small_tri3_mesh.h5"
INTEGER(I4B), PARAMETER :: nsd = 2
INTEGER(I4B), PARAMETER :: nq = 3
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

TYPE(HDF5File_) :: meshfile
TYPE(FEMesh_), TARGET :: amesh
INTEGER(I4B) :: telements, nns

CALL e%SetQuietMode(math%yes)

CALL meshfile%Initiate(fileName=filename, MODE="READ")
CALL meshfile%OPEN()
CALL amesh%Initiate(hdf5=meshfile, dim=nsd)

nns = amesh%GetMaxNNE()
telements = amesh%GetTotalElements()

! Test 1: the values of obj2 are copied, and obj keeps its mesh
BLOCK
  TYPE(MeshField_) :: obj, obj2
  CLASS(AbstractMesh_), POINTER :: meshptr

  CALL NewScalarField(obj, "committed", 0.0_DFP)
  CALL NewScalarField(obj2, "current", 1.0_DFP)
  CALL obj%CopyValue(obj2=obj2)
  CALL OK(IsScalarField(obj, 1.0_DFP), "scalar field: values copied")

  meshptr => obj%GetMeshPointer()
  CALL OK(ASSOCIATED(meshptr, amesh), "scalar field: mesh of obj kept")

  NULLIFY (meshptr)
  CALL obj%DEALLOCATE()
  CALL obj2%DEALLOCATE()
END BLOCK

! Test 2: obj holds a copy, so a later change of obj2 does not change it
BLOCK
  TYPE(MeshField_) :: obj, obj2
  TYPE(FEVariable_) :: fevar
  REAL(DFP), ALLOCATABLE :: val(:)

  CALL NewScalarField(obj, "committed", 0.0_DFP)
  CALL NewScalarField(obj2, "current", 1.0_DFP)
  CALL obj%CopyValue(obj2=obj2)

  ALLOCATE (val(nns))
  val = -1.0_DFP
  fevar = NodalVariable(val, TypeFEVariableScalar, TypeFEVariableSpace)
  CALL obj2%Set(globalElement=1, islocal=math%yes, fevar=fevar)
  CALL OK(IsScalarField(obj, 1.0_DFP), &
          "scalar field: obj does not change with obj2")

  DEALLOCATE (val)
  CALL obj%DEALLOCATE()
  CALL obj2%DEALLOCATE()
END BLOCK

! Test 3: blocks of different sizes, like the internal variables of two
! materials (1 or 3 components at each quadrature point)
BLOCK
  TYPE(MeshField_) :: obj, obj2

  CALL NewVectorField(obj, "committed", 0.0_DFP)
  CALL NewVectorField(obj2, "current", 1.0_DFP)
  CALL obj%CopyValue(obj2=obj2)
  CALL OK(IsVectorField(obj, 1.0_DFP), &
          "blocks of different sizes: shapes and values copied")

  CALL obj%DEALLOCATE()
  CALL obj2%DEALLOCATE()
END BLOCK

! Test 4: a field with one value for all elements (fieldType constant)
BLOCK
  TYPE(MeshField_) :: obj, obj2
  TYPE(FEVariable_) :: found

  CALL NewConstantField(obj, "committed", 0.0_DFP)
  CALL NewConstantField(obj2, "current", 3.5_DFP)
  CALL obj%CopyValue(obj2=obj2)

  CALL obj%Get(globalElement=telements, islocal=math%yes, fevar=found)
  CALL OK(found%len .EQ. 1, "constant field: one value")
  CALL OK(ABS(found%val(1) - 3.5_DFP) .LT. tol, &
          "constant field: value copied")

  CALL obj%DEALLOCATE()
  CALL obj2%DEALLOCATE()
END BLOCK

! Test 5: commit and revert of a quantity on two time levels
BLOCK
  TYPE(MeshField_) :: committed, current
  TYPE(FEVariable_) :: fevar
  REAL(DFP), ALLOCATABLE :: val(:)

  CALL NewScalarField(committed, "committed", 0.0_DFP)
  CALL NewScalarField(current, "current", 1.0_DFP)

  ! commit
  CALL committed%CopyValue(obj2=current)

  ! a new iterate changes the current values of all elements
  ALLOCATE (val(nns))
  val = 7.0_DFP
  fevar = NodalVariable(val, TypeFEVariableScalar, TypeFEVariableSpace)
  CALL current%Set(fevar=fevar)
  CALL OK(.NOT. IsScalarField(current, 1.0_DFP), &
          "two levels: the iterate changed current")

  ! revert
  CALL current%CopyValue(obj2=committed)
  CALL OK(IsScalarField(current, 1.0_DFP), &
          "two levels: revert restores the committed values")

  DEALLOCATE (val)
  CALL committed%DEALLOCATE()
  CALL current%DEALLOCATE()
END BLOCK

CALL amesh%DEALLOCATE()
CALL meshfile%CLOSE()
CALL meshfile%DEALLOCATE()

CONTAINS

! Scalar nodal field that varies in space, with ScalarValues in every
! element
SUBROUTINE NewScalarField(obj, name, scale)
  TYPE(MeshField_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: name
  REAL(DFP), INTENT(IN) :: scale

  TYPE(FEVariable_) :: fevar
  INTEGER(I4B) :: iel

  CALL obj%Initiate( &
    name=name, fieldType=fieldopt%normal, &
    engine=engineopt%native_serial, defineOn=fevaropt%nodal, &
    varType=fevaropt%space, rank=fevaropt%scalar, s=[nns], mesh=amesh)

  DO iel = 1, telements
    fevar = NodalVariable(ScalarValues(iel, scale), TypeFEVariableScalar, &
                          TypeFEVariableSpace)
    CALL obj%Insert(globalElement=iel, islocal=math%yes, fevar=fevar)
  END DO
END SUBROUTINE NewScalarField

! Values of element iel of NewScalarField: node k holds
! scale * (10 * iel + k)
FUNCTION ScalarValues(iel, scale) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: iel
  REAL(DFP), INTENT(IN) :: scale
  REAL(DFP), ALLOCATABLE :: ans(:)

  INTEGER(I4B) :: kk

  ans = [(scale * REAL(10 * iel + kk, DFP), kk=1, nns)]
END FUNCTION ScalarValues

! True if every element of obj holds the values of NewScalarField
FUNCTION IsScalarField(obj, scale) RESULT(ans)
  TYPE(MeshField_), INTENT(IN) :: obj
  REAL(DFP), INTENT(IN) :: scale
  LOGICAL(LGT) :: ans

  TYPE(FEVariable_) :: found
  INTEGER(I4B) :: iel

  ans = .TRUE.
  DO iel = 1, telements
    CALL obj%Get(globalElement=iel, islocal=math%yes, fevar=found)
    ans = ans .AND. SameValues(found, ScalarValues(iel, scale))
  END DO
END FUNCTION IsScalarField

! Vector field at nq quadrature points, with VectorValues in every
! element
SUBROUTINE NewVectorField(obj, name, scale)
  TYPE(MeshField_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: name
  REAL(DFP), INTENT(IN) :: scale

  TYPE(FEVariable_) :: fevar
  INTEGER(I4B) :: iel

  CALL obj%Initiate( &
    name=name, fieldType=fieldopt%normal, &
    engine=engineopt%native_serial, defineOn=fevaropt%quadrature, &
    varType=fevaropt%space, rank=fevaropt%vector, s=[3, nq], mesh=amesh)

  DO iel = 1, telements
    fevar = QuadratureVariable(VectorValues(iel, scale), &
                               TypeFEVariableVector, TypeFEVariableSpace)
    CALL obj%Insert(globalElement=iel, islocal=math%yes, fevar=fevar)
  END DO
END SUBROUTINE NewVectorField

! Values of element iel of NewVectorField, (components, points): one
! component in odd elements, three in even elements; component i at
! point j holds scale * (100 * iel + 10 * i + j)
FUNCTION VectorValues(iel, scale) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: iel
  REAL(DFP), INTENT(IN) :: scale
  REAL(DFP), ALLOCATABLE :: ans(:, :)

  INTEGER(I4B) :: ii, jj, nc

  nc = 3 - 2 * MOD(iel, 2)
  ans = RESHAPE([((scale * REAL(100 * iel + 10 * ii + jj, DFP), &
                   ii=1, nc), jj=1, nq)], [nc, nq])
END FUNCTION VectorValues

! True if every element of obj has the shape and the values of
! NewVectorField
FUNCTION IsVectorField(obj, scale) RESULT(ans)
  TYPE(MeshField_), INTENT(IN) :: obj
  REAL(DFP), INTENT(IN) :: scale
  LOGICAL(LGT) :: ans

  TYPE(FEVariable_) :: found
  REAL(DFP), ALLOCATABLE :: want(:, :)
  INTEGER(I4B) :: iel

  ans = .TRUE.
  DO iel = 1, telements
    CALL obj%Get(globalElement=iel, islocal=math%yes, fevar=found)
    want = VectorValues(iel, scale)
    ans = ans .AND. ALL(found%s(1:2) .EQ. SHAPE(want))
    ans = ans .AND. SameValues(found, RESHAPE(want, [SIZE(want)]))
  END DO
END FUNCTION IsVectorField

! True if found holds the values want
FUNCTION SameValues(found, want) RESULT(ans)
  TYPE(FEVariable_), INTENT(IN) :: found
  REAL(DFP), INTENT(IN) :: want(:)
  LOGICAL(LGT) :: ans

  ans = found%len .EQ. SIZE(want)
  IF (ans) ans = ALL(ABS(found%val(1:found%len) - want) .LT. tol)
END FUNCTION SameValues

! Scalar field with one value for all elements (fieldType constant)
SUBROUTINE NewConstantField(obj, name, VALUE)
  TYPE(MeshField_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: name
  REAL(DFP), INTENT(IN) :: VALUE

  TYPE(FEVariable_) :: fevar

  CALL obj%Initiate( &
    name=name, fieldType=fieldopt%constant, &
    engine=engineopt%native_serial, defineOn=fevaropt%nodal, &
    varType=fevaropt%constant, rank=fevaropt%scalar, s=[1], mesh=amesh)

  fevar = NodalVariable(VALUE, TypeFEVariableScalar, TypeFEVariableConstant)
  CALL obj%Insert(globalElement=1, islocal=math%yes, fevar=fevar)
END SUBROUTINE NewConstantField

END PROGRAM main
