! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-15
! summary: Native serial block finite element matrix field
!
! This module defines [[BlockMatrixField_]], a [[MatrixField_]] whose
! rows and columns can belong to several physical variables, each with
! its own FEDOF. It uses the `NATIVE_SERIAL` engine and stores the
! matrix in DOF_FMT.

MODULE BlockMatrixField_Class
USE AbstractField_Class, ONLY: AbstractField_
USE AbstractMatrixField_Class, ONLY: AbstractMatrixField_
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDOF_Class, ONLY: FEDOFPointer_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: DOF_FMT
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE HDF5File_Class, ONLY: HDF5File_
USE MatrixField_Class, ONLY: MatrixField_
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
USE TimeFEDOF_Class, ONLY: TimeFEDOFPointer_
IMPLICIT NONE
PRIVATE

PUBLIC :: BlockMatrixField_

INTEGER(I4B), PARAMETER :: mystorageformat = DOF_FMT

!----------------------------------------------------------------------------
!                                                          BlockMatrixField_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-15
! summary: Native serial block finite element matrix
!
! [[BlockMatrixField_]] extends [[MatrixField_]] and has no components
! of its own. Next to the fedof of [[AbstractField_]], it keeps one
! FEDOF pointer per physical variable in `fedofs`.
!
! At present a [[BlockMatrixField_]] is created by the Initiate method
! of [[MatrixField_]], from a node field or another matrix field. This
! gives one physical variable. The constructors for several physical
! variables (from a ParameterList_) are removed. All other methods
! except Import come from [[MatrixField_]].
!
! Full example: src/modules/BlockMatrixField/examples/Initiate_test_1.F90
!
! See also: [[MatrixField_]], [[AbstractMatrixField_]]

TYPE, EXTENDS(MatrixField_) :: BlockMatrixField_
CONTAINS
  FINAL :: obj_Final
  !! Finalizer, deallocate the field

  !@IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import from an HDF5 file
END TYPE BlockMatrixField_

!----------------------------------------------------------------------------
!                                SetBlockMatrixFieldParam@ConstructorMethods
!----------------------------------------------------------------------------

! Removed API, kept for reference: SetBlockMatrixFieldParam set the
! parameters of a ParameterList_ for creating a BlockMatrixField_ with
! several physical variables. The sizes of physicalVarNames, spaceCompo
! and timeCompo were the same.

! INTERFACE
!   MODULE SUBROUTINE SetBlockMatrixFieldParam( &
!     param, name, matrixProp, physicalVarNames, spaceCompo, timeCompo, &
!     engine, fieldType, comm, local_n, global_n)
!     TYPE(ParameterList_), INTENT(INOUT) :: param
!     !! Options to create [[BlockMatrixField_]] will be stored in this
!     CHARACTER(*), INTENT(IN) :: name
!     !! Name of the matrix field
!     CHARACTER(*), INTENT(IN) :: matrixProp
!     !! Matrix property, "SYM" or "UNSYM"
!     CHARACTER(*), INTENT(IN) :: physicalVarNames(:)
!     !! Name of physical variables
!     INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
!     !! Number of space-components in each physicalVarNames, see [[DOF_]]
!     INTEGER(I4B), INTENT(IN) :: timeCompo(:)
!     !! Number of time-components in each physicalVarNames, see [[DOF_]]
!     CHARACTER(*), INTENT(IN) :: engine
!     !! engine
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: fieldType
!     !! fieldType can be following
!     !! FIELD_TYPE_NORMAL <-- DEFAULT
!     !! FIELD_TYPE_CONSTANT
!     !! FIELD_TYPE_CONSTANT_SPACE
!     !! FIELD_TYPE_CONSTANT_TIME
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: comm
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_n
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: global_n
!   END SUBROUTINE SetBlockMatrixFieldParam
! END INTERFACE

!----------------------------------------------------------------------------
!                       SetBlockMatrixFieldPrecondParam@sConstructorMethods
!----------------------------------------------------------------------------

! Removed API, kept for reference: SetBlockMatrixFieldPrecondParam set
! the parameters of a ParameterList_ for the preconditioner.

! INTERFACE
!   MODULE SUBROUTINE SetBlockMatrixFieldPrecondParam( &
!     param, name, engine, lfil, mbloc, droptol, permtol, alpha, comm, &
!     local_n, global_n)
!     TYPE(ParameterList_), INTENT(INOUT) :: param
!     !! Options to create precondition of [[BlockMatrixField_]]
!     INTEGER(I4B), INTENT(IN) :: name
!     !! Name of precondition
!     CHARACTER(*), INTENT(IN) :: engine
!     !! engine
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: lfil
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: mbloc
!     REAL(DFP), OPTIONAL, INTENT(IN) :: droptol
!     !! Droptoleranace
!     REAL(DFP), OPTIONAL, INTENT(IN) :: permtol
!     !! permutation tolerance
!     REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: comm
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_n
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: global_n
!   END SUBROUTINE SetBlockMatrixFieldPrecondParam
! END INTERFACE

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-16
! summary: Finalizer of a block matrix field
!
! This finalizer calls the DEALLOCATE method of [[MatrixField_]].
!
! See also: [[MatrixField_:obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(BlockMatrixField_), INTENT(INOUT) :: obj
    !! block matrix field
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-16
! summary: Import a block matrix field from an HDF5 file
!
! This routine reads a matrix field written by the Export method of
! [[MatrixField_]] from the group `group` of an HDF5 file open in read
! mode. obj must not be initiated, and rowFedof or rowFedofs must be
! present.
!
!- AbstractFieldImport reads the data of [[AbstractField_]] (name,
!  engine, fieldType, comm and sizes).
!- matrixProp and isRectangle are read from `group`.
!- The number of physical variables is read from
!  `group/tPhysicalVarNames`. If only rowFedof is given, `rowFedofs`
!  gets one pointer to rowFedof per physical variable; colFedof is
!  handled in the same way.
!- The FEDOF pointers are set by [[AbstractMatrixFieldSetFEDOF]]; an
!  absent column argument takes the row value (square matrix).
!- The CSR matrix is read from `group/mat`.
!- The preconditioner is read from `group/pmat` if it is present.
!
! Changes:
!   - 2026-11-10: Keep the name, engine and fieldType read by
!     AbstractFieldImport. Allocate fedofs only when fedofs is absent.
!     Remove the unused reads of spaceCompo and timeCompo.
!   - 2026-11-11: Set rowFedofs and colFedofs (square matrix), one
!     pointer per physical variable. The arguments are rowFedof(s),
!     rowGeoFedof(s), colFedof(s) and colGeoFedof(s) instead of
!     fedof(s), geofedof(s) and timefedof(s).
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE BlockMatrixField_Class, ONLY: BlockMatrixField_
! USE ExceptionHandler_Class, ONLY: e
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDOF_Class, ONLY: FEDOFPointer_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
! USE VectorField_Class, ONLY: VectorField_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "BlockMatrixField_test.toml"
! CHARACTER(*), PARAMETER :: filename = "./Import_test_1.h5"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(VectorField_) :: v
! TYPE(BlockMatrixField_) :: obj, obj2, obj3
! TYPE(FEDOFPointer_) :: fedofs(1)
! TYPE(HDF5File_) :: h5
! REAL(DFP) :: val1, val2
! INTEGER(I4B) :: ii
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL v%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="v")
! CALL obj%Initiate(obj2=v)
! DO ii = 1, SIZE(obj%mat%A)
!   obj%mat%A(ii) = REAL(ii, DFP)
! END DO
!
! CALL h5%Initiate(filename=filename, mode="NEW")
! CALL h5%OPEN()
! CALL obj%Export(hdf5=h5, group="/K")
! CALL h5%DEALLOCATE()
!
! ! Test 1: one fedof for all physical variables
! CALL h5%Initiate(filename=filename, mode="READ")
! CALL h5%OPEN()
! CALL obj2%Import(hdf5=h5, group="/K", rowFedof=fedof, &
!                   rowGeoFedof=geofedof)
! CALL h5%DEALLOCATE()
!
! CALL OK(obj2%IsInitiated(), "Import: IsInitiated")
! CALL OK(obj2%GetName() .EQ. "v", "Import: name")
! CALL OK(obj2%GetEngineName() .EQ. "NATIVE_SERIAL", "Import: engine")
! CALL OK(ALL(obj2%SHAPE() .EQ. [12, 12]), "Import: Shape")
! CALL OK(obj2%mat%csr%nnz .EQ. 112, "Import: nnz")
! CALL OK(ALL(obj2%mat%csr%JA .EQ. obj%mat%csr%JA), "Import: sparsity")
! CALL OK(ALL(ABS(obj2%mat%A - obj%mat%A) .LT. tol), "Import: values")
! CALL obj%Get(iNodeNum=5, jNodeNum=6, islocal=math%yes, idof=2, jdof=1, &
!              VALUE=val1)
! CALL obj2%Get(iNodeNum=5, jNodeNum=6, islocal=math%yes, idof=2, jdof=1, &
!               VALUE=val2)
! CALL OK(ABS(val1 - val2) .LT. tol, "Import: dof, Get(5, 6)")
! CALL OK(SIZE(obj2%rowFedofs) .EQ. 1, "Import: one fedof per variable")
! CALL OK(ASSOCIATED(obj2%rowFedofs(1)%ptr, fedof), "Import: rowFedofs(1)")
! CALL OK(ASSOCIATED(obj2%colFedofs(1)%ptr, fedof), "Import: colFedofs(1)")
!
! ! Test 2: one fedof pointer per physical variable
! fedofs(1)%ptr => fedof
! CALL h5%Initiate(filename=filename, mode="READ")
! CALL h5%OPEN()
! CALL obj3%Import(hdf5=h5, group="/K", rowFedofs=fedofs)
! CALL h5%DEALLOCATE()
!
! CALL OK(obj3%IsInitiated(), "Import fedofs: IsInitiated")
! CALL OK(ALL(ABS(obj3%mat%A - obj%mat%A) .LT. tol), "Import fedofs: values")
! CALL OK(SIZE(obj3%rowFedofs) .EQ. 1, "Import fedofs: SIZE(rowFedofs)")
! CALL OK(ASSOCIATED(obj3%rowFedofs(1)%ptr, fedof), "Import fedofs: row")
! CALL OK(ASSOCIATED(obj3%colFedofs(1)%ptr, fedof), "Import fedofs: col")
!
! NULLIFY (fedofs(1)%ptr)
! CALL obj3%DEALLOCATE()
! CALL obj2%DEALLOCATE()
! CALL obj%DEALLOCATE()
! CALL v%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[MatrixField_:obj_Export]], [[MatrixField_:obj_Import]]

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, rowFedof, rowFedofs, &
                               rowGeoFedof, rowGeoFedofs, colFedof, &
                               colFedofs, colGeoFedof, colGeoFedofs)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    !! matrix field, not initiated
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file open in read mode
    CHARACTER(*), INTENT(IN) :: group
    !! group of the matrix field in the HDF5 file
    CLASS(FEDOF_), OPTIONAL, TARGET, INTENT(IN) :: rowFedof
    !! FEDOF of the rows
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: rowFedofs(:)
    !! FEDOF of the rows, one for each physical variable
    CLASS(FEDOF_), OPTIONAL, TARGET, INTENT(IN) :: rowGeoFedof
    !! geometric FEDOF of the rows
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: rowGeoFedofs(:)
    !! geometric FEDOF of the rows, one for each physical variable
    CLASS(FEDOF_), OPTIONAL, TARGET, INTENT(IN) :: colFedof
    !! FEDOF of the columns, default is rowFedof
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: colFedofs(:)
    !! FEDOF of the columns, default is rowFedofs
    CLASS(FEDOF_), OPTIONAL, TARGET, INTENT(IN) :: colGeoFedof
    !! geometric FEDOF of the columns, default is rowGeoFedof
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: colGeoFedofs(:)
    !! geometric FEDOF of the columns, default is rowGeoFedofs
  END SUBROUTINE obj_Import
END INTERFACE

END MODULE BlockMatrixField_Class
