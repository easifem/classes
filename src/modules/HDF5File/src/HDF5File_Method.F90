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
!

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2021
! summary: Some additional methods for HDF5File

MODULE HDF5File_Method
USE GlobalData, ONLY: DFP, I4B, LGT
USE BaseType, ONLY: DOF_, CSRSparsity_, CSRMatrix_, RealVector_, IntVector_
USE String_Class
USE DOF_Method
USE CSRSparsity_Method
USE CSRMatrix_Method
USE RealVector_Method
USE IntVector_Method

USE HDF5File_Class
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE
PRIVATE

PUBLIC :: ExportDOF
PUBLIC :: ImportDOF
PUBLIC :: ExportCSRSparsity
PUBLIC :: ImportCSRSparsity
PUBLIC :: ExportCSRMatrix
PUBLIC :: ImportCSRMatrix
PUBLIC :: ExportRealVector
PUBLIC :: ImportRealVector
PUBLIC :: ExportIntVector
PUBLIC :: ImportIntVector
PUBLIC :: HDF5ReadScalar
PUBLIC :: HDF5ReadVector
PUBLIC :: HDF5ReadMatrix
PUBLIC :: HDF5GetEntities

INTERFACE HDF5ReadVector
  MODULE PROCEDURE HDF5ReadIntVector, HDF5ReadRealVector
END INTERFACE HDF5ReadVector

INTERFACE HDF5ReadMatrix
  MODULE PROCEDURE HDF5ReadIntMatrix, HDF5ReadRealMatrix
END INTERFACE HDF5ReadMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                               obj_Import_GetEntities
!----------------------------------------------------------------------------

SUBROUTINE HDF5GetEntities(hdf5, group, dim, tEntities, myName,  &
  & modName)
  CLASS(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group
  INTEGER(I4B), INTENT(IN) :: dim
  INTEGER(I4B), INTENT(OUT) :: tEntities
  CHARACTER(*), INTENT(IN) :: myName
  CHARACTER(*), INTENT(IN) :: modName

  SELECT CASE (dim)

  CASE (0)
    ! numPointEntities
    CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group,  &
      & VALUE=tEntities, fieldname="numPointEntities",  &
      & myName=myName, modName=modName)

  CASE (1)
    ! numCurveEntities
    CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group,  &
      & VALUE=tEntities, fieldname="numCurveEntities",  &
      & myName=myName, modName=modName)

  CASE (2)
    ! numSurfaceEntities
    CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group,  &
      & VALUE=tEntities, fieldname="numSurfaceEntities",  &
      & myName=myName, modName=modName)

  CASE (3)
    ! numVolumeEntities
    CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group,  &
      & VALUE=tEntities, fieldname="numVolumeEntities",  &
      & myName=myName, modName=modName)

  CASE default
  END SELECT

END SUBROUTINE HDF5GetEntities

!----------------------------------------------------------------------------
!                                                          HDF5ReadIntMatrix
!----------------------------------------------------------------------------

SUBROUTINE HDF5ReadIntMatrix(hdf5, VALUE, group, fieldname, myname,  &
  & modname, check)
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
  CHARACTER(*), INTENT(IN) :: group
  CHARACTER(*), INTENT(IN) :: fieldname
  CHARACTER(*), INTENT(IN) :: myname
  CHARACTER(*), INTENT(IN) :: modname
  LOGICAL(LGT), INTENT(IN) :: check

  LOGICAL(LGT) :: isok0
  CHARACTER(:), ALLOCATABLE :: astr

  astr = group//"/"//fieldname
  isok0 = hdf5%pathExists(astr)
  IF (isok0) THEN
    CALL hdf5%READ(astr, VALUE)
  END IF

  IF (check .AND. .NOT. isok0) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
      & '[INTERNAL ERROR]:: '//astr//' path does not exists.')
    RETURN
  END IF

  astr = ""
END SUBROUTINE HDF5ReadIntMatrix

!----------------------------------------------------------------------------
!                                                         HDF5ReadRealMatrix
!----------------------------------------------------------------------------

SUBROUTINE HDF5ReadRealMatrix(hdf5, VALUE, group, fieldname, myname,  &
  & modname, check)
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
  CHARACTER(*), INTENT(IN) :: group
  CHARACTER(*), INTENT(IN) :: fieldname
  CHARACTER(*), INTENT(IN) :: myname
  CHARACTER(*), INTENT(IN) :: modname
  LOGICAL(LGT), INTENT(IN) :: check

  LOGICAL(LGT) :: isok0
  CHARACTER(:), ALLOCATABLE :: astr

  astr = group//"/"//fieldname
  isok0 = hdf5%pathExists(astr)

  IF (isok0) THEN
    CALL hdf5%READ(astr, VALUE)
  END IF

  IF (check .AND. .NOT. isok0) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
      & '[INTERNAL ERROR]:: '//astr//' path does not exists.')
    RETURN
  END IF

  astr = ""
END SUBROUTINE HDF5ReadRealMatrix

!----------------------------------------------------------------------------
!                                                        HDF5ReadRealVector
!----------------------------------------------------------------------------

SUBROUTINE HDF5ReadRealVector(hdf5, VALUE, group, fieldname, myname,  &
  & modname, check)
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
  CHARACTER(*), INTENT(IN) :: group
  CHARACTER(*), INTENT(IN) :: fieldname
  CHARACTER(*), INTENT(IN) :: myname
  CHARACTER(*), INTENT(IN) :: modname
  LOGICAL(LGT), INTENT(IN) :: check

  LOGICAL(LGT) :: isok0
  CHARACTER(:), ALLOCATABLE :: astr

  astr = group//"/"//fieldname
  isok0 = hdf5%pathExists(astr)

  IF (isok0) THEN
    CALL hdf5%READ(astr, VALUE)
  END IF

  IF (check .AND. .NOT. isok0) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
      & '[INTERNAL ERROR]:: '//astr//' path does not exists.')
    RETURN
  END IF

  astr = ""
END SUBROUTINE HDF5ReadRealVector

!----------------------------------------------------------------------------
!                                                         HDF5ReadIntVector
!----------------------------------------------------------------------------

SUBROUTINE HDF5ReadIntVector(hdf5, VALUE, group, fieldname, myname,  &
  & modname, check)
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
  CHARACTER(*), INTENT(IN) :: group
  CHARACTER(*), INTENT(IN) :: fieldname
  CHARACTER(*), INTENT(IN) :: myname
  CHARACTER(*), INTENT(IN) :: modname
  LOGICAL(LGT), INTENT(IN) :: check

  LOGICAL(LGT) :: isok0
  CHARACTER(:), ALLOCATABLE :: astr

  astr = group//"/"//fieldname
  isok0 = hdf5%pathExists(astr)

  IF (isok0) THEN
    CALL hdf5%READ(astr, VALUE)
  END IF

  IF (check .AND. .NOT. isok0) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
      & '[INTERNAL ERROR]:: '//astr//' path does not exists.')
    RETURN
  END IF

  astr = ""

END SUBROUTINE HDF5ReadIntVector

!----------------------------------------------------------------------------
!                                                            HDF5ReadScalar
!----------------------------------------------------------------------------

SUBROUTINE HDF5ReadScalar(hdf5, VALUE, group, fieldname, myname, modname,  &
  & check)
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  CLASS(*), INTENT(INOUT) :: VALUE
  CHARACTER(*), INTENT(IN) :: group
  CHARACTER(*), INTENT(IN) :: fieldname
  CHARACTER(*), INTENT(IN) :: myname
  CHARACTER(*), INTENT(IN) :: modname
  LOGICAL(LGT), INTENT(IN) :: check

  LOGICAL(LGT) :: isok0
  CHARACTER(:), ALLOCATABLE :: astr

  astr = group//"/"//fieldname
  isok0 = hdf5%pathExists(astr)

  IF (check .AND. .NOT. isok0) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
      & '[INTERNAL ERROR]:: '//astr//' path does not exists.')
    RETURN
  END IF

  IF (isok0) THEN
    SELECT TYPE (VALUE)

    TYPE is (INTEGER(I4B))
      CALL hdf5%READ(astr, VALUE)

    TYPE is (REAL(DFP))
      CALL hdf5%READ(astr, VALUE)

    TYPE IS (String)
      CALL hdf5%READ(astr, VALUE)

    TYPE IS (CHARACTER(LEN=*))
      CALL hdf5%READ(astr, VALUE)

    END SELECT
  END IF

  astr = ""

END SUBROUTINE HDF5ReadScalar

!----------------------------------------------------------------------------
!                                                                 ExportDOF
!----------------------------------------------------------------------------

SUBROUTINE ExportDOF(obj, hdf5, group)
  TYPE(DOF_), INTENT(IN) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group
  ! Internal variable
  TYPE(String) :: dsetname
  dsetname = TRIM(group)//"/storageFMT"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), &
    & vals=obj%storageFMT)
  !>
  IF (ALLOCATED(obj%map)) THEN
    dsetname = TRIM(group)//"/map"
    CALL hdf5%WRITE(dsetname=dsetname%chars(), &
      & vals=obj%map)
  END IF
  !>
  IF (ALLOCATED(obj%valMap)) THEN
    dsetname = TRIM(group)//"/valMap"
    CALL hdf5%WRITE(dsetname=dsetname%chars(), &
      & vals=obj%valMap)
  END IF
END SUBROUTINE ExportDOF

!----------------------------------------------------------------------------
!                                                                 ImportDOF
!----------------------------------------------------------------------------

SUBROUTINE ImportDOF(obj, hdf5, group)
  TYPE(DOF_), INTENT(INOUT) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group
  ! Internal variable
  TYPE(String) :: dsetname
  dsetname = TRIM(group)//"/storageFMT"
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%storageFMT)
  !> Map
  dsetname = TRIM(group)//"/map"
  IF (hdf5%pathExists(dsetname%chars())) THEN
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%map)
  END IF
  !> valmap
  dsetname = TRIM(group)//"/valMap"
  IF (hdf5%pathExists(dsetname%chars())) THEN
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%valMap)
  END IF
END SUBROUTINE ImportDOF

!----------------------------------------------------------------------------
!                                                         ExportCSRSparsity
!----------------------------------------------------------------------------

SUBROUTINE ExportCSRSparsity(obj, hdf5, group)
  TYPE(CSRSparsity_), INTENT(IN) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group
  ! Internal variable
  TYPE(String) :: dsetname

  dsetname = TRIM(group)//"/nnz"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), &
    & vals=obj%nnz)
  !>
  dsetname = TRIM(group)//"/ncol"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), &
    & vals=obj%ncol)
  !>
  dsetname = TRIM(group)//"/nrow"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), &
    & vals=obj%nrow)
  !>
  dsetname = TRIM(group)//"/isSorted"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), &
    & vals=obj%isSorted)
  !>
  dsetname = TRIM(group)//"/isInitiated"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), &
    & vals=obj%isInitiated)
  !>
  dsetname = TRIM(group)//"/isSparsityLock"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), &
    & vals=obj%isSparsityLock)
  !>
  CALL ExportDOF(obj=obj%idof, hdf5=hdf5, group=TRIM(group)//"/idof")
  CALL ExportDOF(obj=obj%jdof, hdf5=hdf5, group=TRIM(group)//"/jdof")
  !>
  IF (ALLOCATED(obj%IA)) THEN
    dsetname = TRIM(group)//"/IA"
    CALL hdf5%WRITE(dsetname=dsetname%chars(), &
      & vals=obj%IA)
  END IF
  !>
  IF (ALLOCATED(obj%JA)) THEN
    dsetname = TRIM(group)//"/JA"
    CALL hdf5%WRITE(dsetname=dsetname%chars(), &
      & vals=obj%JA)
  END IF
END SUBROUTINE ExportCSRSparsity

!----------------------------------------------------------------------------
!                                                         ImportCSRSparsity
!----------------------------------------------------------------------------

SUBROUTINE ImportCSRSparsity(obj, hdf5, group)
  TYPE(CSRSparsity_), INTENT(INOUT) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group
  ! Internal variable
  TYPE(String) :: dsetname
  !> nnzz
  dsetname = TRIM(group)//"/nnz"
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%nnz)
  !> ncol
  dsetname = TRIM(group)//"/ncol"
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%ncol)
  !> nrow
  dsetname = TRIM(group)//"/nrow"
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%nrow)
  !> isSorted
  dsetname = TRIM(group)//"/isSorted"
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%isSorted)
  !> isInitiated
  dsetname = TRIM(group)//"/isInitiated"
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%isInitiated)
  !> isSparsityLock
  dsetname = TRIM(group)//"/isSparsityLock"
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%isSparsityLock)
  !> dof
  CALL ImportDOF(obj=obj%idof, hdf5=hdf5, group=TRIM(group)//"/idof")
  CALL ImportDOF(obj=obj%jdof, hdf5=hdf5, group=TRIM(group)//"/jdof")
  !> IA
  dsetname = TRIM(group)//"/IA"
  IF (hdf5%pathExists(dsetname%chars())) THEN
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%IA)
  END IF
  !> JA
  dsetname = TRIM(group)//"/JA"
  IF (hdf5%pathExists(dsetname%chars())) THEN
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%JA)
  END IF
END SUBROUTINE ImportCSRSparsity

!----------------------------------------------------------------------------
!                                                         ExportCSRMatrix
!----------------------------------------------------------------------------

SUBROUTINE ExportCSRMatrix(obj, hdf5, group)
  TYPE(CSRMatrix_), INTENT(IN) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group
  ! Internal variable
  TYPE(String) :: dsetname
  !>
  dsetname = TRIM(group)//"/csrOwnership"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), &
    & vals=obj%csrOwnership)
  !>
  dsetname = TRIM(group)//"/tDimension"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), &
    & vals=obj%tDimension)
  !>
  dsetname = TRIM(group)//"/matrixProp"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), &
    & vals=String(obj%matrixProp))
  !>
  IF (ALLOCATED(obj%A)) THEN
    dsetname = TRIM(group)//"/A"
    CALL hdf5%WRITE(dsetname=dsetname%chars(), &
      & vals=obj%A)
  END IF
  !>
  CALL ExportCSRSparsity(obj=obj%csr, hdf5=hdf5, group=TRIM(group)//"/csr")
END SUBROUTINE ExportCSRMatrix

!----------------------------------------------------------------------------
!                                                            ImportCSRMatrix
!----------------------------------------------------------------------------

SUBROUTINE ImportCSRMatrix(obj, hdf5, group)
  TYPE(CSRMatrix_), INTENT(INOUT) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group
  ! Internal variable
  TYPE(String) :: dsetname, strval
  !> main
  !> csrOwnership
  dsetname = TRIM(group)//"/csrOwnership"
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%csrOwnership)
  !> tDimension
  dsetname = TRIM(group)//"/tDimension"
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%tDimension)
  !> matrixProp
  dsetname = TRIM(group)//"/matrixProp"
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
  obj%matrixProp = strval%chars()
  !>
  dsetname = TRIM(group)//"/A"
  IF (hdf5%pathExists(dsetname%chars())) THEN
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%A)
  END IF
  !>
  dsetname = TRIM(group)//"/csr"
  IF (hdf5%pathExists(dsetname%chars())) THEN
    CALL ImportCSRSparsity(obj=obj%csr, hdf5=hdf5, &
      & group=dsetname%chars())
  END IF
END SUBROUTINE ImportCSRMatrix

!----------------------------------------------------------------------------
!                                                           ExportRealVector
!----------------------------------------------------------------------------

SUBROUTINE ExportRealVector(obj, hdf5, group)
  TYPE(RealVector_), INTENT(IN) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group
  !> internal variables
  TYPE(String) :: dsetname
  !> tDimension
  dsetname = TRIM(group)//"/tDimension"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%tDimension)
  !> Val
  dsetname = TRIM(group)//"/Val"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%Val)
END SUBROUTINE ExportRealVector

!----------------------------------------------------------------------------
!                                                           ImportRealVector
!----------------------------------------------------------------------------

SUBROUTINE ImportRealVector(obj, hdf5, group)
  TYPE(RealVector_), INTENT(INOUT) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group
  !> internal variables
  TYPE(String) :: dsetname
  !> tDimension
  dsetname = TRIM(group)//"/tDimension"
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%tDimension)
  !> Val
  dsetname = TRIM(group)//"/Val"
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%Val)
END SUBROUTINE ImportRealVector

!----------------------------------------------------------------------------
!                                                           ExportIntVector
!----------------------------------------------------------------------------

SUBROUTINE ExportIntVector(obj, hdf5, group)
  TYPE(IntVector_), INTENT(IN) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group
  !> internal variables
  TYPE(String) :: dsetname
  !> tDimension
  dsetname = TRIM(group)//"/tDimension"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%tDimension)
  !> Val
  dsetname = TRIM(group)//"/Val"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%Val)
END SUBROUTINE ExportIntVector

!----------------------------------------------------------------------------
!                                                           ImportIntVector
!----------------------------------------------------------------------------

SUBROUTINE ImportIntVector(obj, hdf5, group)
  TYPE(IntVector_), INTENT(INOUT) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group
  !> internal variables
  TYPE(String) :: dsetname
  !> tDimension
  dsetname = TRIM(group)//"/tDimension"
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%tDimension)
  !> Val
  dsetname = TRIM(group)//"/Val"
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%Val)
END SUBROUTINE ImportIntVector

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE HDF5File_Method
