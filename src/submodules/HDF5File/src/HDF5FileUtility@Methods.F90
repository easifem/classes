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

SUBMODULE(HDF5FileUtility) Methods
USE ExceptionHandler_Class, ONLY: e
USE BaseType, ONLY: math => TypeMathOpt
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "HDF5File_Method.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                               obj_Import_GetEntities
!----------------------------------------------------------------------------

MODULE PROCEDURE HDF5GetEntities
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "HDF5GetEntities()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

SELECT CASE (dim)

CASE (0)
  ! numPointEntities
  CALL HDF5ReadScalar(hdf5=hdf5, check=math%yes, group=group, &
                      VALUE=tEntities, fieldname="numPointEntities")

CASE (1)
  ! numCurveEntities
  CALL HDF5ReadScalar(hdf5=hdf5, check=math%yes, group=group, &
                      VALUE=tEntities, fieldname="numCurveEntities")

CASE (2)
  ! numSurfaceEntities
  CALL HDF5ReadScalar(hdf5=hdf5, check=math%yes, group=group, &
                      VALUE=tEntities, fieldname="numSurfaceEntities")

CASE (3)
  ! numVolumeEntities
  CALL HDF5ReadScalar(hdf5=hdf5, check=math%yes, group=group, &
                      VALUE=tEntities, fieldname="numVolumeEntities")

CASE DEFAULT
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
                    'no case found for given dim.')
#endif
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE HDF5GetEntities

!----------------------------------------------------------------------------
!                                                          HDF5ReadIntMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE HDF5ReadIntMatrix
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "HDF5ReadIntMatrix()"
#endif

LOGICAL(LGT) :: isok0
CHARACTER(:), ALLOCATABLE :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

astr = group//"/"//fieldname
isok0 = hdf5%pathExists(astr)
IF (isok0) THEN
  CALL hdf5%READ(astr, VALUE)
END IF

#ifdef DEBUG_VER
IF (check) THEN
  CALL AssertError1(isok0, myName, astr//' path does not exists.')
END IF
#endif

astr = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE HDF5ReadIntMatrix

!----------------------------------------------------------------------------
!                                                         HDF5ReadRealMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE HDF5ReadRealMatrix
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "HDF5ReadRealMatrix()"
#endif
LOGICAL(LGT) :: isok0
CHARACTER(:), ALLOCATABLE :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

astr = group//"/"//fieldname
isok0 = hdf5%pathExists(astr)
IF (isok0) THEN
  CALL hdf5%READ(astr, VALUE)
END IF

#ifdef DEBUG_VER
IF (check) THEN
  CALL AssertError1(isok0, myName, astr//' path does not exists.')
END IF
#endif

astr = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE HDF5ReadRealMatrix

!----------------------------------------------------------------------------
!                                                        HDF5ReadRealVector
!----------------------------------------------------------------------------

MODULE PROCEDURE HDF5ReadRealVector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "HDF5ReadRealVector()"
#endif
LOGICAL(LGT) :: isok0
CHARACTER(:), ALLOCATABLE :: astr

astr = group//"/"//fieldname
isok0 = hdf5%pathExists(astr)

IF (isok0) THEN
  CALL hdf5%READ(astr, VALUE)
END IF

#ifdef DEBUG_VER
IF (check) THEN
  CALL AssertError1(isok0, myName, astr//' path does not exists.')
END IF
#endif

astr = ""
END PROCEDURE HDF5ReadRealVector

!----------------------------------------------------------------------------
!                                                         HDF5ReadIntVector
!----------------------------------------------------------------------------

MODULE PROCEDURE HDF5ReadIntVector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "HDF5ReadIntVector()"
#endif
LOGICAL(LGT) :: isok0
CHARACTER(:), ALLOCATABLE :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

astr = group//"/"//fieldname
isok0 = hdf5%pathExists(astr)

IF (isok0) THEN
  CALL hdf5%READ(astr, VALUE)
END IF

#ifdef DEBUG_VER
IF (check) THEN
  CALL AssertError1(isok0, myName, astr//' path does not exists.')
END IF
#endif

astr = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE HDF5ReadIntVector

!----------------------------------------------------------------------------
!                                                            HDF5ReadScalar
!----------------------------------------------------------------------------

MODULE PROCEDURE HDF5ReadScalar
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "HDF5ReadScalar()"
#endif
LOGICAL(LGT) :: isok0
CHARACTER(:), ALLOCATABLE :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

astr = group//"/"//fieldname
isok0 = hdf5%pathExists(astr)

#ifdef DEBUG_VER
IF (check) THEN
  CALL AssertError1(isok0, myName, astr//' path does not exists.')
END IF
#endif

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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE HDF5ReadScalar

!----------------------------------------------------------------------------
!                                                                 ExportDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE ExportDOF
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ExportDOF()"
#endif
TYPE(String) :: dsetname

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dsetname = TRIM(group)//"/storageFMT"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%storageFMT)

IF (ALLOCATED(obj%map)) THEN
  dsetname = TRIM(group)//"/map"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%map)
END IF

IF (ALLOCATED(obj%valMap)) THEN
  dsetname = TRIM(group)//"/valMap"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%valMap)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE ExportDOF

!----------------------------------------------------------------------------
!                                                                 ImportDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE ImportDOF
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ImportDOF()"
#endif

TYPE(String) :: dsetname
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dsetname = TRIM(group)//"/storageFMT"
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%storageFMT)

dsetname = TRIM(group)//"/map"
isok = hdf5%pathExists(dsetname%chars())
IF (isok) &
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%map)

dsetname = TRIM(group)//"/valMap"
isok = hdf5%pathExists(dsetname%chars())
IF (isok) &
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%valMap)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE ImportDOF

!----------------------------------------------------------------------------
!                                                         ExportCSRSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE ExportCSRSparsity
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ExportCSRSparsity()"
#endif
TYPE(String) :: dsetname

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dsetname = TRIM(group)//"/nnz"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%nnz)

dsetname = TRIM(group)//"/ncol"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%ncol)

dsetname = TRIM(group)//"/nrow"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%nrow)

dsetname = TRIM(group)//"/isSorted"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%isSorted)

dsetname = TRIM(group)//"/isInitiated"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%isInitiated)

dsetname = TRIM(group)//"/isSparsityLock"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%isSparsityLock)

CALL ExportDOF(obj=obj%idof, hdf5=hdf5, group=TRIM(group)//"/idof")
CALL ExportDOF(obj=obj%jdof, hdf5=hdf5, group=TRIM(group)//"/jdof")

IF (ALLOCATED(obj%IA)) THEN
  dsetname = TRIM(group)//"/IA"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%IA)
END IF

IF (ALLOCATED(obj%JA)) THEN
  dsetname = TRIM(group)//"/JA"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%JA)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE ExportCSRSparsity

!----------------------------------------------------------------------------
!                                                         ImportCSRSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE ImportCSRSparsity
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ImportCSRSparsity()"
#endif
TYPE(String) :: dsetname

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE ImportCSRSparsity

!----------------------------------------------------------------------------
!                                                         ExportCSRMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE ExportCSRMatrix
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ExportCSRMatrix()"
#endif
TYPE(String) :: dsetname

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dsetname = TRIM(group)//"/csrOwnership"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%csrOwnership)

dsetname = TRIM(group)//"/tDimension"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%tDimension)

dsetname = TRIM(group)//"/matrixProp"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=String(obj%matrixProp))

IF (ALLOCATED(obj%A)) THEN
  dsetname = TRIM(group)//"/A"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%A)
END IF

CALL ExportCSRSparsity(obj=obj%csr, hdf5=hdf5, group=TRIM(group)//"/csr")

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE ExportCSRMatrix

!----------------------------------------------------------------------------
!                                                            ImportCSRMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE ImportCSRMatrix
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ImportCSRMatrix()"
#endif
TYPE(String) :: dsetname, strval

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

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

dsetname = TRIM(group)//"/A"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%A)
END IF

dsetname = TRIM(group)//"/csr"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL ImportCSRSparsity(obj=obj%csr, hdf5=hdf5, group=dsetname%chars())
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE ImportCSRMatrix

!----------------------------------------------------------------------------
!                                                           ExportRealVector
!----------------------------------------------------------------------------

MODULE PROCEDURE ExportRealVector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ExportRealVector()"
#endif
TYPE(String) :: dsetname

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

!> tDimension
dsetname = TRIM(group)//"/tDimension"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%tDimension)

!> Val
dsetname = TRIM(group)//"/Val"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%Val)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE ExportRealVector

!----------------------------------------------------------------------------
!                                                           ImportRealVector
!----------------------------------------------------------------------------

MODULE PROCEDURE ImportRealVector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ImportRealVector()"
#endif
TYPE(String) :: dsetname

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dsetname = TRIM(group)//"/tDimension"
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%tDimension)

dsetname = TRIM(group)//"/Val"
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%Val)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE ImportRealVector

!----------------------------------------------------------------------------
!                                                           ExportIntVector
!----------------------------------------------------------------------------

MODULE PROCEDURE ExportIntVector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ExportIntVector()"
#endif
TYPE(String) :: dsetname

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

!> tDimension
dsetname = TRIM(group)//"/tDimension"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%tDimension)
!> Val
dsetname = TRIM(group)//"/Val"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%Val)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE ExportIntVector

!----------------------------------------------------------------------------
!                                                           ImportIntVector
!----------------------------------------------------------------------------

MODULE PROCEDURE ImportIntVector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ImportIntVector()"
#endif
TYPE(String) :: dsetname

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

!> tDimension
dsetname = TRIM(group)//"/tDimension"
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%tDimension)
!> Val
dsetname = TRIM(group)//"/Val"
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%Val)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE ImportIntVector

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
