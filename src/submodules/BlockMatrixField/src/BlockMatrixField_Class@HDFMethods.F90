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
! date: 16 July 2021
! summary: HDF5 methods of [[BlockMatrixField_]]

SUBMODULE(BlockMatrixField_Class) HDFMethods
USE AbstractField_Class, ONLY: AbstractFieldImport
USE AbstractMatrixField_Class, ONLY: AbstractMatrixFieldSetFEDOF
USE HDF5FileUtility, ONLY: ImportCSRMatrix
USE MatrixFieldUtility, ONLY: Import_Header
USE MatrixFieldUtility, ONLY: Import_CheckError
USE String_Class, ONLY: String
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "BlockMatrixField_Class@HDFMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
#endif

TYPE(String) :: dsetname, matrixProp
INTEGER(I4B) :: ii, tvar
LOGICAL(LGT) :: isok
TYPE(FEDOFPointer_), ALLOCATABLE :: rowPtrs(:), colPtrs(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = PRESENT(rowFedof) .OR. PRESENT(rowFedofs)
CALL AssertError1(isok, myName, &
                  "Either rowFedof or rowFedofs should be present")
#endif

! From MatrixFieldUtility
CALL Import_CheckError(obj=obj, hdf5=hdf5, group=group)

! name, engine, fieldType, comm and sizes
CALL AbstractFieldImport(obj=obj, hdf5=hdf5, group=group)

! From MatrixFieldUtility
CALL Import_Header(obj=obj, hdf5=hdf5, group=group, &
                   matrixProp=matrixProp, isRectangle=obj%isRectangle)

! tPhysicalVarNames
dsetname = TRIM(group)//"/tPhysicalVarNames"

#ifdef DEBUG_VER
isok = hdf5%PathExists(dsetname%chars())
CALL AssertError1(isok, myName, 'dataset '//dsetname//' should be present')
#endif

CALL hdf5%READ(dsetname=dsetname%chars(), vals=tvar)

! one FEDOF pointer for each physical variable; a single FEDOF is
! used for all physical variables
IF (PRESENT(rowFedofs)) THEN
  rowPtrs = rowFedofs
ELSE IF (PRESENT(rowFedof)) THEN
  ALLOCATE (rowPtrs(tvar))
  DO ii = 1, tvar
    rowPtrs(ii)%ptr => rowFedof
  END DO
END IF

IF (PRESENT(colFedofs)) THEN
  colPtrs = colFedofs
ELSE IF (PRESENT(colFedof)) THEN
  ALLOCATE (colPtrs(tvar))
  DO ii = 1, tvar
    colPtrs(ii)%ptr => colFedof
  END DO
END IF

! an absent column argument takes the row value (square matrix);
! an unallocated array counts as absent
CALL AbstractMatrixFieldSetFEDOF( &
  obj=obj, rowFedof=rowFedof, rowFedofs=rowPtrs, rowGeoFedof=rowGeoFedof, &
  rowGeoFedofs=rowGeoFedofs, colFedof=colFedof, colFedofs=colPtrs, &
  colGeoFedof=colGeoFedof, colGeoFedofs=colGeoFedofs)

! mat
dsetname = TRIM(group)//"/mat"

#ifdef DEBUG_VER
isok = hdf5%PathExists(dsetname%chars())
CALL AssertError1(isok, myName, &
                  "[WIP ERROR] :: "//dsetname//" should be present. "// &
                  "Import without /mat is under development")
#endif

CALL ImportCSRMatrix(obj=obj%mat, hdf5=hdf5, group=dsetname%chars())

obj%isInit = .TRUE.
obj%isPmatInitiated = .FALSE.

! pmat is present only if the precondition was set before Export
dsetname = TRIM(group)//"/pmat"
isok = hdf5%PathExists(dsetname%chars())
IF (isok) THEN
  CALL obj%ImportPmat(hdf5=hdf5, group=dsetname%chars(), fedof=rowFedof, &
                      fedofs=rowPtrs)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE HDFMethods
