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
! summary: This module contains constructor method for [[MatrixField_]]

SUBMODULE(MatrixField_Class) HDFMethods
USE AbstractField_Class, ONLY: AbstractFieldExport
USE AbstractField_Class, ONLY: AbstractFieldImport
USE AbstractMatrixField_Class, ONLY: AbstractMatrixFieldDisplay
USE BaseType, ONLY: DOF_
USE CSRMatrix_Method, ONLY: CSRMatrix_SPY => SPY
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: Tostring
USE HDF5FileUtility, ONLY: ExportCSRMatrix, ImportCSRMatrix
USE MatrixFieldUtility, ONLY: Export_Header
USE MatrixFieldUtility, ONLY: Import_Header
USE MatrixFieldUtility, ONLY: Import_CheckError
USE String_Class, ONLY: String
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "MatrixField_Class@HDFMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Export
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Export
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Export()"
#endif

TYPE(String) :: dname, matprop
TYPE(DOF_), POINTER :: dofobj

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractFieldExport(obj=obj, hdf5=hdf5, group=group)

!From MatrixFieldUtility
CALL Export_Header(obj=obj, hdf5=hdf5, group=group, &
                   dname=dname, matprop=matprop)

! mat
CALL ExportCSRMatrix(obj=obj%mat, hdf5=hdf5, group=TRIM(group)//"/mat")

! pmat
CALL obj%ExportPmat(hdf5=hdf5, group=group)

NULLIFY (dofobj)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!                                                                 Export
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ExportPmat
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ExportPmat()"
#endif

LOGICAL(LGT) :: isok
TYPE(String) :: dname

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = obj%isPmatInitiated

IF (.NOT. isok) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

! pmat/pmatName
dname = TRIM(group)//"/pmat/pmatName"
CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), vals=obj%pmat%pmatName)

! pmat/nnz
dname = TRIM(group)//"/pmat/nnz"
CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), vals=obj%pmat%nnz)

! pmat/ncol
dname = TRIM(group)//"/pmat/ncol"
CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), vals=obj%pmat%ncol)

! pmat/nrow
dname = TRIM(group)//"/pmat/nrow"
CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), vals=obj%pmat%nrow)

! pmat/isInitiated
dname = TRIM(group)//"/pmat/isInitiated"
CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), vals=obj%pmat%isInitiated)

! pmat/lfil
dname = TRIM(group)//"/pmat/lfil"
CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), vals=obj%pmat%lfil)

! pmat/mbloc
dname = TRIM(group)//"/pmat/mbloc"
CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), vals=obj%pmat%mbloc)

! pmat/alpha
dname = TRIM(group)//"/pmat/alpha"
CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), vals=obj%pmat%alpha)

! pmat/droptol
dname = TRIM(group)//"/pmat/droptol"
CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), vals=obj%pmat%droptol)

! pmat/permtol
dname = TRIM(group)//"/pmat/permtol"
CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), vals=obj%pmat%permtol)

! pmat/A
IF (ALLOCATED(obj%pmat%A)) THEN
  dname = TRIM(group)//"/pmat/A"
  CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), vals=obj%pmat%A)
END IF

! pmat/JA
IF (ALLOCATED(obj%pmat%JA)) THEN
  dname = TRIM(group)//"/pmat/JA"
  CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), vals=obj%pmat%JA)
END IF

! pmat/IA
IF (ALLOCATED(obj%pmat%IA)) THEN
  dname = TRIM(group)//"/pmat/IA"
  CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), vals=obj%pmat%IA)
END IF

! pmat/JU
IF (ALLOCATED(obj%pmat%JU)) THEN
  dname = TRIM(group)//"/pmat/JU"
  CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), vals=obj%pmat%JU)
END IF

! pmat/IPERM
IF (ALLOCATED(obj%pmat%IPERM)) THEN
  dname = TRIM(group)//"/pmat/IPERM"
  CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), vals=obj%pmat%IPERM)
END IF

! pmat/LEVS
IF (ALLOCATED(obj%pmat%LEVS)) THEN
  dname = TRIM(group)//"/pmat/LEVS"
  CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), vals=obj%pmat%LEVS)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ExportPmat

!----------------------------------------------------------------------------
!                                                                 Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
#endif

TYPE(String) :: dsetname, matrixProp
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

!From MatrixFieldUtility
CALL Import_CheckError(obj=obj, hdf5=hdf5, group=group)

! name, engine, fieldType, dof and the fedof pointers
CALL AbstractFieldImport( &
  obj=obj, hdf5=hdf5, group=group, fedof=fedof, fedofs=fedofs, &
  timefedof=timefedof, timefedofs=timefedofs, geofedof=geofedof, &
  geofedofs=geofedofs)

! From MatrixFieldUtility
CALL Import_Header(obj=obj, hdf5=hdf5, group=group, &
                   matrixProp=matrixProp, isRectangle=obj%isRectangle)

! mat
dsetname = TRIM(group)//"/mat"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//"::"//myName//" - "// &
                        "Importing "//dsetname%chars())
#endif

isok = hdf5%PathExists(dsetname%chars())

#ifdef DEBUG_VER
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
  CALL obj%ImportPmat(hdf5=hdf5, group=dsetname%chars(), &
                      fedof=fedof, fedofs=fedofs)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                                                 Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportPmat
CHARACTER(*), PARAMETER :: myName = "obj_ImportPmat"
TYPE(String) :: dsetname

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//"::"//myName//" - "// &
                        "Importing an Instance of MatrixFieldPrecondition_")
#endif

obj%isPmatInitiated = .TRUE.

! pmatName
dsetname = TRIM(group)//"/pmatName"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=obj%pmat%pmatName)
END IF

! nnz
dsetname = TRIM(group)//"/nnz"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=obj%pmat%nnz)
END IF

! ncol
dsetname = TRIM(group)//"/ncol"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=obj%pmat%ncol)
END IF

! nrow
dsetname = TRIM(group)//"/nrow"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=obj%pmat%nrow)
END IF

! isInitiated
dsetname = TRIM(group)//"/isInitiated"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=obj%pmat%isInitiated)
END IF

! lfil
dsetname = TRIM(group)//"/lfil"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=obj%pmat%lfil)
END IF

! mbloc
dsetname = TRIM(group)//"/mbloc"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=obj%pmat%mbloc)
END IF

! alpha
dsetname = TRIM(group)//"/alpha"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=obj%pmat%alpha)
END IF

! droptol
dsetname = TRIM(group)//"/droptol"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=obj%pmat%droptol)
END IF

! permtol
dsetname = TRIM(group)//"/permtol"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=obj%pmat%permtol)
END IF

! A
dsetname = TRIM(group)//"/A"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=obj%pmat%A)
END IF

! JA
dsetname = TRIM(group)//"/JA"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=obj%pmat%JA)
END IF

! IA
dsetname = TRIM(group)//"/IA"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=obj%pmat%IA)
END IF

! JU
dsetname = TRIM(group)//"/JU"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=obj%pmat%JU)
END IF

! IPERM
dsetname = TRIM(group)//"/IPERM"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=obj%pmat%IPERM)
END IF

! LEVS
dsetname = TRIM(group)//"/LEVS"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=obj%pmat%LEVS)
END IF

END PROCEDURE obj_ImportPmat

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE HDFMethods
