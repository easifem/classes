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

SUBMODULE(AbstractNodeField_Class) HDFMethods
USE String_Class, ONLY: String
USE BaseType, ONLY: math => TypeMathOpt
USE AbstractField_Class, ONLY: AbstractFieldImport
USE AbstractField_Class, ONLY: AbstractFieldExport
USE HDF5FileUtility, ONLY: ImportRealVector
USE HDF5FileUtility, ONLY: ImportDOF
USE HDF5FileUtility, ONLY: ExportRealVector
USE HDF5FileUtility, ONLY: ImportRealVector

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
#endif

TYPE(String) :: dsetname
TYPE(String), ALLOCATABLE :: tempStrs(:)
INTEGER(I4B) :: ii, tsize
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractFieldImport( &
  obj=obj, hdf5=hdf5, group=group, fedof=fedof, fedofs=fedofs, &
  geofedof=geofedof, geofedofs=geofedofs)

dsetname = TRIM(group)//"/INTR0/dof_tPhysicalVars"
isok = hdf5%pathExists(dsetname%chars())
IF (isok) CALL hdf5%READ(dsetname=dsetname%chars(), &
                         vals=obj%dof_tPhysicalVars)

dsetname = TRIM(group)//"/INTR0/dof_storageFMT"
isok = hdf5%pathExists(dsetname%chars())
IF (isok) CALL hdf5%READ(dsetname=dsetname%chars(), &
                         vals=obj%dof_storageFMT)

dsetname = TRIM(group)//"/INTR0/tSize"
isok = hdf5%pathExists(dsetname%chars())
IF (isok) CALL hdf5%READ(dsetname=dsetname%chars(), &
                         vals=obj%tSize)

dsetname = TRIM(group)//"/INTR1/dof_spaceCompo"
isok = hdf5%pathExists(dsetname%chars())
IF (isok) CALL hdf5%READ(dsetname=dsetname%chars(), &
                         vals=obj%dof_spaceCompo)

dsetname = TRIM(group)//"/INTR1/dof_timeCompo"
isok = hdf5%pathExists(dsetname%chars())
IF (isok) CALL hdf5%READ(dsetname=dsetname%chars(), &
                         vals=obj%dof_timeCompo)

dsetname = TRIM(group)//"/INTR1/dof_tNodes"
isok = hdf5%pathExists(dsetname%chars())
IF (isok) CALL hdf5%READ(dsetname=dsetname%chars(), &
                         vals=obj%dof_tNodes)

! StringR1
dsetname = TRIM(group)//"/StringR1/dof_names_char"
isok = hdf5%pathExists(dsetname%chars())
IF (isok) THEN
  CALL hdf5%READ(dsetname=dsetname%Chars(), vals=tempStrs)
  tsize = SIZE(tempStrs)
  ALLOCATE (obj%dof_names_char(tSize))
  DO ii = 1, tsize
    obj%dof_names_char(ii) = tempStrs(ii)%slice(1, 1)
    tempStrs(ii) = ""
  END DO
  DEALLOCATE (tempStrs)
END IF

dsetname = TRIM(group)//"/RealVectorR0/realVec"
isok = hdf5%pathExists(dsetname%chars())
IF (isok) CALL ImportRealVector(obj=obj%realvec, hdf5=hdf5, &
                                group=dsetname%chars())

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                                                    Export
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Export
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Export()"
#endif
LOGICAL(LGT) :: isok

TYPE(String) :: dsetname
TYPE(String), ALLOCATABLE :: tempStrs(:)
INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractFieldExport(obj=obj, hdf5=hdf5, group=group)

! Integer Scalars
! ---------------
! dof_tPhysicalVars
dsetname = TRIM(group)//"/IntR0/dof_tPhysicalVars"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%dof_tPhysicalVars)

! dof_storageFMT
dsetname = TRIM(group)//"/IntR0/dof_storageFMT"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%dof_storageFMT)

! tSize
dsetname = TRIM(group)//"/IntR0/tSize"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%tSize)

! Integer Vectors
! ---------------

isok = ALLOCATED(obj%dof_spaceCompo)
IF (isok) THEN
  dsetname = TRIM(group)//"/IntR1/dof_spaceCompo"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%dof_spaceCompo)
END IF

isok = ALLOCATED(obj%dof_timeCompo)
IF (isok) THEN
  dsetname = TRIM(group)//"/IntR1/dof_timeCompo"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%dof_timeCompo)
END IF

isok = ALLOCATED(obj%dof_tNodes)
IF (isok) THEN
  dsetname = TRIM(group)//"/IntR1/dof_tNodes"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%dof_tNodes)
END IF

! StringR1
isok = ALLOCATED(obj%dof_names_char)
IF (isok) THEN
  tsize = SIZE(obj%dof_names_char)
  ALLOCATE (tempStrs(tsize))
  DO ii = 1, tsize
    tempStrs(ii) = obj%dof_names_char(ii)
  END DO
  dsetname = TRIM(group)//"/StringR1/dof_names_char"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=tempStrs)
  DO ii = 1, tsize
    tempStrs(ii) = ""
  END DO
  DEALLOCATE (tempStrs)
END IF

! realVec
dsetname = TRIM(group)//"/RealVectorR0/realVec"
CALL ExportRealVector(obj=obj%realVec, hdf5=hdf5, group=dsetname%chars())

! info
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE HDFMethods
