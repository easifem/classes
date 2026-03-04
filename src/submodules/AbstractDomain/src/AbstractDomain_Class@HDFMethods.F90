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

SUBMODULE(AbstractDomain_Class) HDFMethods
USE Display_Method, ONLY: ToString
USE HDF5FileUtility, ONLY: HDF5ReadScalar
USE HDF5FileUtility, ONLY: HDF5ReadVector
USE HDF5FileUtility, ONLY: HDF5ReadMatrix
USE BaseType, ONLY: math => TypeMathOpt
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "AbstractDomain_Class@HDFMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                     Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractDomainImportCheckErr(obj=obj, hdf5=hdf5)
CALL AbstractDomainImportMetaData(obj=obj, hdf5=hdf5, group=group)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                               AbstractDomainImportCheckErr
!----------------------------------------------------------------------------

SUBROUTINE AbstractDomainImportCheckErr(obj, hdf5)
  CLASS(AbstractDomain_), INTENT(INOUT) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5

  ! internal variable
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName0 = "AbstractDomainImportCheckErr()"
#endif
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName0//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  isok = .NOT. obj%isInit
  CALL AssertError1(isok, myName0, &
                    "obj is already initiated.")
#endif

#ifdef DEBUG_VER
  isok = hdf5%isOpen()
  CALL AssertError1(isok, myName0, &
                    "HDF5 file is not opened.")
#endif

#ifdef DEBUG_VER
  isok = hdf5%isRead()
  CALL AssertError1(isok, myName0, &
                    "HDF5 file does not have read permission.")
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName0//' - '// &
                          '[END] ')
#endif
END SUBROUTINE AbstractDomainImportCheckErr

!----------------------------------------------------------------------------
!                                               AbstractDomainImportMetaData
!----------------------------------------------------------------------------

SUBROUTINE AbstractDomainImportMetaData(obj, hdf5, group)
  CLASS(AbstractDomain_), INTENT(INOUT) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "AbstractDomainImportMetaData()"
#endif
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  obj%isInit = math%yes

  ! read engine
  CALL HDF5ReadScalar(hdf5=hdf5, check=math%yes, group=group, &
                      VALUE=obj%engine, fieldname="engine")

  ! read majorVersion
  CALL HDF5ReadScalar(hdf5=hdf5, check=math%yes, group=group, &
                      VALUE=obj%majorVersion, fieldname="majorVersion")

  ! read minorVersion
  CALL HDF5ReadScalar(hdf5=hdf5, check=math%yes, group=group, &
                      VALUE=obj%minorVersion, fieldname="minorVersion")

  ! read version
  CALL HDF5ReadScalar(hdf5=hdf5, check=math%yes, group=group, &
                      VALUE=obj%version, fieldname="version")

  ! read NSD
  CALL HDF5ReadScalar(hdf5=hdf5, check=math%yes, group=group, &
                      VALUE=obj%NSD, fieldname="NSD")

  ! maxNptrs
  CALL HDF5ReadScalar(hdf5=hdf5, check=math%yes, group=group, &
                      VALUE=obj%maxNptrs, fieldname="maxNptrs")

  ! minNptrs
  CALL HDF5ReadScalar(hdf5=hdf5, check=math%yes, group=group, &
                      VALUE=obj%minNptrs, fieldname="minNptrs")

  ! tNodes
  CALL HDF5ReadScalar(hdf5=hdf5, check=math%yes, group=group, &
                      VALUE=obj%tNodes, fieldname="tNodes")

  ! nodeCoord
  CALL HDF5ReadMatrix(hdf5=hdf5, check=math%yes, group=group, &
                      VALUE=obj%nodeCoord, fieldname="nodeCoord")

  ! is node number sparse
  isok = (obj%maxNptrs - obj%minNptrs) .EQ. (obj%tNodes - 1)
  obj%isNodeNumberSparse = math%yes
  IF (isok) obj%isNodeNumberSparse = math%no

  ! maxElemNum
  CALL HDF5ReadScalar(hdf5=hdf5, check=math%yes, group=group, &
                      VALUE=obj%maxElemNum, fieldname="maxElemNum")

  ! minElemNum
  CALL HDF5ReadScalar(hdf5=hdf5, check=math%yes, group=group, &
                      VALUE=obj%minElemNum, fieldname="minElemNum")

  ! tEntitiesForNodes
  CALL HDF5ReadScalar(hdf5=hdf5, check=math%yes, group=group, &
                      VALUE=obj%tEntitiesForNodes, &
                      fieldname="tEntitiesForNodes")

  ! tEntitiesForElements
  CALL HDF5ReadScalar(hdf5=hdf5, check=math%yes, group=group, &
                      VALUE=obj%tEntitiesForElements, &
                      fieldname="tEntitiesForElements")

  ! numVolumeEntities
  CALL HDF5ReadScalar(hdf5=hdf5, check=math%yes, group=group, &
                      VALUE=obj%tEntities(3), &
                      fieldname="numVolumeEntities")

  ! numSurfaceEntities
  CALL HDF5ReadScalar(hdf5=hdf5, check=math%yes, group=group, &
                      VALUE=obj%tEntities(2), &
                      fieldname="numSurfaceEntities")

  ! numCurveEntities
  CALL HDF5ReadScalar(hdf5=hdf5, check=math%yes, group=group, &
                      VALUE=obj%tEntities(1), &
                      fieldname="numCurveEntities")

  ! numPointEntities
  CALL HDF5ReadScalar(hdf5=hdf5, check=math%yes, group=group, &
                      VALUE=obj%tEntities(0), &
                      fieldname="numPointEntities")

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE AbstractDomainImportMetaData

!----------------------------------------------------------------------------
!                                                               Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE HDFMethods
