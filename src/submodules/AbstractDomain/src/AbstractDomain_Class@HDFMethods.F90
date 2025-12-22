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
USE HDF5File_Method, ONLY: HDF5ReadScalar, &
                           HDF5ReadVector, &
                           HDF5ReadMatrix
IMPLICIT NONE
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
                    "AbstractDomain_Class::obj is already initiated.")
#endif

#ifdef DEBUG_VER
  isok = hdf5%isOpen()
  CALL AssertError1(isok, myName0, &
                    "HDF5File_Class::HDF5 file is not opened.")
#endif

#ifdef DEBUG_VER
  isok = hdf5%isRead()
  CALL AssertError1(isok, myName0, &
                   "HDF5File_Class::HDF5 file does not have read permission.")
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

  obj%isInit = .TRUE.

  ! read engine
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group, &
                      VALUE=obj%engine, fieldname="engine", &
                      myName=myName, modName=modName)

  ! read majorVersion
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group, &
                      VALUE=obj%majorVersion, fieldname="majorVersion", &
                      myName=myName, modName=modName)

  ! read minorVersion
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group, &
                      VALUE=obj%minorVersion, fieldname="minorVersion", &
                      myName=myName, modName=modName)

  ! read version
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group, &
                      VALUE=obj%version, fieldname="version", &
                      myName=myName, modName=modName)

  ! read NSD
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group, &
                      VALUE=obj%NSD, fieldname="NSD", &
                      myName=myName, modName=modName)

  ! maxNptrs
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group, &
                      VALUE=obj%maxNptrs, fieldname="maxNptrs", &
                      myName=myName, modName=modName)

  ! minNptrs
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group, &
                      VALUE=obj%minNptrs, fieldname="minNptrs", &
                      myName=myName, modName=modName)

  ! tNodes
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group, &
                      VALUE=obj%tNodes, fieldname="tNodes", &
                      myName=myName, modName=modName)

  ! nodeCoord
  CALL HDF5ReadMatrix(hdf5=hdf5, check=.TRUE., group=group, &
                      VALUE=obj%nodeCoord, fieldname="nodeCoord", &
                      myName=myName, modName=modName)

  ! is node number sparse
  isok = (obj%maxNptrs - obj%minNptrs) .EQ. (obj%tNodes - 1)
  obj%isNodeNumberSparse = .TRUE.
  IF (isok) obj%isNodeNumberSparse = .FALSE.

  ! maxElemNum
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group, &
                      VALUE=obj%maxElemNum, fieldname="maxElemNum", &
                      myName=myName, modName=modName)

  ! minElemNum
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group, &
                      VALUE=obj%minElemNum, fieldname="minElemNum", &
                      myName=myName, modName=modName)

  ! tEntitiesForNodes
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group, &
                      VALUE=obj%tEntitiesForNodes, &
                      fieldname="tEntitiesForNodes", &
                      myName=myName, modName=modName)

  ! tEntitiesForElements
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group, &
                      VALUE=obj%tEntitiesForElements, &
                      fieldname="tEntitiesForElements", &
                      myName=myName, modName=modName)

  ! numVolumeEntities
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group, &
                      VALUE=obj%tEntities(3), &
                      fieldname="numVolumeEntities", &
                      myName=myName, modName=modName)

  ! numSurfaceEntities
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group, &
                      VALUE=obj%tEntities(2), &
                      fieldname="numSurfaceEntities", &
                      myName=myName, modName=modName)

  ! numCurveEntities
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group, &
                      VALUE=obj%tEntities(1), &
                      fieldname="numCurveEntities", &
                      myName=myName, modName=modName)

  ! numPointEntities
  CALL HDF5ReadScalar(hdf5=hdf5, check=.TRUE., group=group, &
                      VALUE=obj%tEntities(0), &
                      fieldname="numPointEntities", &
                      myName=myName, modName=modName)

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
