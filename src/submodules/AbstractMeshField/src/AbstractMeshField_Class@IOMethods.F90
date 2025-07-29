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

SUBMODULE(AbstractMeshField_Class) IOMethods
USE Display_Method, ONLY: Display, ToString
USE SafeSizeUtility, ONLY: SafeSize
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif
LOGICAL(LGT) :: bool1

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(obj%isInit, 'Object INITIATED: ', unitno=unitno)

IF (.NOT. obj%isInit) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

CALL Display('name: '//obj%name%chars(), unitno=unitno)
CALL Display('prefix: '//obj%GetPrefix(), unitno=unitno)

CALL Display('fieldType: '//typefield%ToString(obj%fieldType), &
             unitno=unitno)

CALL Display('engine: '//obj%engine%chars(), unitno=unitno)

CALL Display(obj%tSize, 'tSize: ', unitno=unitno)

IF (obj%defineOn .EQ. fevaropt%nodal) THEN
  CALL Display('defineOn: Nodal', unitno=unitno)
ELSE
  CALL Display('defineOn: Quadrature', unitno=unitno)
END IF

SELECT CASE (obj%rank)
CASE (fevaropt%scalar)
  CALL Display('rank: Scalar', unitno=unitno)
CASE (fevaropt%vector)
  CALL Display('rank: Vector', unitno=unitno)
CASE (fevaropt%matrix)
  CALL Display('rank: Matrix', unitno=unitno)

#ifdef DEBUG_VER
CASE DEFAULT
  CALL assertError1(.FALSE., myName, &
                    'No case found for rank = '//ToString(obj%rank))
#endif

END SELECT

SELECT CASE (obj%varType)
CASE (typefield%constant)
  CALL Display('varType: Constant', unitno=unitno)
CASE (typefield%space)
  CALL Display('varType: Space', unitno=unitno)
CASE (typefield%time)
  CALL Display('varType: Time', unitno=unitno)
CASE (typefield%spaceTime)
  CALL Display('varType: SpaceTime', unitno=unitno)

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myName, &
                    'No case found for varType = '//ToString(obj%varType))
#endif
END SELECT

bool1 = ALLOCATED(obj%val)
CALL Display(bool1, 'val ALLOCATED: ', unitno=unitno)
CALL Display(SafeSize(obj%val), "Size of val:", unitno=unitno)

bool1 = ALLOCATED(obj%indxVal)
CALL Display(bool1, 'indxVal ALLOCATED: ', unitno=unitno)
CALL Display(SafeSize(obj%indxVal), "Size of indxVal:", unitno=unitno)

bool1 = ASSOCIATED(obj%mesh)
CALL Display(bool1, 'mesh ASSOCIATED: ', unitno=unitno)

CALL Display(obj%totalShape, 'totalShape: ', unitno=unitno)
CALL Display(obj%ss, 'ss:', unitno=unitno)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

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

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                                                     Export
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Export
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Export()"
#endif

TYPE(String) :: strval, dsetname
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = obj%isInit
CALL AssertError1(isok, myName, &
                  'MeshField object is not initiated')
#endif

#ifdef DEBUG_VER
isok = hdf5%IsOpen()
CALL AssertError1(isok, myName, &
                  'HDF5 file is not opened')
#endif

#ifdef DEBUG_VER
isok = hdf5%IsWrite()
CALL AssertError1(isok, myName, &
                  'HDF5 file does not have write permission')
#endif

dsetname = TRIM(group)//"/fieldType"
strval = typefield%ToString(obj%fieldType)
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)

dsetname = TRIM(group)//"/name"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%name)

dsetname = TRIM(group)//"/engine"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%engine)

dsetname = TRIM(group)//"/tSize"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%tSize)

dsetname = TRIM(group)//"/defineOn"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%defineOn)

dsetname = TRIM(group)//"/rank"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%rank)

dsetname = TRIM(group)//"/varType"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%varType)

isok = ALLOCATED(obj%val)
IF (isok) THEN
  dsetname = TRIM(group)//"/val"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%val)
END IF

isok = ALLOCATED(obj%indxVal)
IF (isok) THEN
  dsetname = TRIM(group)//"/indxVal"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%indxVal)
END IF

dsetname = TRIM(group)//"/totalShape"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%totalShape)

isok = ALLOCATED(obj%ss)
IF (isok) THEN
  dsetname = TRIM(group)//"/shape"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%ss)
END IF

isok = ALLOCATED(obj%indxShape)
IF (isok) THEN
  dsetname = TRIM(group)//"/indxShape"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%indxShape)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ExportInVTK
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ExportInVTK()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  'child classes')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ExportInVTK

!----------------------------------------------------------------------------
!                                                               Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE IOMethods
