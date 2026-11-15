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

SUBMODULE(AbstractMeshField_Class) HDFMethods
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "AbstractMeshField_Class@HDFMethods"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                     Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
#endif

TYPE(String) :: strval, dsetname
LOGICAL(LGT) :: isok
INTEGER(I4B), ALLOCATABLE :: intvec(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = hdf5%IsOpen()
CALL AssertError1(isok, myName, &
                  'HDF5 file is not opened')
#endif

#ifdef DEBUG_VER
isok = hdf5%IsRead()
CALL AssertError1(isok, myName, &
                  'HDF5 file does not have read permission')
#endif

CALL obj%DEALLOCATE()

! BoolR0
! isInit
dsetname = TRIM(group)//"/BoolR0/isInit"
isok = hdf5%PathExists(dsetname%chars())
IF (isok) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%isInit)
ELSE
  obj%isInit = .TRUE.
END IF

! IntR0
! fieldType, it is stored as string, see obj_Export
dsetname = TRIM(group)//"/IntR0/fieldType"
#ifdef DEBUG_VER
isok = hdf5%PathExists(dsetname%chars())
CALL AssertError1(isok, myName, &
                  'dataset '//dsetname%chars()//' not found in hdf5')
#endif
CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
obj%fieldType = typefield%ToNumber(strval%chars())

! tSize
dsetname = TRIM(group)//"/IntR0/tSize"
#ifdef DEBUG_VER
isok = hdf5%PathExists(dsetname%chars())
CALL AssertError1(isok, myName, &
                  'dataset '//dsetname%chars()//' not found in hdf5')
#endif
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%tSize)

! defineOn
dsetname = TRIM(group)//"/IntR0/defineOn"
#ifdef DEBUG_VER
isok = hdf5%PathExists(dsetname%chars())
CALL AssertError1(isok, myName, &
                  'dataset '//dsetname%chars()//' not found in hdf5')
#endif
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%defineOn)

! rank
dsetname = TRIM(group)//"/IntR0/rank"
#ifdef DEBUG_VER
isok = hdf5%PathExists(dsetname%chars())
CALL AssertError1(isok, myName, &
                  'dataset '//dsetname%chars()//' not found in hdf5')
#endif
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%rank)

! varType
dsetname = TRIM(group)//"/IntR0/varType"
#ifdef DEBUG_VER
isok = hdf5%PathExists(dsetname%chars())
CALL AssertError1(isok, myName, &
                  'dataset '//dsetname%chars()//' not found in hdf5')
#endif
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%varType)

! totalShape
dsetname = TRIM(group)//"/IntR0/totalShape"
#ifdef DEBUG_VER
isok = hdf5%PathExists(dsetname%chars())
CALL AssertError1(isok, myName, &
                  'dataset '//dsetname%chars()//' not found in hdf5')
#endif
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%totalShape)

#ifdef DEBUG_VER
CALL AssertError3(obj%totalShape, fevaropt%maxRank, myName, &
                  "a=obj%totalShape, b=fevaropt%maxRank")
#endif

! StringR0
! name
dsetname = TRIM(group)//"/StringR0/name"
#ifdef DEBUG_VER
isok = hdf5%PathExists(dsetname%chars())
CALL AssertError1(isok, myName, &
                  'dataset '//dsetname%chars()//' not found in hdf5')
#endif
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%name)

! engine
dsetname = TRIM(group)//"/StringR0/engine"
#ifdef DEBUG_VER
isok = hdf5%PathExists(dsetname%chars())
CALL AssertError1(isok, myName, &
                  'dataset '//dsetname%chars()//' not found in hdf5')
#endif
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%engine)

! IntR1
! maxShape, only maxShape(1:totalShape) is stored
dsetname = TRIM(group)//"/IntR1/maxShape"
#ifdef DEBUG_VER
isok = hdf5%PathExists(dsetname%chars())
CALL AssertError1(isok, myName, &
                  'dataset '//dsetname%chars()//' not found in hdf5')
#endif
CALL hdf5%READ(dsetname=dsetname%chars(), vals=intvec)

#ifdef DEBUG_VER
CALL AssertError2(SIZE(intvec), obj%totalShape, myName, &
                  "a=SIZE(maxShape), b=obj%totalShape")
#endif

obj%maxShape = 0
obj%maxShape(1:obj%totalShape) = intvec(1:obj%totalShape)

! ss, indxShape, indxVal, and val are stored only if allocated
dsetname = TRIM(group)//"/IntR1/ss"
isok = hdf5%PathExists(dsetname%chars())
IF (isok) CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%ss)

dsetname = TRIM(group)//"/IntR1/indxShape"
isok = hdf5%PathExists(dsetname%chars())
IF (isok) CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%indxShape)

dsetname = TRIM(group)//"/IntR1/indxVal"
isok = hdf5%PathExists(dsetname%chars())
IF (isok) CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%indxVal)

! RealR1
dsetname = TRIM(group)//"/RealR1/val"
isok = hdf5%PathExists(dsetname%chars())
IF (isok) CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%val)

! mesh is not stored in the hdf5 file
IF (PRESENT(mesh)) obj%mesh => mesh

#ifdef DEBUG_VER
IF (PRESENT(mesh) .AND. obj%fieldType .NE. typefield%constant) THEN
  CALL AssertError2(obj%tSize, mesh%GetTotalElements(), myName, &
                    "a=obj%tSize, b=mesh%GetTotalElements()")
END IF
#endif

IF (ALLOCATED(intvec)) DEALLOCATE (intvec)

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

dsetname = TRIM(group)//"/BoolR0/isInit"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%isInit)

dsetname = TRIM(group)//"/IntR0/fieldType"
strval = typefield%ToString(obj%fieldType)
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)

dsetname = TRIM(group)//"/StringR0/name"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%name)

dsetname = TRIM(group)//"/StringR0/engine"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%engine)

dsetname = TRIM(group)//"/IntR0/tSize"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%tSize)

dsetname = TRIM(group)//"/IntR0/defineOn"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%defineOn)

dsetname = TRIM(group)//"/IntR0/rank"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%rank)

dsetname = TRIM(group)//"/IntR0/varType"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%varType)

dsetname = TRIM(group)//"/IntR0/totalShape"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%totalShape)

dsetname = TRIM(group)//"/IntR1/maxShape"
CALL hdf5%WRITE(dsetname=dsetname%chars(), &
                vals=obj%maxShape(1:obj%totalShape))

isok = ALLOCATED(obj%ss)
IF (isok) THEN
  dsetname = TRIM(group)//"/IntR1/ss"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%ss)
END IF

isok = ALLOCATED(obj%indxShape)
IF (isok) THEN
  dsetname = TRIM(group)//"/IntR1/indxShape"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%indxShape)
END IF

isok = ALLOCATED(obj%indxVal)
IF (isok) THEN
  dsetname = TRIM(group)//"/IntR1/indxVal"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%indxVal)
END IF

isok = ALLOCATED(obj%val)
IF (isok) THEN
  dsetname = TRIM(group)//"/RealR1/val"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%val)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!                                                               Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE HDFMethods
