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

SUBMODULE(UserFunction_Class) HDFMethods
USE Display_Method, ONLY: Display
USE BaseType, ONLY: varopt => TypeFEVariableOpt
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
TYPE(String) :: dsetname, strval
REAL(DFP), ALLOCATABLE :: vectorValue(:), matrixValue(:, :)
INTEGER(I4B) :: tsize, nrow, ncol

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//"::"//myName//" - "// &
                        "[START]")
#endif

IF (.NOT. hdf5%isOpen()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: HDF5 file is not opened')
END IF

IF (.NOT. hdf5%isRead()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                '[INTERNAL ERROR] :: HDF5 file does not have read permission')
END IF

!> name
dsetname = TRIM(group)//"/name"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%name)
ELSE
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[CONFIG ERROR] :: name should be present.')
END IF

!> isUserFunctionSet
dsetname = TRIM(group)//"/isExternalFunc"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%isExternalFunc)
ELSE
  obj%isExternalFunc = .FALSE.
END IF

!> isExternalFunc
IF (obj%isExternalFunc) THEN
  ! dsetname = TRIM(group)//"/userFunction"
  ! ALLOCATE (obj%userFunction)
  ! CALL obj%userFunction%IMPORT(hdf5=hdf5, group=dsetname%chars())
  CALL e%RaiseError(modName//'::'//myName//' - '// &
            '[WIP ERROR] :: currently import does not work for external func')
END IF

IF (.NOT. obj%isExternalFunc) THEN
  !> returnType
  dsetname = TRIM(group)//"/returnType"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                  'dsetname '//dsetname%chars()//'is not present in HDFFile_')
  ELSE
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
    obj%returnType = UserFunctionGetReturnType(strval%chars())
  END IF

  !> argType
  dsetname = TRIM(group)//"/argType"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                  'dsetname '//dsetname%chars()//'is not present in HDFFile_')
  ELSE
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
    obj%argType = UserFunctionGetArgType(strval%chars())
  END IF

  !> check the argType, and decide the importer
  IF (obj%argType == varopt%constant) THEN

    !> scalarValue, vectorValue, matrixValue
    SELECT CASE (obj%returnType)

    CASE (varopt%scalar)
      dsetname = TRIM(group)//"/scalarValue"
      IF (hdf5%pathExists(dsetname%chars())) THEN
        CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%scalarValue)
      END IF

    CASE (varopt%vector)
      dsetname = TRIM(group)//"/vectorValue"
      IF (hdf5%pathExists(dsetname%chars())) THEN
        CALL hdf5%READ(dsetname=dsetname%chars(), vals=vectorValue)
        tsize = SIZE(vectorValue)
        obj%vectorValue(1:tsize) = vectorValue(1:tsize)
        DEALLOCATE (vectorValue)
      END IF

    CASE (varopt%matrix)
      dsetname = TRIM(group)//"/matrixValue"
      IF (hdf5%pathExists(dsetname%chars())) THEN
        CALL hdf5%READ(dsetname=dsetname%chars(), vals=matrixValue)
        nrow = SIZE(matrixValue, 1)
        ncol = SIZE(matrixValue, 2)
        obj%matrixValue(1:nrow, 1:ncol) = matrixValue(1:nrow, 1:ncol)
        DEALLOCATE (matrixValue)
      END IF

    CASE DEFAULT

    END SELECT

  ELSE

    CALL e%RaiseError(modName//'::'//myName//" - "// &
               'Currently, EASIFEM Supports import of constant userFunction.')

  END IF
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Export
CHARACTER(*), PARAMETER :: myName = "obj_Export"
TYPE(String) :: dsetname, strval

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] Export()')
#endif

!> check
IF (.NOT. hdf5%isOpen()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: HDF5 file is not opened')
END IF

!> check
IF (.NOT. hdf5%isWrite()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
               '[INTERNAL ERROR] :: HDF5 file does not have write permission')
END IF

!> name
dsetname = TRIM(group)//"/name"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%name)

!> isExternalFunc
IF (obj%isExternalFunc) THEN
  !> isUserFunctionSet
  ! dsetname = TRIM(group)//"/isUserFunctionSet"
  ! CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%isUserFunctionSet)
  ! !> returnType
  ! dsetname = TRIM(group)//"/userFunction"
  ! CALL obj%userFunction%Export(hdf5=hdf5, group=dsetname%chars())
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                 '[WIP ERROR] :: Currently export function does not work '// &
                    ' for UserFunction.')
END IF

IF (.NOT. obj%isExternalFunc) THEN
  !> isUserFunctionSet
  dsetname = TRIM(group)//"/isExternalFunc"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%isExternalFunc)

  !> returnType
  dsetname = TRIM(group)//"/returnType"
  strval = NAME_RETURN_TYPE(obj%returnType)
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)

  !> argType
  dsetname = TRIM(group)//"/argType"
  strval = NAME_ARG_TYPE(obj%argType)
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)
  !>
  IF (obj%argType == varopt%constant) THEN

    SELECT CASE (obj%returnType)

    CASE (varopt%Scalar)
      dsetname = TRIM(group)//"/scalarValue"
      CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%scalarValue)

    CASE (varopt%vector)
      dsetname = TRIM(group)//"/vectorValue"
      CALL hdf5%WRITE(dsetname=dsetname%chars(), &
                      vals=obj%vectorValue(1:obj%numReturns))

    CASE (varopt%matrix)
      dsetname = TRIM(group)//"/matrixValue"
      CALL hdf5%WRITE(dsetname=dsetname%chars(), &
             vals=obj%matrixValue(1:obj%returnShape(1), 1:obj%returnShape(2)))

    CASE DEFAULT
    END SELECT

  ELSE
    CALL e%RaiseError(modName//'::'//myName//" - "// &
               'Currently, EASIFEM Supports import of constant userFunction.')
  END IF

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE HDFMethods
