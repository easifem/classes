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

SUBMODULE(AbstractField_Class) IOMethods
USE Display_Method, ONLY: Display, ToString
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
INTEGER(I4B) :: ii
LOGICAL(LGT) :: isok

CALL Display(msg, unitNo=unitNo)

IF (obj%isInitiated) THEN
  CALL Display("isInitiated : TRUE", unitNo=unitNo)
ELSE
  CALL Display("isInitiated : FALSE, Nothing to Display!", unitNo=unitNo)
  RETURN
END IF

CALL Display(obj%name%chars(), msg="name : ", unitNo=unitNo)

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL Display("fieldType : CONSTANT", unitNo=unitNo)
ELSE
  CALL Display("fieldType : NORMAL", unitNo=unitNo)
END IF

CALL Display(obj%engine%chars(), msg='engine : ', unitNo=unitNo)
CALL Display(obj%comm, msg='comm: ', unitNo=unitNo)
CALL Display(obj%myRank, msg='myRank: ', unitNo=unitNo)
CALL Display(obj%numProcs, msg='numProcs: ', unitNo=unitNo)
CALL Display(obj%global_n, msg='global_n: ', unitNo=unitNo)
CALL Display(obj%local_n, msg='local_n: ', unitNo=unitNo)
CALL Display(obj%is, msg='is: ', unitNo=unitNo)
CALL Display(obj%ie, msg='ie: ', unitNo=unitNo)
CALL Display(obj%lis_ptr, msg='lis_ptr: ', unitNo=unitNo)

isok = ASSOCIATED(obj%fedof)
CALL Display(isok, "fedof ASSOCIATED: ", unitNo=unitNo)

IF (ALLOCATED(obj%fedofs)) THEN
  CALL Display("fedofs : ALLOCATED [" &
               //TOSTRING(SIZE(obj%fedofs)) &
               //"]", unitNo=unitNo)
  DO ii = 1, SIZE(obj%fedofs)

    isok = ASSOCIATED(obj%fedofs(ii)%ptr)
    CALL Display(isok, "fedofs("//TOSTRING(ii) &
                 //")%ptr ASSOCIATED: ", unitNo=unitNo)

  END DO

ELSE
  CALL Display("fedofs : NOT ALLOCATED", unitNo=unitNo)
END IF

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteData_hdf5
CHARACTER(*), PARAMETER :: myName = "obj_WriteData_hdf5()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
            '[IMPLEMENTATION ERROR] :: This method should be implemented '// &
                  'by children of AbstractField_')
END PROCEDURE obj_WriteData_hdf5

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteData_vtk
CHARACTER(*), PARAMETER :: myName = "obj_WriteData_vtk()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
          '[IMPLEMENTATION ERROR] :: This method should be implemented by'// &
                  ' children of AbstractField_')
END PROCEDURE obj_WriteData_vtk

!----------------------------------------------------------------------------
!                                                                    Export
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Export
CHARACTER(*), PARAMETER :: myName = "obj_Export()"
TYPE(String) :: dname

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
              '[INTERNAL ERROR] :: Instnace of MatrixField_ is not initiated')
END IF

! Check
IF (.NOT. hdf5%isOpen()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: HDF5 file is not opened')
END IF

! Check
IF (.NOT. hdf5%isWrite()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
               '[INTERNAL ERROR] :: HDF5 file does not have write permission')
END IF

! fieldType
dname = TRIM(group)//"/fieldType"
CALL hdf5%WRITE(dsetname=dname%chars(), &
                vals=STRING(FIELD_TYPE_NAME(obj%fieldType)))

! name
dname = TRIM(group)//"/name"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%name)

! engine
dname = TRIM(group)//"/engine"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%engine)

! comm
dname = TRIM(group)//"/comm"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%comm)

! myRank
dname = TRIM(group)//"/myRank"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%myRank)

! numProcs
dname = TRIM(group)//"/numProcs"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%numProcs)

! local_n
dname = TRIM(group)//"/local_n"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%local_n)

! global_n
dname = TRIM(group)//"/global_n"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%global_n)

! is
dname = TRIM(group)//"/is"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%is)

! ie
dname = TRIM(group)//"/ie"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%ie)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!                                                             obj_Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
TYPE(String) :: strval, dsetname
INTEGER(I4B) :: tsize, ii
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! main program
IF (obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    '[INTERNAL ERROR] :: The instance of AbstractField_ is already initiated')
  RETURN
END IF

! Check
IF (.NOT. hdf5%isOpen()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: HDF5 file is not opened')
  RETURN
END IF

! Check
IF (.NOT. hdf5%isRead()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                '[INTERNAL ERROR] :: HDF5 file does not have read permission')
  RETURN
END IF

! fieldType
dsetname = TRIM(group)//"/fieldType"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
  obj%fieldType = FIELD_TYPE_NUMBER(strval%chars())
ELSE
  obj%fieldType = FIELD_TYPE_NORMAL
END IF

! name
dsetname = TRIM(group)//"/name"
isok = hdf5%pathExists(dsetname%chars())
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: The dataset name should be present')
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%name)
END IF

! engine
dsetname = TRIM(group)//"/engine"
isok = hdf5%pathExists(dsetname%chars())
IF (.NOT. isok) THEN
  obj%engine = "NATIVE_SERIAL"
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%engine)
END IF

! comm
dsetname = TRIM(group)//"/comm"
isok = hdf5%pathExists(dsetname%chars())
IF (.NOT. isok) THEN
  obj%comm = 0
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%comm)
END IF

! myRank
dsetname = TRIM(group)//"/myRank"
isok = hdf5%pathExists(dsetname%chars())
IF (.NOT. isok) THEN
  obj%myRank = 0
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%myRank)
END IF

! numProcs
dsetname = TRIM(group)//"/numProcs"
isok = hdf5%pathExists(dsetname%chars())
IF (.NOT. isok) THEN
  obj%numProcs = 1
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%numProcs)
END IF

! global_n
dsetname = TRIM(group)//"/global_n"
isok = hdf5%pathExists(dsetname%chars())
IF (.NOT. isok) THEN
  obj%global_n = 1
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%global_n)
END IF

! local_n
dsetname = TRIM(group)//"/local_n"
isok = hdf5%pathExists(dsetname%chars())
IF (.NOT. isok) THEN
  obj%local_n = 1
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%local_n)
END IF

! is
dsetname = TRIM(group)//"/is"
isok = hdf5%pathExists(dsetname%chars())
IF (.NOT. isok) THEN
  obj%is = 1
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%is)
END IF

! ie
dsetname = TRIM(group)//"/ie"
isok = hdf5%pathExists(dsetname%chars())
IF (.NOT. isok) THEN
  obj%ie = 1
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%ie)
END IF

IF (ASSOCIATED(obj%fedof)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
              '[INTERNAL ERROR] :: obj%fedof is associated, deallocate first')
  RETURN
END IF

IF (ALLOCATED(obj%fedofs)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
              '[INTERNAL ERROR] :: obj%fedofs is allocated, deallocate first')
  RETURN
END IF

IF (PRESENT(fedof)) THEN
  obj%fedof => fedof

ELSE IF (PRESENT(fedofs)) THEN

  tsize = SIZE(fedofs)

  ALLOCATE (obj%fedofs(tsize))

  DO ii = 1, tsize
    obj%fedofs(ii)%ptr => fedofs(ii)%ptr
  END DO

ELSE

  CALL e%RaiseError(modName//'::'//myName//" - "// &
    "[INTERNAL ERROR] :: For non-rectangle matrix dom should be present, "// &
                    "for rectangle matrix matrix fedofs should be present")
  RETURN

END IF

obj%isInitiated = .TRUE.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Import

END SUBMODULE IOMethods
