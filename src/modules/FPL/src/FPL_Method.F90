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
! summary: This module contains extra method for Fortran Parameter Lists

MODULE FPL_Method
USE GlobalData
USE BaseType
USE BaseMethod
USE FPL, ONLY: ParameterList_
USE ExceptionHandler_Class, ONLY: e
PRIVATE
CHARACTER(*), PARAMETER :: modName_ = "FPL_Method"
!! TYPE(ExceptionHandler_) :: e
PUBLIC :: Set
PUBLIC :: GetValue
PUBLIC :: CheckEssentialParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE Set
  MODULE PROCEDURE fpl_Set1
  MODULE PROCEDURE fpl_Set_Int
  MODULE PROCEDURE fpl_Set_Int_R1
  MODULE PROCEDURE fpl_Set_Real
  MODULE PROCEDURE fpl_Set_Real_R1
  MODULE PROCEDURE fpl_Set_Real_R2
  MODULE PROCEDURE fpl_Set_String
  MODULE PROCEDURE fpl_Set_Char
  MODULE PROCEDURE fpl_Set_Bool
END INTERFACE Set

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE GetValue
  MODULE PROCEDURE fpl_GetValue1
  MODULE PROCEDURE fpl_GetValue2
  MODULE PROCEDURE fpl_Get_Int
  MODULE PROCEDURE fpl_Get_Int_R1
  MODULE PROCEDURE fpl_Get_Int_IntVec
  MODULE PROCEDURE fpl_Get_Real
  MODULE PROCEDURE fpl_Get_Real_R1
  MODULE PROCEDURE fpl_Get_Real_RealVec
  MODULE PROCEDURE fpl_Get_Real_R2
  MODULE PROCEDURE fpl_Get_Real_RealMatrix
  MODULE PROCEDURE fpl_Get_Bool
  MODULE PROCEDURE fpl_Get_Bool_R1
  MODULE PROCEDURE fpl_Get_String
  MODULE PROCEDURE fpl_Get_Char
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                     CheckEssentialParam
!----------------------------------------------------------------------------

INTERFACE CheckEssentialParam
  MODULE PROCEDURE fpl_CheckEssentialParam_1, fpl_CheckEssentialParam_2
END INTERFACE CheckEssentialParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                     CheckEssentialParam
!----------------------------------------------------------------------------

SUBROUTINE fpl_CheckEssentialParam_1(obj, keys, prefix, myName, modName)
  ! Define dummy variables
  TYPE(ParameterList_), INTENT(IN) :: obj
  TYPE(String), INTENT(IN) :: keys(:)
  !! String keys to be check in obj
  CHARACTER(*), INTENT(IN) :: prefix
  !! Prefix
  CHARACTER(*), INTENT(IN) :: myName
  !! myName
  CHARACTER(*), INTENT(IN) :: modName
  ! internal variables

  INTEGER(I4B) :: ii
  LOGICAL(LGT) :: abool

  DO ii = 1, SIZE(keys)
    abool = obj%isPresent(key=prefix//"/"//keys(ii))
    IF (.NOT. abool) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
        & prefix//"/"//keys(ii)//' should be present in param. '//  &
        & "Error in "//tostring(ii)//"th parameter. "//  &
        & "This routine is called from fpl_CheckEssentialParam in "//  &
        & modName_//" module in the file "//__FILE__)
    END IF
  END DO
END SUBROUTINE fpl_CheckEssentialParam_1

!----------------------------------------------------------------------------
!                                                     CheckEssentialParam
!----------------------------------------------------------------------------

SUBROUTINE fpl_CheckEssentialParam_2(obj, keys, prefix, myName, modName)
  ! Define dummy variables
  TYPE(ParameterList_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: keys
  !! String keys to be check in obj
  CHARACTER(*), INTENT(IN) :: prefix
  !! Prefix
  CHARACTER(*), INTENT(IN) :: myName
  !! myName
  CHARACTER(*), INTENT(IN) :: modName
  ! internal variables

  ! internal error
  INTEGER(I4B) :: ii
  TYPE(String) :: astr
  TYPE(String), ALLOCATABLE :: essentialParam(:)

  astr = keys
  CALL astr%Split(essentialParam, sep="/")
  CALL CheckEssentialParam(obj=obj, keys=essentialParam,  &
    & prefix=prefix, myName=myName, modName=modName)

  IF (ALLOCATED(essentialParam)) THEN
    DO ii = 1, SIZE(essentialParam)
      essentialParam(ii) = ""
    END DO
    DEALLOCATE (essentialParam)
  END IF
  astr = ""
END SUBROUTINE fpl_CheckEssentialParam_2

!----------------------------------------------------------------------------
!                                                                 fpl_Set1
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Set the parameters for DOF object

SUBROUTINE fpl_Set1(obj, key, VALUE)
  ! Define dummy variables
  TYPE(ParameterList_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: key
  TYPE(DOF_), INTENT(IN) :: VALUE
  ! Internal variable
  INTEGER(I4B) :: ierr
  ierr = obj%Set(key=TRIM(key)//"/map", VALUE=VALUE%map)
  ierr = obj%Set(key=TRIM(key)//"/valmap", VALUE=VALUE%valmap)
  ierr = obj%Set(key=TRIM(key)//"/storageFMT", VALUE=VALUE%storageFMT)
END SUBROUTINE fpl_Set1

!----------------------------------------------------------------------------
!                                                                 fpl_Set1
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Set the integer scalar parameter
!
!TODO: Implement fpl_Set for Int8, Int16, Int32, Int64

SUBROUTINE fpl_Set_Int(obj, datatype, prefix, key, VALUE)
  ! Define dummy variables
  TYPE(ParameterList_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: prefix
  CHARACTER(*), INTENT(IN) :: key
  INTEGER(I4B), INTENT(IN) :: datatype
  !! This argument is only to create unique interface
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: VALUE
  ! Internal variable
  INTEGER(I4B) :: ierr
  IF (PRESENT(VALUE)) THEN
    ierr = obj%Set(key=TRIM(prefix)//"/"//TRIM(key), VALUE=VALUE)
  END IF
END SUBROUTINE fpl_Set_Int

!----------------------------------------------------------------------------
!                                                                 fpl_Set1
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Set the integer scalar parameter
!
!TODO: Implement fpl_Set for Int8, Int16, Int32, Int64

SUBROUTINE fpl_Set_Int_R1(obj, datatype, prefix, key, VALUE)
  ! Define dummy variables
  TYPE(ParameterList_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: prefix
  CHARACTER(*), INTENT(IN) :: key
  INTEGER(I4B), INTENT(IN) :: datatype(1)
  !! This argument is only to create unique interface
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: VALUE(:)
  ! Internal variable
  INTEGER(I4B) :: ierr
  IF (PRESENT(VALUE)) THEN
    ierr = obj%Set(key=TRIM(prefix)//"/"//TRIM(key), VALUE=VALUE)
  END IF
END SUBROUTINE fpl_Set_Int_R1

!----------------------------------------------------------------------------
!                                                                 fpl_Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Set the real scalar parameter
!
!TODO: Implement fpl_Set for Real32 and Real64
SUBROUTINE fpl_Set_Real(obj, datatype, prefix, key, VALUE)
  ! Define dummy variables
  TYPE(ParameterList_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: prefix
  CHARACTER(*), INTENT(IN) :: key
  REAL(DFP), INTENT(IN) :: datatype
  !! This argument is only to create unique interface
  REAL(DFP), OPTIONAL, INTENT(IN) :: VALUE
  ! Internal variable
  INTEGER(I4B) :: ierr
  IF (PRESENT(VALUE)) THEN
    ierr = obj%Set(key=TRIM(prefix)//"/"//TRIM(key), VALUE=VALUE)
  END IF
END SUBROUTINE fpl_Set_Real

!----------------------------------------------------------------------------
!                                                                 fpl_Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Set the real vector parameter
!
!TODO: Implement fpl_Set for Real32 and Real64
SUBROUTINE fpl_Set_Real_R1(obj, datatype, prefix, key, VALUE)
  ! Define dummy variables
  TYPE(ParameterList_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: prefix
  CHARACTER(*), INTENT(IN) :: key
  REAL(DFP), INTENT(IN) :: datatype(1)
  !! This argument is only to create unique interface
  REAL(DFP), OPTIONAL, INTENT(IN) :: VALUE(:)
  ! Internal variable
  INTEGER(I4B) :: ierr
  IF (PRESENT(VALUE)) THEN
    ierr = obj%Set(key=TRIM(prefix)//"/"//TRIM(key), VALUE=VALUE)
  END IF
END SUBROUTINE fpl_Set_Real_R1

!----------------------------------------------------------------------------
!                                                                 fpl_Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Set the real vector parameter
!
!TODO: Implement fpl_Set for Real32 and Real64
SUBROUTINE fpl_Set_Real_R2(obj, datatype, prefix, key, VALUE)
  ! Define dummy variables
  TYPE(ParameterList_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: prefix
  CHARACTER(*), INTENT(IN) :: key
  REAL(DFP), INTENT(IN) :: datatype(1, 1)
  !! This argument is only to create unique interface
  REAL(DFP), OPTIONAL, INTENT(IN) :: VALUE(:, :)
  ! Internal variable
  INTEGER(I4B) :: ierr
  IF (PRESENT(VALUE)) THEN
    ierr = obj%Set(key=TRIM(prefix)//"/"//TRIM(key), VALUE=VALUE)
  END IF
END SUBROUTINE fpl_Set_Real_R2

!----------------------------------------------------------------------------
!                                                                 fpl_Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Set the real scalar parameter

SUBROUTINE fpl_Set_String(obj, datatype, prefix, key, VALUE)
  ! Define dummy variables
  TYPE(ParameterList_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: prefix
  CHARACTER(*), INTENT(IN) :: key
  TYPE(String), INTENT(IN) :: datatype
  TYPE(String), OPTIONAL, INTENT(IN) :: VALUE
  ! Internal variable
  INTEGER(I4B) :: ierr
  IF (PRESENT(VALUE)) THEN
    ierr = obj%Set(key=TRIM(prefix)//"/"//TRIM(key), VALUE=VALUE%chars())
  END IF
END SUBROUTINE fpl_Set_String

!----------------------------------------------------------------------------
!                                                                 fpl_Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Set the real scalar parameter

SUBROUTINE fpl_Set_Char(obj, datatype, prefix, key, VALUE)
  ! Define dummy variables
  TYPE(ParameterList_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: prefix
  CHARACTER(*), INTENT(IN) :: key
  CHARACTER(*), INTENT(IN) :: datatype
  CHARACTER(*), OPTIONAL, INTENT(IN) :: VALUE
  ! Internal variable
  INTEGER(I4B) :: ierr
  IF (PRESENT(VALUE)) THEN
    ierr = obj%Set(key=TRIM(prefix)//"/"//TRIM(key), VALUE=TRIM(VALUE))
  END IF
END SUBROUTINE fpl_Set_Char

!----------------------------------------------------------------------------
!                                                                 fpl_Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Set the real scalar parameter

SUBROUTINE fpl_Set_Bool(obj, datatype, prefix, key, VALUE)
  ! Define dummy variables
  TYPE(ParameterList_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: prefix
  CHARACTER(*), INTENT(IN) :: key
  LOGICAL(LGT), INTENT(IN) :: datatype
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: VALUE
  ! Internal variable
  INTEGER(I4B) :: ierr
  IF (PRESENT(VALUE)) THEN
    ierr = obj%Set(key=TRIM(prefix)//"/"//TRIM(key), VALUE=VALUE)
  END IF
END SUBROUTINE fpl_Set_Bool

!----------------------------------------------------------------------------
!                                                                 fpl_Get
!----------------------------------------------------------------------------

SUBROUTINE fpl_GetValue1(obj, key, VALUE)
  ! Define dummy variables
  TYPE(ParameterList_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: key
  TYPE(DOF_), INTENT(INOUT) :: VALUE
  ! Internal variable
  INTEGER(I4B) :: ierr
  INTEGER(I4B), ALLOCATABLE :: s(:)
  ierr = obj%GetShape(key=TRIM(key)//"/map", shape=s)
  CALL Reallocate(VALUE%map, s(1), s(2))
  ierr = obj%GetShape(key=TRIM(key)//"/valmap", shape=s)
  CALL Reallocate(VALUE%valmap, s(1))
  ierr = obj%Get(key=TRIM(key)//"/map", VALUE=VALUE%map)
  ierr = obj%Get(key=TRIM(key)//"/valmap", VALUE=VALUE%valmap)
  ierr = obj%Get(key=TRIM(key)//"/storageFMT", VALUE=VALUE%storageFMT)
  DEALLOCATE (s)
END SUBROUTINE fpl_GetValue1

!----------------------------------------------------------------------------
!                                                                  GetValue
!----------------------------------------------------------------------------

SUBROUTINE fpl_GetValue2(obj, key, VALUE)
  ! Define dummy variables
  TYPE(ParameterList_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: key
  TYPE(String), INTENT(INOUT) :: VALUE
  ! Internal variable
  CHARACTER(:), ALLOCATABLE :: char_var
  INTEGER(I4B) :: ierr
  CHARACTER(*), PARAMETER :: myName = "fpl_GetValue2()"

  IF (obj%isPresent(key=key)) THEN
    ALLOCATE (CHARACTER( &
      & obj%DataSizeInBytes(key=key)) :: char_var)
    ierr = obj%Get(key=key, VALUE=char_var)
    IF (ALLOCATED(char_var)) THEN
      VALUE = char_var
      DEALLOCATE (char_var)
    ELSE
      VALUE = ""
    END IF
  ELSE
    CALL e%raiseError(modName_//'::'//myName//" - "// &
      & key//' not found in obj')
  END IF
END SUBROUTINE fpl_GetValue2

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Get the integer scalar parameter
!
!TODO: Implement fpl_Set for Int8, Int16, Int32, Int64

SUBROUTINE fpl_Get_Int(obj, prefix, key, VALUE)
  ! Define dummy variables
  TYPE(ParameterList_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: prefix
  CHARACTER(*), INTENT(IN) :: key
  INTEGER(I4B), INTENT(OUT) :: VALUE
  ! Internal variable
  INTEGER(I4B) :: ierr
  TYPE(String) :: varname
  varname = TRIM(prefix)//"/"//TRIM(key)
  IF (obj%isPresent(key=varname%chars())) THEN
    ierr = obj%Get(key=varname%chars(), VALUE=VALUE)
  END IF
  varname = ""
END SUBROUTINE fpl_Get_Int

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Get the integer vector parameter
!
!TODO: Implement fpl_Set for Int8, Int16, Int32, Int64

SUBROUTINE fpl_Get_Int_R1(obj, prefix, key, VALUE)
  ! Define dummy variables
  TYPE(ParameterList_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: prefix
  CHARACTER(*), INTENT(IN) :: key
  INTEGER(I4B), INTENT(OUT) :: VALUE(:)
  ! Internal variable
  INTEGER(I4B) :: ierr
  TYPE(String) :: varname
  varname = TRIM(prefix)//"/"//TRIM(key)
  IF (obj%isPresent(key=varname%chars())) THEN
    ierr = obj%Get(key=varname%chars(), VALUE=VALUE)
  END IF
  varname = ""
END SUBROUTINE fpl_Get_Int_R1

!----------------------------------------------------------------------------
!                                                                  GetValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Get the real vector parameter

SUBROUTINE fpl_Get_Int_IntVec(obj, prefix, key, VALUE)
  ! Define dummy variables
  TYPE(ParameterList_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: prefix
  CHARACTER(*), INTENT(IN) :: key
  TYPE(IntVector_), INTENT(INOUT) :: VALUE

  ! internal variables
  INTEGER(I4B), ALLOCATABLE :: value_(:)
  INTEGER(I4B) :: tsize

  tsize = SIZE(VALUE)
  CALL Reallocate(value_, tsize)
  CALL GetValue(obj=obj, prefix=prefix, key=key, VALUE=value_)
  VALUE = IntVector(value_)
  DEALLOCATE (value_)
END SUBROUTINE fpl_Get_Int_IntVec

!----------------------------------------------------------------------------
!                                                                  GetValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Get the real scalar parameter
!
!TODO: Implement fpl_Set for Real32, Real64

SUBROUTINE fpl_Get_Real(obj, prefix, key, VALUE)
  ! Define dummy variables
  TYPE(ParameterList_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: prefix
  CHARACTER(*), INTENT(IN) :: key
  REAL(DFP), INTENT(OUT) :: VALUE
  ! Internal variable
  INTEGER(I4B) :: ierr
  TYPE(String) :: varname
  varname = TRIM(prefix)//"/"//TRIM(key)
  IF (obj%isPresent(key=varname%chars())) THEN
    ierr = obj%Get(key=varname%chars(), VALUE=VALUE)
  END IF
  varname = ""
END SUBROUTINE fpl_Get_Real

!----------------------------------------------------------------------------
!                                                                  GetValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Get the real vector parameter
!
!TODO: Implement fpl_Set for Real32, Real64

SUBROUTINE fpl_Get_Real_R1(obj, prefix, key, VALUE)
  ! Define dummy variables
  TYPE(ParameterList_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: prefix
  CHARACTER(*), INTENT(IN) :: key
  REAL(DFP), INTENT(OUT) :: VALUE(:)
  ! Internal variable
  INTEGER(I4B) :: ierr
  TYPE(String) :: varname
  varname = TRIM(prefix)//"/"//TRIM(key)
  IF (obj%isPresent(key=varname%chars())) THEN
    ierr = obj%Get(key=varname%chars(), VALUE=VALUE)
  END IF
  varname = ""
END SUBROUTINE fpl_Get_Real_R1

!----------------------------------------------------------------------------
!                                                                  GetValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Get the real vector parameter
!
!TODO: Implement fpl_Set for Real32, Real64

SUBROUTINE fpl_Get_Real_RealVec(obj, prefix, key, VALUE)
  ! Define dummy variables
  TYPE(ParameterList_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: prefix
  CHARACTER(*), INTENT(IN) :: key
  TYPE(RealVector_), INTENT(INOUT) :: VALUE

  ! internal variables
  REAL(DFP), ALLOCATABLE :: value_(:)
  INTEGER(I4B) :: tsize

  tsize = SIZE(VALUE)
  CALL Reallocate(value_, tsize)
  CALL GetValue(obj=obj, prefix=prefix, key=key, VALUE=value_)
  VALUE = RealVector(value_)
  DEALLOCATE (value_)
END SUBROUTINE fpl_Get_Real_RealVec

!----------------------------------------------------------------------------
!                                                                  GetValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Get the real vector parameter

SUBROUTINE fpl_Get_Real_R2(obj, prefix, key, VALUE)
  ! Define dummy variables
  TYPE(ParameterList_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: prefix
  CHARACTER(*), INTENT(IN) :: key
  REAL(DFP), INTENT(OUT) :: VALUE(:, :)
  ! Internal variable
  INTEGER(I4B) :: ierr
  TYPE(String) :: varname
  varname = TRIM(prefix)//"/"//TRIM(key)
  IF (obj%isPresent(key=varname%chars())) THEN
    ierr = obj%Get(key=varname%chars(), VALUE=VALUE)
  END IF
  varname = ""
END SUBROUTINE fpl_Get_Real_R2

!----------------------------------------------------------------------------
!                                                                  GetValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Get the real matrix parameter

SUBROUTINE fpl_Get_Real_RealMatrix(obj, prefix, key, VALUE)
  ! Define dummy variables
  TYPE(ParameterList_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: prefix
  CHARACTER(*), INTENT(IN) :: key
  TYPE(RealMatrix_), INTENT(INOUT) :: VALUE

  ! internal variables
  REAL(DFP), ALLOCATABLE :: value_(:, :)
  INTEGER(I4B) :: tsize(2)

  tsize = SHAPE(VALUE)
  CALL Reallocate(value_, tsize(1), tsize(2))
  CALL GetValue(obj=obj, prefix=prefix, key=key, VALUE=value_)
  CALL Convert(From=value_, To=VALUE)
  DEALLOCATE (value_)
END SUBROUTINE fpl_Get_Real_RealMatrix

!----------------------------------------------------------------------------
!                                                                  GetValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Get the boolean scalar parameter

SUBROUTINE fpl_Get_Bool(obj, prefix, key, VALUE)
  ! Define dummy variables
  TYPE(ParameterList_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: prefix
  CHARACTER(*), INTENT(IN) :: key
  LOGICAL(LGT), INTENT(OUT) :: VALUE
  ! Internal variable
  INTEGER(I4B) :: ierr
  TYPE(String) :: varname
  varname = TRIM(prefix)//"/"//TRIM(key)
  IF (obj%isPresent(key=varname%chars())) THEN
    ierr = obj%Get(key=varname%chars(), VALUE=VALUE)
  END IF
  varname = ""
END SUBROUTINE fpl_Get_Bool

!----------------------------------------------------------------------------
!                                                                  GetValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Get the boolean vector parameter

SUBROUTINE fpl_Get_Bool_R1(obj, prefix, key, VALUE)
  ! Define dummy variables
  TYPE(ParameterList_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: prefix
  CHARACTER(*), INTENT(IN) :: key
  LOGICAL(LGT), INTENT(OUT) :: VALUE(:)
  ! Internal variable
  INTEGER(I4B) :: ierr
  TYPE(String) :: varname
  varname = TRIM(prefix)//"/"//TRIM(key)
  IF (obj%isPresent(key=varname%chars())) THEN
    ierr = obj%Get(key=varname%chars(), VALUE=VALUE)
  END IF
  varname = ""
END SUBROUTINE fpl_Get_Bool_R1

!----------------------------------------------------------------------------
!                                                                  GetValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Get the string scalar parameter

SUBROUTINE fpl_Get_String(obj, prefix, key, VALUE)
  ! Define dummy variables
  TYPE(ParameterList_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: prefix
  CHARACTER(*), INTENT(IN) :: key
  TYPE(String), INTENT(OUT) :: VALUE
  ! Internal variable
  CHARACTER(:), ALLOCATABLE :: char_var
  INTEGER(I4B) :: ierr
  TYPE(String) :: varname
  varname = TRIM(prefix)//"/"//TRIM(key)

  IF (obj%isPresent(key=varname%chars())) THEN
    ALLOCATE (CHARACTER( &
      & obj%DataSizeInBytes(key=varname%chars())) :: char_var)
    ierr = obj%Get(key=varname%chars(), VALUE=char_var)
    IF (ALLOCATED(char_var)) THEN
      VALUE = char_var
      DEALLOCATE (char_var)
    ELSE
      VALUE = ""
    END IF
  END IF
  varname = ""
END SUBROUTINE fpl_Get_String

!----------------------------------------------------------------------------
!                                                                  GetValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Get the fortran string scalar parameter

SUBROUTINE fpl_Get_Char(obj, prefix, key, VALUE)
  ! Define dummy variables
  TYPE(ParameterList_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: prefix
  CHARACTER(*), INTENT(IN) :: key
  CHARACTER(*), INTENT(OUT) :: VALUE
  ! Internal variable
  CHARACTER(:), ALLOCATABLE :: char_var
  INTEGER(I4B) :: ierr
  TYPE(String) :: varname
  varname = TRIM(prefix)//"/"//TRIM(key)

  IF (obj%isPresent(key=varname%chars())) THEN
    ALLOCATE (CHARACTER( &
      & obj%DataSizeInBytes(key=varname%chars())) :: char_var)
    ierr = obj%Get(key=varname%chars(), VALUE=char_var)
    IF (ALLOCATED(char_var)) THEN
      VALUE = char_var
      DEALLOCATE (char_var)
    ELSE
      VALUE = ""
    END IF
  END IF
  varname = ""
END SUBROUTINE fpl_Get_Char

END MODULE FPL_Method
