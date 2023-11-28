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
! date: 27 Aug 2021
! summary: Abstract class for Material Model

MODULE AbstractMaterialModel_Class
USE GlobalData
USE BaseType
USE String_Class
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Class
USE FPL, ONLY: ParameterList_
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "AbstractMaterialModel_Class"
PUBLIC :: AbstractMaterialModel_
PUBLIC :: AbstractMaterialModelPointer_
PUBLIC :: AbstractMaterialModelDeallocate

!----------------------------------------------------------------------------
!                                                   AbstractMaterialModel_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Oct 2021
! summary: abstract class for modelling the behaviour of materials

TYPE, ABSTRACT :: AbstractMaterialModel_
  LOGICAL(LGT) :: isInit = .FALSE.
  TYPE(String) :: name
CONTAINS
  PRIVATE
  PROCEDURE(amb_CheckEssentialParam), DEFERRED, PUBLIC, PASS(obj) :: &
    & CheckEssentialParam
  PROCEDURE(amb_Initiate), DEFERRED, PUBLIC, PASS(obj) :: Initiate
  PROCEDURE(amb_Deallocate), DEFERRED, PUBLIC, PASS(obj) :: &
    & DEALLOCATE
  PROCEDURE(amb_Import), DEFERRED, PUBLIC, PASS(obj) :: IMPORT
  PROCEDURE(amb_Export), DEFERRED, PUBLIC, PASS(obj) :: Export
  PROCEDURE(amb_Display), DEFERRED, PUBLIC, PASS(obj) :: Display
  PROCEDURE(amb_GetPrefix), DEFERRED, PUBLIC, PASS(obj) :: GetPrefix
  PROCEDURE, PUBLIC, PASS(obj) :: isInitiated => amb_isInitiated
  PROCEDURE, PUBLIC, PASS(obj) :: GetName => amb_GetName
  PROCEDURE, PUBLIC, PASS(obj) :: SetIsInitiated => amb_SetIsInitiated
  PROCEDURE, PUBLIC, PASS(obj) :: SetName => amb_SetName
END TYPE AbstractMaterialModel_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: AbstractMaterialModelPointer_
  CLASS(AbstractMaterialModel_), POINTER :: ptr => NULL()
END TYPE AbstractMaterialModelPointer_

!----------------------------------------------------------------------------
!                                                       CheckEssentialParam
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE amb_CheckEssentialParam(obj, param)
    IMPORT :: AbstractMaterialModel_, ParameterList_
    CLASS(AbstractMaterialModel_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE amb_CheckEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Initiate
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE amb_Initiate(obj, param)
    IMPORT :: AbstractMaterialModel_, ParameterList_
    CLASS(AbstractMaterialModel_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE amb_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE amb_Deallocate(obj)
    IMPORT :: AbstractMaterialModel_
    CLASS(AbstractMaterialModel_), INTENT(INOUT) :: obj
  END SUBROUTINE amb_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    Import
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE amb_Import(obj, hdf5, group)
    IMPORT :: AbstractMaterialModel_, HDF5File_
    CLASS(AbstractMaterialModel_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE amb_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    Export
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE amb_Export(obj, hdf5, group)
    IMPORT :: AbstractMaterialModel_, HDF5File_
    CLASS(AbstractMaterialModel_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE amb_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE amb_Display(obj, msg, unitNo)
    IMPORT :: AbstractMaterialModel_, I4B
    CLASS(AbstractMaterialModel_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE amb_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 GetPrefix
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  FUNCTION amb_GetPrefix(obj) RESULT(ans)
    IMPORT :: AbstractMaterialModel_
    CLASS(AbstractMaterialModel_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION amb_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Contains
!----------------------------------------------------------------------------

CONTAINS

SUBROUTINE AbstractMaterialModelDeallocate(obj)
  CLASS(AbstractMaterialModel_), INTENT(INOUT) :: obj
  obj%name = ""
  obj%isInit = .FALSE.
END SUBROUTINE AbstractMaterialModelDeallocate

!----------------------------------------------------------------------------
!                                                                 GetName
!----------------------------------------------------------------------------

FUNCTION amb_GetName(obj) RESULT(ans)
  CLASS(AbstractMaterialModel_), INTENT(IN) :: obj
  CHARACTER(:), ALLOCATABLE :: ans
  ans = ""
  ans = obj%name%chars()
END FUNCTION amb_GetName

!----------------------------------------------------------------------------
!                                                           isInitiated
!----------------------------------------------------------------------------

FUNCTION amb_isInitiated(obj) RESULT(ans)
  CLASS(AbstractMaterialModel_), INTENT(IN) :: obj
  LOGICAL(LGT) :: ans
  ans = obj%isInit
END FUNCTION amb_isInitiated

!----------------------------------------------------------------------------
!                                                           isInitiated
!----------------------------------------------------------------------------

SUBROUTINE amb_SetIsInitiated(obj, VALUE)
  CLASS(AbstractMaterialModel_), INTENT(INOUT) :: obj
  LOGICAL(LGT), INTENT(IN) :: VALUE
  obj%isInit = VALUE
END SUBROUTINE amb_SetIsInitiated

!----------------------------------------------------------------------------
!                                                           SetName
!----------------------------------------------------------------------------

SUBROUTINE amb_SetName(obj, VALUE)
  CLASS(AbstractMaterialModel_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: VALUE
  obj%name = VALUE
END SUBROUTINE amb_SetName

END MODULE AbstractMaterialModel_Class
