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

!> authors: Vikas Sharma, Ph. D.
! date: 2 Oct 2021
! summary: Data type of Newtonian fluid model
!
!# Introduction
!
! This module defines a class [[NewtonianFluidModel_]] to
! model the fluid behavior. In addition, it defines a subroutine
! [[SetNewtonianFluidModelParam]] to Set the option
! to construct an instance of [[NewtonianFluidModel_]].

MODULE NewtonianFluidModel_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE String_Class, ONLY: String
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Class, ONLY: HDF5File_
USE AbstractFluidMechanicsModel_Class, ONLY: AbstractFluidMechanicsModel_
USE tomlf, ONLY: toml_table

IMPLICIT NONE

PRIVATE

PUBLIC :: NewtonianFluidModel_
PUBLIC :: NewtonianFluidModelPointer_

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "NewtonianFluidModel_CLASS"
#endif

!----------------------------------------------------------------------------
!                                                       NewtonianFluidModel_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Oct 2021
! summary: A class for modeling Newtonian fluids
!
!# Introduction
!
! [[NewtonianFluidModel_]] class defines a Newtonian fluid.
!
! - `mu` represents the dynamicviscosity of the fluid.
! - dynamicViscosity $\mu$ of [[NewtonianFluidModel_]] is independent of the
! stress, and strain rate.
! - However, $\mu$ can depends upon the temperature, in case of
! non-isothermal fluid flow applications.
!
!@todo
! The dynamicviscosity of the fluid usually depends upon the temperature.
! Add this facility to the current class.
!@endtodo

TYPE, EXTENDS(AbstractFluidMechanicsModel_) :: NewtonianFluidModel_
  PRIVATE
  REAL(DFP) :: mu = 0.0_DFP
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  FINAL :: obj_FINAL

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display

  ! IO:
  ! @HDFMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export

  ! IO:
  ! @TomlMethods
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! Import from the toml

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetDynamicViscosity => &
    obj_GetDynamicViscosity
    !! Get the dynamic viscosity
  PROCEDURE, PUBLIC, PASS(obj) :: GetParam => obj_GetParam
  !! Get the parameters
  PROCEDURE, PUBLIC, PASS(obj) :: GetDataSize => obj_GetDataSize
  !! Get size of data
  PROCEDURE, PUBLIC, PASS(obj) :: GetData => obj_GetData
  !! Get the data

  ! SET:
  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: SetParam => obj_SetParam
  !! Set parameter
  PROCEDURE, PUBLIC, PASS(obj) :: SetData => obj_SetData
  !! Set data
  PROCEDURE, PUBLIC, PASS(obj) :: UpdateData => obj_UpdateData
  !! Get updated data
END TYPE NewtonianFluidModel_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: NewtonianFluidModelPointer_
  CLASS(NewtonianFluidModel_), POINTER :: ptr => NULL()
END TYPE NewtonianFluidModelPointer_

!----------------------------------------------------------------------------
!                                               Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Oct 2021
! summary: Deallocate data stored in the object

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(NewtonianFluidModel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Oct 2021
! summary: Deallocate data stored in the object

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(NewtonianFluidModel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@HDFMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Oct 2021
! summary: Initiate the Newtonian fluid model from hdf5 file

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group)
    CLASS(NewtonianFluidModel_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Oct 2021
! summary: Export the Newtonian fluid model to an hdf5 file

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(NewtonianFluidModel_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Oct 2021
! summary: Displays the content of Newtonian fluid model

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(NewtonianFluidModel_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetdynamicViscosity@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Oct 2021
! summary: Returns the dynamicviscosity

INTERFACE
  MODULE FUNCTION obj_GetDynamicViscosity(obj) RESULT(ans)
    CLASS(NewtonianFluidModel_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION obj_GetDynamicViscosity
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetParam@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-22
! summary:  Get Parameters

INTERFACE
  MODULE SUBROUTINE obj_GetParam(obj, dynamicViscosity)
    CLASS(NewtonianFluidModel_), INTENT(IN) :: obj
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: dynamicViscosity
  END SUBROUTINE obj_GetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetDataSize@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Get the size of data needed by obj

INTERFACE
  MODULE FUNCTION obj_GetDataSize(obj) RESULT(ans)
    CLASS(NewtonianFluidModel_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetDataSize
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-26
! summary: Get the  data from the model

INTERFACE
  MODULE SUBROUTINE obj_GetData(obj, DATA, tsize)
    CLASS(NewtonianFluidModel_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: DATA(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written in data
  END SUBROUTINE obj_GetData
END INTERFACE

!----------------------------------------------------------------------------
!                                                         SetParam@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-22
! summary:  Set Parameters

INTERFACE
  MODULE SUBROUTINE obj_SetParam(obj, dynamicViscosity)
    CLASS(NewtonianFluidModel_), INTENT(INOUT) :: obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: dynamicViscosity
  END SUBROUTINE obj_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetData@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-30
! summary: Set data

INTERFACE
  MODULE SUBROUTINE obj_SetData(obj, DATA)
    CLASS(NewtonianFluidModel_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: DATA(:)
  END SUBROUTINE obj_SetData
END INTERFACE

!----------------------------------------------------------------------------
!                                                       UpdateData@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_UpdateData(obj, DATA, tsize)
    CLASS(NewtonianFluidModel_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: DATA(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Size of data written in data
  END SUBROUTINE obj_UpdateData
END INTERFACE

!----------------------------------------------------------------------------
!                                                 ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate param from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(NewtonianFluidModel_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE NewtonianFluidModel_Class
