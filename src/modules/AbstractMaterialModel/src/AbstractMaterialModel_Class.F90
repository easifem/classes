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
USE GlobalData, ONLY: I4B, LGT, DFP
USE String_Class, ONLY: String
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Class, ONLY: HDF5File_
USE TxtFile_Class, ONLY: TxtFile_
USE tomlf, ONLY: toml_table

IMPLICIT NONE

PRIVATE

PUBLIC :: AbstractMaterialModel_
PUBLIC :: AbstractMaterialModelPointer_
PUBLIC :: AbstractMaterialModelDeallocate
PUBLIC :: TypeMaterialModelOpt

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "AbstractMaterialModel_Class"
#endif

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: MaterialModelOpt_
  CHARACTER(18) :: linearElastic = "LINEARELASTICMODEL"
  CHARACTER(19) :: newtonianFluid = "NEWTONIANFLUIDMODEL"
END TYPE MaterialModelOpt_

TYPE(MaterialModelOpt_), PARAMETER :: TypeMaterialModelOpt=MaterialModelOpt_()

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

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate

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
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetName => obj_GetName
  !! Returns the name
  PROCEDURE, PUBLIC, PASS(obj) :: isInitiated => obj_isInitiated
  !! Returns the init status
  PROCEDURE, PUBLIC, PASS(obj) :: GetDataSize => obj_GetDataSize
  !! Get size of data
  PROCEDURE, PUBLIC, PASS(obj) :: GetData => obj_GetData
  !! Get the data

  ! SET:
  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: SetIsInitiated => obj_SetIsInitiated
  !! Set the status of AbstractMaterialModel
  PROCEDURE, PUBLIC, PASS(obj) :: SetName => obj_SetName
  !! Set name of AbstractMaterialModel_
  PROCEDURE, PUBLIC, PASS(obj) :: SetData => obj_SetData
  !! Set data
  PROCEDURE, PUBLIC, PASS(obj) :: UpdateData => obj_UpdateData
  !! Get updated data
END TYPE AbstractMaterialModel_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: AbstractMaterialModelPointer_
  CLASS(AbstractMaterialModel_), POINTER :: ptr => NULL()
END TYPE AbstractMaterialModelPointer_

!----------------------------------------------------------------------------
!                                               Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-30
! summary: Deallocate the data stored in AbstractMaterialModel_

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(AbstractMaterialModel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

INTERFACE AbstractMaterialModelDeallocate
  MODULE PROCEDURE obj_Deallocate
END INTERFACE AbstractMaterialModelDeallocate

!----------------------------------------------------------------------------
!                                                           Import@HDFMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary: Import data from the HDF5File

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group)
    CLASS(AbstractMaterialModel_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@HDFMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Export the data into HDF5File

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(AbstractMaterialModel_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Display the content

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(AbstractMaterialModel_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                 ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate param from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(AbstractMaterialModel_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                  ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate kernel from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, filename, &
                                        printToml)
    CLASS(AbstractMaterialModel_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetIsInitiated@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Set initiate status

INTERFACE
  MODULE SUBROUTINE obj_SetIsInitiated(obj, VALUE)
    CLASS(AbstractMaterialModel_), INTENT(INOUT) :: obj
    LOGICAL(LGT), INTENT(IN) :: VALUE
  END SUBROUTINE obj_SetIsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetName@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Set the name of material model

INTERFACE
  MODULE SUBROUTINE obj_SetName(obj, VALUE)
    CLASS(AbstractMaterialModel_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: VALUE
  END SUBROUTINE obj_SetName
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetData@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Set data

INTERFACE
  MODULE SUBROUTINE obj_SetData(obj, DATA)
    CLASS(AbstractMaterialModel_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: DATA(:)
  END SUBROUTINE obj_SetData
END INTERFACE

!----------------------------------------------------------------------------
!                                                       UpdateData@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Update data

INTERFACE
  MODULE SUBROUTINE obj_UpdateData(obj, DATA)
    CLASS(AbstractMaterialModel_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: DATA(:)
  END SUBROUTINE obj_UpdateData
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetName@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Get the name of model

INTERFACE
  MODULE FUNCTION obj_GetName(obj) RESULT(ans)
    CLASS(AbstractMaterialModel_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetName
END INTERFACE

!----------------------------------------------------------------------------
!                                                      isInitiated@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Returns the init status of obj

INTERFACE
  MODULE FUNCTION obj_isInitiated(obj) RESULT(ans)
    CLASS(AbstractMaterialModel_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetDataSize@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Get the size of data needed by obj

INTERFACE
  MODULE FUNCTION obj_GetDataSize(obj) RESULT(ans)
    CLASS(AbstractMaterialModel_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetDataSize
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Get the  data from the model

INTERFACE
  MODULE SUBROUTINE obj_GetData(obj, DATA)
    CLASS(AbstractMaterialModel_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: DATA(:)
  END SUBROUTINE obj_GetData
END INTERFACE

END MODULE AbstractMaterialModel_Class
