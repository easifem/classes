! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

MODULE GmshLine_Class
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: LGT
USE BaseType, ONLY: math => TypeMathOpt
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_
USE Gmsh_Class, ONLY: Gmsh_
IMPLICIT NONE

PRIVATE
PUBLIC :: GmshLine_
PUBLIC :: GmshLinePointer_
PUBLIC :: GmshLineImportFromToml

!----------------------------------------------------------------------------
!                                                                  GmshLine_
!----------------------------------------------------------------------------

TYPE :: GmshLine_
  PRIVATE
  INTEGER(I4B) :: pointId(2)
  INTEGER(I4B) :: indx = math%one_i

CONTAINS

  ! @Methods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: SetPointId => obj_SetPointId
  PROCEDURE, PUBLIC, PASS(obj) :: SetIndx => obj_SetIndx
  PROCEDURE, PUBLIC, PASS(obj) :: GetPointId => obj_GetPointId
  PROCEDURE, PUBLIC, PASS(obj) :: GetIndx => obj_GetIndx
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display

  ! @TomlMethods
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2

  ! @GmshMethods
  PROCEDURE, PUBLIC, PASS(obj) :: CreateGmshModel => obj_CreateGmshModel
END TYPE GmshLine_

!----------------------------------------------------------------------------
!                                                          GmshLinePointer_
!----------------------------------------------------------------------------

TYPE :: GmshLinePointer_
  CLASS(GmshLine_), POINTER :: ptr => NULL()
END TYPE GmshLinePointer_

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Initiate GmshLine
!
!# Initiate
!
! Initiate GmshLine.

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, pointId, indx)
    CLASS(GmshLine_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: pointId(2)
    INTEGER(I4B), INTENT(IN) :: indx
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                         SetPointId@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Set pointId in GmshLine
!
!# SetPointId
!
! Set pointId in GmshLine.

INTERFACE
  MODULE SUBROUTINE obj_SetPointId(obj, pointId)
    CLASS(GmshLine_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: pointId(2)
  END SUBROUTINE obj_SetPointId
END INTERFACE

!----------------------------------------------------------------------------
!                                                            SetIndx@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Set indx GmshLine
!
!# SetIndx
!
! Set indx in GmshLine

INTERFACE
  MODULE SUBROUTINE obj_SetIndx(obj, indx)
    CLASS(GmshLine_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
  END SUBROUTINE obj_SetIndx
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetPointId@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Get pointId from GmshLine
!
!# GetPointId
!
! Get pointId from GmshLine.

INTERFACE
  MODULE FUNCTION obj_GetPointId(obj) RESULT(ans)
    CLASS(GmshLine_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans(2)
  END FUNCTION obj_GetPointId
END INTERFACE

!----------------------------------------------------------------------------
!                                                            GetIndx@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Get indx from GmshLine
!
!# GetIndx
!
! Get indx from GmshLine.

INTERFACE
  MODULE FUNCTION obj_GetIndx(obj) RESULT(ans)
    CLASS(GmshLine_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION obj_GetIndx
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Display@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Display GmshLine
!
!# Display
!
! Display GmshLine.

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(GmshLine_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Import from toml table.
!
!# ImportFromToml
!
! Import GmshLine from toml table.

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(GmshLine_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Import GmshLine from toml file
!
!# ImportFromToml
!
! Import GmshLine from toml file. After getting the toml table from
! the provided file, this method calls ImportFromToml1.
!
!## Examples
!
!```fortran
!{{% fortran-code file="examples/ImportFromToml_test_1.F90" %}}
!```

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2( &
    obj, tomlName, afile, filename, printToml)
    CLASS(GmshLine_), INTENT(INOUT) :: obj
    !! GmshLine
    CHARACTER(*), INTENT(IN) :: tomlName
    !! name of the key
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    !! txt file where toml config is stored
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    !! you can pass the filename, then we will make
    !! the file, open it and read the toml config and
    !! close the file.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
    !! if it is true then we will print the toml config
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!                                                    ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary: Initiate a vector of GmshLine_ from the toml table

INTERFACE GmshLineImportFromToml
  MODULE SUBROUTINE obj_ImportFromToml3(obj, table, tomlName)
    TYPE(GmshLinePointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! Should be allocated outside
    TYPE(toml_table), INTENT(INOUT) :: table
    !! Toml table to returned
    CHARACTER(*), INTENT(IN) :: tomlName
  END SUBROUTINE obj_ImportFromToml3
END INTERFACE GmshLineImportFromToml

!----------------------------------------------------------------------------
!                                                 ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-25
! summary: Import a vector of GmshLinePointer_ from toml file

INTERFACE GmshLineImportFromToml
  MODULE SUBROUTINE obj_ImportFromToml4( &
    obj, tomlName, afile, filename, printToml)
    TYPE(GmshLinePointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! Gmsh point
    CHARACTER(*), INTENT(IN) :: tomlName
    !! name of the key
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    !! txt file where toml config is stored
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    !! you can pass the filename, then we will make
    !! the file, open it and read the toml config and
    !! close the file.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
    !! if it is true then we will print the toml config
  END SUBROUTINE obj_ImportFromToml4
END INTERFACE GmshLineImportFromToml

!----------------------------------------------------------------------------
!                                                CreateGmshModel@GmshMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-26
! summary: Create gmsh model for GmshCurveLoop_
!
!# CreateGmshModel
!
! Create gmsh model for GmshCurveLoop_

INTERFACE
  MODULE SUBROUTINE obj_CreateGmshModel(obj, gmsh)
    CLASS(GmshLine_), INTENT(INOUT) :: obj
    TYPE(Gmsh_), INTENT(INOUT) :: gmsh
  END SUBROUTINE obj_CreateGmshModel
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE GmshLine_Class
