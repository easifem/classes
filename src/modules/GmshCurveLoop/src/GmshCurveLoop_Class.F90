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

MODULE GmshCurveLoop_Class
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: LGT
USE BaseType, ONLY: math => TypeMathOpt
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_
USE Gmsh_Class, ONLY: Gmsh_
IMPLICIT NONE

PRIVATE
PUBLIC :: GmshCurveLoop_
PUBLIC :: GmshCurveLoopPointer_
PUBLIC :: GmshCurveLoopImportFromToml

!----------------------------------------------------------------------------
!                                                             GmshCurveLoop_
!----------------------------------------------------------------------------

TYPE :: GmshCurveLoop_
  PRIVATE
  INTEGER(I4B) :: indx = math%one_i
  !! surface id
  LOGICAL(LGT) :: reorient = math%no
  !! should we reorient the loop
  INTEGER(I4B), ALLOCATABLE :: curveId(:)
  !! indx of curve entities
CONTAINS

  ! @Methods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: SetCurveId => obj_SetCurveId
  PROCEDURE, PUBLIC, PASS(obj) :: SetIndx => obj_SetIndx
  PROCEDURE, PUBLIC, PASS(obj) :: GetCurveId => obj_GetCurveId
  PROCEDURE, PUBLIC, PASS(obj) :: GetIndx => obj_GetIndx
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  PROCEDURE, PUBLIC, PASS(obj) :: Copy => obj_Copy

  ! @TomlMethods
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2

  !@GmshMethods
  PROCEDURE, PUBLIC, PASS(obj) :: CreateGmshModel => obj_CreateGmshModel
END TYPE GmshCurveLoop_

!----------------------------------------------------------------------------
!                                                      GmshCurveLoopPointer_
!----------------------------------------------------------------------------

TYPE :: GmshCurveLoopPointer_
  CLASS(GmshCurveLoop_), POINTER :: ptr => NULL()
END TYPE GmshCurveLoopPointer_

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Initiate GmshCurveLoop
!
!# Initiate
!
! Initiate GmshCurveLoop.

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, curveId, indx, reorient)
    CLASS(GmshCurveLoop_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: curveId(:)
    INTEGER(I4B), INTENT(IN) :: indx
    LOGICAL(LGT), INTENT(IN) :: reorient
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                         SetCurveId@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Set CurveId in GmshCurveLoop
!
!# SetCurveId
!
! Set CurveId in GmshCurveLoop.

INTERFACE
  MODULE SUBROUTINE obj_SetCurveId(obj, curveId)
    CLASS(GmshCurveLoop_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: curveId(:)
  END SUBROUTINE obj_SetCurveId
END INTERFACE

!----------------------------------------------------------------------------
!                                                            SetIndx@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Set indx GmshCurveLoop
!
!# SetIndx
!
! Set indx in GmshCurveLoop

INTERFACE
  MODULE SUBROUTINE obj_SetIndx(obj, indx)
    CLASS(GmshCurveLoop_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
  END SUBROUTINE obj_SetIndx
END INTERFACE

!----------------------------------------------------------------------------
!                                                        SetReorient@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Set Reorient GmshCurveLoop
!
!# SetReorient
!
! Set Reorient in GmshCurveLoop

INTERFACE
  MODULE SUBROUTINE obj_SetReorient(obj, reorient)
    CLASS(GmshCurveLoop_), INTENT(INOUT) :: obj
    LOGICAL(LGT), INTENT(IN) :: reorient
  END SUBROUTINE obj_SetReorient
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetCurveId@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Get CurveId from GmshCurveLoop
!
!# GetCurveId
!
! Get CurveId from GmshCurveLoop.

INTERFACE
  MODULE FUNCTION obj_GetCurveId(obj) RESULT(ans)
    CLASS(GmshCurveLoop_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetCurveId
END INTERFACE

!----------------------------------------------------------------------------
!                                                            GetIndx@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Get indx from GmshCurveLoop
!
!# GetIndx
!
! Get indx from GmshCurveLoop.

INTERFACE
  MODULE FUNCTION obj_GetIndx(obj) RESULT(ans)
    CLASS(GmshCurveLoop_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetIndx
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetReorient@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Get Reorient from GmshCurveLoop
!
!# GetReorient
!
! Get Reorient from GmshCurveLoop.

INTERFACE
  MODULE FUNCTION obj_GetReorient(obj) RESULT(ans)
    CLASS(GmshCurveLoop_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_GetReorient
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Copy@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-25
! summary: Copy GmshCurveLoop

INTERFACE
  MODULE SUBROUTINE obj_Copy(obj, obj2)
    CLASS(GmshCurveLoop_), INTENT(INOUT) :: obj
    CLASS(GmshCurveLoop_), INTENT(IN) :: obj2
  END SUBROUTINE obj_Copy
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Display@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Display GmshCurveLoop
!
!# Display
!
! Display GmshCurveLoop.

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(GmshCurveLoop_), INTENT(INOUT) :: obj
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
! Import GmshCurveLoop from toml table.

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(GmshCurveLoop_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Import GmshCurveLoop from toml file
!
!# ImportFromToml
!
! Import GmshCurveLoop from toml file. After getting the toml table from
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
    CLASS(GmshCurveLoop_), INTENT(INOUT) :: obj
    !! GmshCurveLoop
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
! summary: Initiate a vector of GmshCurveLoop_ from the toml table

INTERFACE GmshCurveLoopImportFromToml
  MODULE SUBROUTINE obj_ImportFromToml3(obj, table, tomlName)
    TYPE(GmshCurveLoopPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! Should be allocated outside
    TYPE(toml_table), INTENT(INOUT) :: table
    !! Toml table to returned
    CHARACTER(*), INTENT(IN) :: tomlName
  END SUBROUTINE obj_ImportFromToml3
END INTERFACE GmshCurveLoopImportFromToml

!----------------------------------------------------------------------------
!                                                 ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-25
! summary: Import a vector of GmshCurveLoopPointer_ from toml file

INTERFACE GmshCurveLoopImportFromToml
  MODULE SUBROUTINE obj_ImportFromToml4( &
    obj, tomlName, afile, filename, printToml)
    TYPE(GmshCurveLoopPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
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
END INTERFACE GmshCurveLoopImportFromToml

!----------------------------------------------------------------------------
!                                                 CreateGmshModel@GmshMethods
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
    CLASS(GmshCurveLoop_), INTENT(INOUT) :: obj
    TYPE(Gmsh_), INTENT(INOUT) :: gmsh
  END SUBROUTINE obj_CreateGmshModel
END INTERFACE

END MODULE GmshCurveLoop_Class
