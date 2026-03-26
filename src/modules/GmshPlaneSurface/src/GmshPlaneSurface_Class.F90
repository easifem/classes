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

MODULE GmshPlaneSurface_Class
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: LGT
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_
USE GmshCurveLoop_Class, ONLY: GmshCurveLoop_
USE GmshCurveLoop_Class, ONLY: GmshCurveLoopPointer_
USE Gmsh_Class, ONLY: Gmsh_
USE BaseType, ONLY: math => TypeMathOpt
IMPLICIT NONE

PRIVATE
PUBLIC :: GmshPlaneSurface_
PUBLIC :: GmshPlaneSurfacePointer_
PUBLIC :: GmshPlaneSurfaceImportFromToml
INTEGER(I4B), PARAMETER :: innerloopExpandSize = math%two_i

!----------------------------------------------------------------------------
!                                                          GmshPlaneSurface_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-25
! summary: Gmsh plane surface

TYPE :: GmshPlaneSurface_
  PRIVATE
  INTEGER(I4B) :: indx = math%one_i
  !! surface id
  LOGICAL(LGT) :: isInnerLoop = math%no
  !! does inner loop exists
  INTEGER(I4B) :: innerloopsSize = math%zero_i
  !! size of inner loops
  INTEGER(I4B) :: innerloopsCapacity = math%zero_i
  !! capacity of inner loops
  TYPE(GmshCurveLoop_) :: outerloop
  !! outer loop
  TYPE(GmshCurveLoopPointer_), ALLOCATABLE :: innerloops(:)
  !! inner loops inside the plane surface

CONTAINS

  !  @Methods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: SetOuterloop => obj_SetOuterloop
  PROCEDURE, PUBLIC, PASS(obj) :: SetInnerloops => obj_SetInnerloops
  PROCEDURE, PUBLIC, PASS(obj) :: SetInnerloop => obj_SetInnerloop
  PROCEDURE, PUBLIC, PASS(obj) :: AddInnerloop => obj_AddInnerloop
  PROCEDURE, PUBLIC, PASS(obj) :: AllocateInnerloops => &
    obj_AllocateInnerloops
  PROCEDURE, PUBLIC, PASS(obj) :: SetIndx => obj_SetIndx
  PROCEDURE, PUBLIC, PASS(obj) :: GetOuterloop => obj_GetOuterloop
  PROCEDURE, PUBLIC, PASS(obj) :: GetInnerloops => &
    obj_GetInnerloops
  PROCEDURE, PUBLIC, PASS(obj) :: GetInnerloopPointer => &
    obj_GetInnerloopPointer
  PROCEDURE, PUBLIC, PASS(obj) :: GetIndx => obj_GetIndx
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display

  ! @TomlMethods
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2

  ! @GmshMethods
  PROCEDURE, PUBLIC, PASS(obj) :: CreateGmshModel => obj_CreateGmshModel
  !! create gmsh model
END TYPE GmshPlaneSurface_

!----------------------------------------------------------------------------
!                                                   GmshPlaneSurfacePointer_
!----------------------------------------------------------------------------

TYPE :: GmshPlaneSurfacePointer_
  CLASS(GmshPlaneSurface_), POINTER :: ptr => NULL()
END TYPE GmshPlaneSurfacePointer_

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Initiate GmshPlaneSurface
!
!# Initiate
!
! Initiate GmshPlaneSurface.

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, outerloop, innerloops, indx)
    CLASS(GmshPlaneSurface_), INTENT(INOUT) :: obj
    TYPE(GmshCurveLoop_), INTENT(IN) :: outerloop
    TYPE(GmshCurveLoopPointer_), INTENT(IN) :: innerloops(:)
    INTEGER(I4B), INTENT(IN) :: indx
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetOuterloop@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Set outerloop in GmshPlaneSurface
!
!# SetOuterloop
!
! Set outerloop in GmshPlaneSurface.

INTERFACE
  MODULE SUBROUTINE obj_SetOuterloop(obj, outerloop)
    CLASS(GmshPlaneSurface_), INTENT(INOUT) :: obj
    TYPE(GmshCurveLoop_), INTENT(IN) :: outerloop
  END SUBROUTINE obj_SetOuterloop
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetInnerloops@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Set all Innerloops in GmshPlaneSurface
!
!# SetInnerloop
!
! Set all Innerloops in GmshPlaneSurface.

INTERFACE
  MODULE SUBROUTINE obj_SetInnerloops(obj, innerloops)
    CLASS(GmshPlaneSurface_), INTENT(INOUT) :: obj
    TYPE(GmshCurveLoopPointer_), INTENT(IN) :: innerloops(:)
  END SUBROUTINE obj_SetInnerloops
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetInnerloop@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Set an innerloop in GmshPlaneSurface
!
!# SetInnerloop
!
! Set a innerloop in GmshPlaneSurface.

INTERFACE
  MODULE SUBROUTINE obj_SetInnerloop(obj, innerloop, loopId)
    CLASS(GmshPlaneSurface_), INTENT(INOUT) :: obj
    TYPE(GmshCurveLoop_), TARGET, INTENT(IN) :: innerloop
    INTEGER(I4B), INTENT(IN) :: loopId
  END SUBROUTINE obj_SetInnerloop
END INTERFACE

!----------------------------------------------------------------------------
!                                                       AddInnerloop@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Add (append) a innerloop in GmshPlaneSurface
!
!# AddInnerloop
!
! Add (append) a innerloop in GmshPlaneSurface.

INTERFACE
  MODULE SUBROUTINE obj_AddInnerloop(obj, innerloop)
    CLASS(GmshPlaneSurface_), INTENT(INOUT) :: obj
    TYPE(GmshCurveLoop_), TARGET, INTENT(IN) :: innerloop
  END SUBROUTINE obj_AddInnerloop
END INTERFACE

!----------------------------------------------------------------------------
!                                                  AllocateInnerloops@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: allocate inner loops
!
!# AllocateInnerloops
!
! Allocate innerloops in GmshPlaneSurface.

INTERFACE
  MODULE SUBROUTINE obj_AllocateInnerloops(obj, tsize)
    CLASS(GmshPlaneSurface_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: tsize
  END SUBROUTINE obj_AllocateInnerloops
END INTERFACE

!----------------------------------------------------------------------------
!                                                            SetIndx@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Set indx GmshPlaneSurface
!
!# SetIndx
!
! Set indx in GmshPlaneSurface

INTERFACE
  MODULE SUBROUTINE obj_SetIndx(obj, indx)
    CLASS(GmshPlaneSurface_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
  END SUBROUTINE obj_SetIndx
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetOuterloop@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Get outerloop from GmshPlaneSurface
!
!# GetOuterloop
!
! Get outerloop from GmshPlaneSurface.

INTERFACE
  MODULE FUNCTION obj_GetOuterloop(obj) RESULT(ans)
    CLASS(GmshPlaneSurface_), INTENT(IN) :: obj
    TYPE(GmshCurveLoop_) :: ans
  END FUNCTION obj_GetOuterloop
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetInnerloops@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Get all innerloops from GmshPlaneSurface
!
!# GetInnerLoops
!
! Get all the innerloops from GmshPlaneSurface.

INTERFACE
  MODULE FUNCTION obj_GetInnerloops(obj) RESULT(ans)
    CLASS(GmshPlaneSurface_), INTENT(IN) :: obj
    TYPE(GmshCurveLoopPointer_), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetInnerloops
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetInnerloop@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Get an innerloop from GmshPlaneSurface
!
!# GetInnerLoop
!
! Get an innerloop from GmshPlaneSurface.

INTERFACE
  MODULE FUNCTION obj_GetInnerloopPointer(obj, loopId) RESULT(ans)
    CLASS(GmshPlaneSurface_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: loopId
    CLASS(GmshCurveLoop_), POINTER :: ans
  END FUNCTION obj_GetInnerloopPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                            GetIndx@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Get indx from GmshPlaneSurface
!
!# GetIndx
!
! Get indx from GmshPlaneSurface.

INTERFACE
  MODULE FUNCTION obj_GetIndx(obj) RESULT(ans)
    CLASS(GmshPlaneSurface_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION obj_GetIndx
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Display@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Display GmshPlaneSurface
!
!# Display
!
! Display GmshPlaneSurface.

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(GmshPlaneSurface_), INTENT(INOUT) :: obj
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
! Import GmshPlaneSurface from toml table.

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(GmshPlaneSurface_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Import GmshPlaneSurface from toml file
!
!# ImportFromToml
!
! Import GmshPlaneSurface from toml file. After getting the toml table from
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
    CLASS(GmshPlaneSurface_), INTENT(INOUT) :: obj
    !! GmshPlaneSurface
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
! summary: Initiate a vector of GmshPlaneSurface_ from the toml table

INTERFACE GmshPlaneSurfaceImportFromToml
  MODULE SUBROUTINE obj_ImportFromToml3(obj, table, tomlName)
    TYPE(GmshPlaneSurfacePointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! Should be allocated outside
    TYPE(toml_table), INTENT(INOUT) :: table
    !! Toml table to returned
    CHARACTER(*), INTENT(IN) :: tomlName
  END SUBROUTINE obj_ImportFromToml3
END INTERFACE GmshPlaneSurfaceImportFromToml

!----------------------------------------------------------------------------
!                                                 ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-25
! summary: Import a vector of GmshPlaneSurfacePointer_ from toml file

INTERFACE GmshPlaneSurfaceImportFromToml
  MODULE SUBROUTINE obj_ImportFromToml4( &
    obj, tomlName, afile, filename, printToml)
    TYPE(GmshPlaneSurfacePointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
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
END INTERFACE GmshPlaneSurfaceImportFromToml

!----------------------------------------------------------------------------
!                                                 CreateGmshModel@GmshMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-26
! summary: Create gmsh model for plane surface
!
!# CreateGmshModel
!
! Create gmsh model for plane surface.

INTERFACE
  MODULE SUBROUTINE obj_CreateGmshModel(obj, gmsh)
    CLASS(GmshPlaneSurface_), INTENT(INOUT) :: obj
    TYPE(Gmsh_), INTENT(INOUT) :: gmsh
  END SUBROUTINE obj_CreateGmshModel
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE GmshPlaneSurface_Class
