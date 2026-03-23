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

MODULE GmshBasicInterface
USE ISO_C_BINDING, ONLY: C_INT
USE ISO_C_BINDING, ONLY: C_DOUBLE
USE ISO_C_BINDING, ONLY: C_SIZE_T
USE ISO_C_BINDING, ONLY: C_PTR
USE ISO_C_BINDING, ONLY: C_CHAR
IMPLICIT NONE

PRIVATE
PUBLIC :: gmshInitialize
PUBLIC :: gmshIsInitialized
PUBLIC :: gmshFinalize
PUBLIC :: gmshOpen
PUBLIC :: gmshMerge
PUBLIC :: gmshWrite
PUBLIC :: gmshClear

INTEGER, PARAMETER, PUBLIC :: GMSH_API_VERSION_MAJOR = 4
INTEGER, PARAMETER, PUBLIC :: GMSH_API_VERSION_MINOR = 11
INTEGER, PARAMETER, PUBLIC :: GMSH_API_VERSION_PATCH = 0
INTEGER, PARAMETER, PUBLIC :: GMSH_API_MAX_STR_LEN = 256
CHARACTER(6), PARAMETER, PUBLIC :: GMSH_API_VERSION = "4.11.0"

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2026-03-22
! summary: Initialize Gmsh API.
!
!# GmshInitialize
!
! Initialize Gmsh API. This must be called before any call to the other
! functions in the API. If `argc' and `argv' (or just `argv' in Python or
! Julia) are provided, they will be handled in the same way as the command
! line arguments in the Gmsh app. If `readConfigFiles' is set, read system
! Gmsh configuration files (gmshrc and gmsh-options). Initializing the API
! sets the options "General.Terminal" to 1 and "General.AbortOnError" to 2.
!
!### CInterface
!
!```c
! GMSH_API void gmshInitialize(int argc, char **argv,
!                              const int readConfigFiles,
!                              int *ierr);
!```

INTERFACE
  SUBROUTINE gmshInitialize(argc, argv, readConfigFiles, run, ierr) &
    BIND(C, name="gmshInitialize")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: argc
    TYPE(C_PTR), INTENT(IN) :: argv(*)
    INTEGER(C_INT), VALUE, INTENT(IN) :: readConfigFiles
    INTEGER(C_INT), VALUE, INTENT(IN) :: run
    INTEGER(C_INT), INTENT(INOUT) :: ierr
  END SUBROUTINE gmshInitialize
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshIsInitialized(ierr) BIND(C, name="gmshIsInitialized")
    IMPORT
    INTEGER(C_INT), INTENT(INOUT) :: ierr
    INTEGER(C_INT) :: gmshIsInitialized
  END FUNCTION gmshIsInitialized
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2026-03-22
! summary: Finalize the Gmsh API. This must be called when you are done
! using the Gmsh API.
!
!# GmshFinalize
!
! Finalize the Gmsh API. This must be called when you are done using the
! Gmsh API.
!
!### CInterface
!
!```c
! GMSH_API void gmshFinalize(int *ierr);
!```

INTERFACE
  SUBROUTINE gmshFinalize(ierr) BIND(C, name="gmshFinalize")
    IMPORT
    INTEGER(C_INT), INTENT(INOUT) :: ierr
  END SUBROUTINE gmshFinalize
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2026-03-22
! summary:  Open a file.
!
!# GmshOpen
!
! Open a file. Equivalent to the `File->Open` menu in the Gmsh app. Handling
! of the file depends on its extension and/or its contents: opening a file
! with model data will create a new model.
!
!
!### CInterface
!
!```c
! GMSH_API void gmshOpen(const char *fileName,
!                        int *ierr);
!```

INTERFACE
  SUBROUTINE gmshOpen(fileName, ierr) BIND(C, name="gmshOpen")
    IMPORT
    TYPE(C_PTR), VALUE, INTENT(IN) :: fileName
    ! CHARACTER(LEN=1, KIND=C_CHAR), INTENT(IN) :: fileName(*)
    INTEGER(C_INT), INTENT(INOUT) :: ierr
  END SUBROUTINE gmshOpen
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2026-03-22
! summary: Merge a file
!
!# GmshMerge
!
! Merge a file. Equivalent to the `File->Merge` menu in the Gmsh app.
! Handling of the file depends on its extension and/or its contents. Merging
! a file with model data will add the data to the current model.
!
! GMSH_API void gmshMerge(const char *fileName,
!                         int *ierr);

INTERFACE
  SUBROUTINE gmshMerge(fileName, ierr) BIND(C, name="gmshMerge")
    IMPORT
    TYPE(C_PTR), VALUE, INTENT(IN) :: fileName
    ! CHARACTER(LEN=1, KIND=C_CHAR), INTENT(IN) :: fileName(*)
    INTEGER(C_INT), INTENT(INOUT) :: ierr
  END SUBROUTINE gmshMerge
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2026-03-22
! summary: Write a file
!
!# GmshWrite
!
! Write a file. The export format is determined by the file extension.
!
!### CInterface
!
!```c
! GMSH_API void gmshWrite(const char *fileName,
!                         int *ierr);
!```

INTERFACE
  SUBROUTINE gmshWrite(fileName, ierr) BIND(C, name="gmshWrite")
    IMPORT
    CHARACTER(LEN=1, KIND=C_CHAR), INTENT(IN) :: fileName(*)
    INTEGER(C_INT), INTENT(INOUT) :: ierr
  END SUBROUTINE gmshWrite
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2026-03-22
! summary: Clear all loaded models
!
!# GmshClear
!
! Clear all loaded models and post-processing data, and add a new empty
! model.
!
!### CInterface
!
!```c
! GMSH_API void gmshClear(int *ierr);
!```

INTERFACE
  SUBROUTINE gmshClear(ierr) BIND(C, name="gmshClear")
    IMPORT
    INTEGER(C_INT), INTENT(INOUT) :: ierr
  END SUBROUTINE gmshClear
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE GmshBasicInterface
