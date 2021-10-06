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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Initialize Gmsh API.
!
!# Introduction
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
SUBROUTINE gmshInitialize( argc, argv, readConfigFiles, ierr ) &
  & BIND(C,name="gmshInitialize")
  IMPORT
  _I_V_IN_ :: argc
  _CPTR_IN_ :: argv
  _I_V_IN_ :: readConfigFiles
  _I_OUT_ :: ierr
END SUBROUTINE
END INTERFACE

PUBLIC :: gmshInitialize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Finalize the Gmsh API. This must be called when you are done using the Gmsh API.
!
!# Introduction
! Finalize the Gmsh API. This must be called when you are done using the Gmsh API.
!
!### CInterface
!
!```c
! GMSH_API void gmshFinalize(int *ierr);
!```

INTERFACE
SUBROUTINE gmshFinalize( ierr ) BIND(C,name="gmshFinalize")
  IMPORT
  _I_OUT_ :: ierr
END SUBROUTINE gmshFinalize
END INTERFACE

PUBLIC :: gmshFinalize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary:  Open a file.
!
!# Introduction
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
SUBROUTINE gmshOpen( fileName, ierr ) &
  & BIND( C, name="gmshOpen" )
  IMPORT
  _CPTR_V_IN_ :: fileName
  _I_OUT_ :: ierr
END SUBROUTINE gmshOpen
END INTERFACE

PUBLIC :: gmshOpen

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Merge a file
!
!# Introduction
!
! Merge a file. Equivalent to the `File->Merge` menu in the Gmsh app.
! Handling of the file depends on its extension and/or its contents. Merging
! a file with model data will add the data to the current model.
!
! GMSH_API void gmshMerge(const char *fileName,
!                         int *ierr);

INTERFACE
SUBROUTINE gmshMerge( fileName, ierr ) &
  & BIND( C, name="gmshMerge" )
  IMPORT
  _CPTR_V_IN_ :: fileName
  _I_OUT_ :: ierr
END SUBROUTINE gmshMerge
END INTERFACE

PUBLIC :: gmshMerge

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Write a file
!
!# Introduction
!
! Write a file. The export format is determined by the file extension.
!
!
!### CInterface
!
!```c
! GMSH_API void gmshWrite(const char *fileName,
!                         int *ierr);
!```

INTERFACE
SUBROUTINE gmshWrite( fileName, ierr ) &
  & BIND( C, name="gmshWrite" )
  IMPORT
  _CPTR_V_IN_ :: fileName
  _I_OUT_ :: ierr
END SUBROUTINE gmshWrite
END INTERFACE

PUBLIC :: gmshWrite

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Clear all loaded models
!
!# Introduction
!
! Clear all loaded models and post-processing data, and add a new empty
! model.
!
!
!### CInterface
!
!```c
! GMSH_API void gmshClear(int *ierr);
!```


INTERFACE
SUBROUTINE gmshClear( ierr ) BIND(C,name="gmshClear")
  IMPORT
  _I_OUT_ :: ierr
END SUBROUTINE gmshClear
END INTERFACE

PUBLIC :: gmshClear

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

