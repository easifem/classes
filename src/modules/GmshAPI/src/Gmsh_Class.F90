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
! date: 2026-03-24
! summary: Gmsh-Fortran Interface

MODULE Gmsh_Class
USE GlobalData, ONLY: I4B, LGT, DFP
USE BaseType, ONLY: math => TypeMathOpt
USE GmshInterface, ONLY: GMSH_API_MAX_STR_LEN
USE GmshInterface, ONLY: GMSH_API_VERSION_MAJOR
USE GmshInterface, ONLY: GMSH_API_VERSION_MINOR
USE GmshInterface, ONLY: GMSH_API_VERSION_PATCH
USE GmshInterface, ONLY: GMSH_API_VERSION
USE GmshGraphics_Class, ONLY: GmshGraphics_
USE GmshFLTK_Class, ONLY: GmshFLTK_
USE GmshOption_Class, ONLY: GmshOption_
USE GmshModel_Class, ONLY: GmshModel_
USE GmshOnelab_Class, ONLY: GmshOnelab_
IMPLICIT NONE

PRIVATE
PUBLIC :: GMSH_API_MAX_STR_LEN
PUBLIC :: GMSH_API_VERSION_MAJOR
PUBLIC :: GMSH_API_VERSION_MINOR
PUBLIC :: GMSH_API_VERSION_PATCH
PUBLIC :: GMSH_API_VERSION
PUBLIC :: Gmsh_
PUBLIC :: GmshPointer_
PUBLIC :: TypeGmsh

!----------------------------------------------------------------------------
!                                                                      Gmsh_
!---------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2026-03-18
! summary: This a data type for prepossing/ post-processing/ mesh handling
! using Gmsh
!
!# Gmsh_
!
!```fortran
! type( Gmsh_ ) :: Gmsh
! ierr = Gmsh%initialize()
! ierr = Gmsh%open( filename )
! ierr = Gmsh%merge( filename )
! ierr = Gmsh%write( filename )
! ierr = Gmsh%clear( filename )
! ierr = Gmsh%finalize()
!```

TYPE :: Gmsh_
  PRIVATE
  LOGICAL(LGT) :: isInit = .FALSE.
  !! IsInitiated
  TYPE(GmshOption_), PUBLIC, POINTER :: option => NULL()
  !! Gmsh option
  TYPE(GmshModel_), PUBLIC, POINTER :: model => NULL()
  !! Gmsh model
  TYPE(GmshGraphics_), PUBLIC, POINTER :: graphics => NULL()
  !! Gmsh graphics
  TYPE(GmshFLTK_), PUBLIC, POINTER :: fltk => NULL()
  !! Gmsh FLTK
  TYPE(GmshOnelab_), PUBLIC, POINTER :: onelab => NULL()
  !! Gmsh onelab
  ! INTEGER( I4B ) :: nsd = 0
  !! TODO
  ! TYPE( GmshView_ ), PUBLIC, POINTER :: view => NULL( )
  !! TODO
  ! TYPE( GmshPlugin_ ), PUBLIC, POINTER :: plugin => NULL( )
  !! TODO
  !! TYPE( GmshParser_ ), PUBLIC, POINTER :: parser => NULL()
  !! TODO Gmsh Parser
  ! TYPE( GmshLogger_ ), PUBLIC, POINTER :: logger => NULL( )
  !! TODO

CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: Initialize => obj_Initialize
  !! Initialize the Gmsh engine
  PROCEDURE, PUBLIC, PASS(obj) :: Finalize => obj_Finalize
  !! Closes the Gmsh engine
  PROCEDURE, PUBLIC, PASS(obj) :: IsInitialized => obj_IsInitialized
  !! is initialized
  PROCEDURE, PUBLIC, PASS(obj) :: IsInitiated => obj_IsInitiated
  !! is initiated
  FINAL :: obj_finalize_
  !! Final for gmsh
  PROCEDURE, PUBLIC, NOPASS :: OPEN => obj_Open
  !! open file to load
  PROCEDURE, PUBLIC, NOPASS :: Merge => obj_Merge
  !! merge model
  PROCEDURE, PUBLIC, NOPASS :: WRITE => obj_Write
  !! Write content in a file
  PROCEDURE, PUBLIC, NOPASS :: Clear => obj_Clear
  !! Clear the content
END TYPE Gmsh_

!----------------------------------------------------------------------------
!                                                                   TypeGmsh
!----------------------------------------------------------------------------

TYPE(Gmsh_), PARAMETER :: TypeGmsh = Gmsh_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshPointer_
  CLASS(Gmsh_), POINTER :: Ptr => NULL()
END TYPE GmshPointer_

!----------------------------------------------------------------------------
!                                                               IsInitialized
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-18
! summary: Returns isInit
!
!# IsInitialized
!
! Returns isInit.
!
INTERFACE
  MODULE FUNCTION obj_IsInitialized(obj) RESULT(ans)
    CLASS(Gmsh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_IsInitialized
END INTERFACE

!----------------------------------------------------------------------------
!                                                                IsInitiated
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-18
! summary: Returns isInit
!
!# IsInitiated
!
! Returns isInit.
!
INTERFACE
  MODULE FUNCTION obj_IsInitiated(obj) RESULT(ans)
    CLASS(Gmsh_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Initialize
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: This function will start the Gmsh engine
!
!# Initialize
!
! This function will start the Gmsh engine, and it allocates
! the pointer fields.
!
!### Usage
!
!```fortran
!ierr = obj%Initialize(NSD)
!```

INTERFACE
  MODULE FUNCTION obj_Initialize(obj, argv, readConfigFiles, run, fltk) &
    RESULT(ans)
    CLASS(Gmsh_), INTENT(INOUT) :: obj
    CHARACTER(*), OPTIONAL, INTENT(IN) :: argv(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: readConfigFiles
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: run
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: fltk
    INTEGER(I4B) :: ans
  END FUNCTION obj_Initialize
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Finalize
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: This function will stop the Gmsh engine
!
!# Introduction
! This function will stop the Gmsh engine
!
!### Usage
!
!```fortran
!ierr = obj%finalize()
!```

INTERFACE
  MODULE FUNCTION obj_Finalize(obj) RESULT(ans)
    CLASS(Gmsh_), INTENT(INOUT) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_Finalize
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Finalize
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-18
! summary: Finalize Gmsh

INTERFACE
  MODULE SUBROUTINE obj_Finalize_(obj)
    TYPE(Gmsh_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Finalize_
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Open
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary:  Open a file.
!
!# Open
!
! Open a file. Equivalent to the `File->Open` menu in the Gmsh app. Handling
! of the file depends on its extension and/or its contents: opening a file
! with model data will create a new model.
!
!
!### Usage
!
!```fortran
! type( gmsh_ ) :: gmsh
! integer( i4b ) :: ierr
! CALL GMSH_INIT
! ierr = gmsh%initialize()
! ierr = gmsh%open(fileName="t1.msh" )
! ierr = gmsh%write(fileName="t2.msh")
! ierr = gmsh%finalize()
! ierr = gmsh%clear()
! CALL GMSH_FINAL
!```

INTERFACE
  MODULE FUNCTION obj_Open(fileName) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: fileName
    INTEGER(I4B) :: ans
  END FUNCTION obj_Open
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Merge
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2026-03-18
! summary: Merge a file
!
!# Merge
!
! Merge a file. Equivalent to the `File->Merge` menu in the Gmsh app.
! Handling of the file depends on its extension and/or its contents. Merging
! a file with model data will add the data to the current model.
!
!### Usage
!
!```fortran
! type( gmsh_ ) :: gmsh
! integer( i4b ) :: ierr
! CALL GMSH_INIT
! ierr = gmsh%initialize()
! ierr = gmsh%open(fileName="t1.msh" )
! ierr = gmsh%write(fileName="t2.msh")
! ierr = gmsh%finalize()
! ierr = gmsh%clear()
! CALL GMSH_FINAL
!```

INTERFACE
  MODULE FUNCTION obj_Merge(fileName) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: fileName
    INTEGER(I4B) :: ans
  END FUNCTION obj_Merge
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2026-03-18
! summary: Write a file
!
!# Write
!
! Write a file. The export format is determined by the file extension.
!
!### Usage
!
!```fortran
! type( gmsh_ ) :: gmsh
! integer( i4b ) :: ierr
! CALL GMSH_INIT
! ierr = gmsh%initialize()
! ierr = gmsh%open(fileName="t1.msh" )
! ierr = gmsh%write(fileName="t2.msh")
! ierr = gmsh%finalize()
! ierr = gmsh%clear()
! CALL GMSH_FINAL
!```

INTERFACE
  MODULE FUNCTION obj_Write(fileName) RESULT(ans)
    CHARACTER(LEN=*), INTENT(IN) :: fileName
    INTEGER(I4B) :: ans
  END FUNCTION obj_Write
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Clear
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2026-03-18
! summary: Clear all loaded models
!
!# Clear
!
! Clear all loaded models and post-processing data, and add a new empty
! model.
!
!### Usage
!
!```fortran
! type( gmsh_ ) :: gmsh
! integer( i4b ) :: ierr
! CALL GMSH_INIT
! ierr = gmsh%initialize()
! ierr = gmsh%open(fileName="t1.msh" )
! ierr = gmsh%write(fileName="t2.msh")
! ierr = gmsh%finalize()
! ierr = gmsh%clear()
! CALL GMSH_FINAL
!```

INTERFACE
  MODULE FUNCTION obj_Clear() RESULT(ans)
    INTEGER(I4B) :: ans
  END FUNCTION obj_Clear
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Gmsh_Class
