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
! date: 	26 April 2021
! summary:Gmsh-Fortran Interface

#ifdef USE_GMSH_SDK
MODULE Gmsh_Class
USE ISO_C_BINDING
USE GlobalData, ONLY: I4B, LGT, DFP
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
USE GmshInterface
USE GmshGraphics_Class
USE GmshFLTK_Class
USE GmshOption_Class
USE GmshModel_Class
USE GmshOnelab_Class
IMPLICIT NONE
PRIVATE

CHARACTER( LEN = * ), PARAMETER :: modName = "GMSH_CLASS"
INTEGER( C_INT ) :: ierr
!$OMP THREADPRIVATE(ierr)
TYPE( ExceptionHandler_ ) :: e
!$OMP THREADPRIVATE(e)
INTEGER( I4B ), PARAMETER :: maxStrLen = 256

PUBLIC :: GMSH_INIT
PUBLIC :: GMSH_FINAL

!----------------------------------------------------------------------------
!                                                                      Gmsh_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	26 April
! summary: This a data type for prepossing/ post-processing/ mesh handling using Gmsh
!
!
!### Usage
!
! ```fortran
!	type( Gmsh_ ) :: Gmsh
! ierr = Gmsh%initialize()
! ierr = Gmsh%open( filename )
! ierr = Gmsh%merge( filename )
! ierr = Gmsh%write( filename )
! ierr = Gmsh%clear( filename )
! ierr = Gmsh%finalize()
! ```

TYPE :: Gmsh_
  PRIVATE
  TYPE( GmshGraphics_ ), PUBLIC, POINTER :: Graphics => NULL()
  TYPE( GmshOption_ ), PUBLIC, POINTER :: Option => NULL()
  TYPE( GmshFLTK_ ), PUBLIC, POINTER :: FLTK => NULL()
  TYPE( GmshOnelab_ ), PUBLIC, POINTER :: Onelab => NULL()
  TYPE( GmshModel_ ), PUBLIC, POINTER :: Model => NULL( )
  ! INTEGER( I4B ) :: nsd = 0
  LOGICAL( LGT ) :: isInitiated = .FALSE.

  CONTAINS
    PRIVATE
    PROCEDURE, PUBLIC, PASS( obj ) :: initialize => Gmsh_initialize
      !! Initialize the Gmsh engine
    PROCEDURE, PUBLIC, PASS( obj ) :: finalize => Gmsh_finalize
      !! Closes the Gmsh engine
    FINAL :: Gmsh_finalize_
      !! Final for Gmsh_
    PROCEDURE, PUBLIC, PASS( obj ) :: open => Gmsh_open
      !! open file to load
    PROCEDURE, PUBLIC, PASS( obj ) :: merge => Gmsh_merge
      !! merge model
    PROCEDURE, PUBLIC, PASS( obj ) :: write => Gmsh_write
      !! Write content in a file
    PROCEDURE, PUBLIC, PASS( obj ) :: clear => Gmsh_clear
      !! Clear the content
END TYPE Gmsh_

PUBLIC :: Gmsh_
TYPE( Gmsh_ ), PUBLIC, PARAMETER :: TypeGmsh = Gmsh_( )

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshPointer_
  CLASS( Gmsh_ ), POINTER :: Ptr => NULL()
END TYPE GmshPointer_

PUBLIC :: GmshPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: This function will start the Gmsh engine
!
!# Introduction
!
! This function will start the Gmsh engine, and it allocates
! the pointer fields.
!
!### Usage
!
!```fortran
! ierr = obj%initialize( NSD )
!```

FUNCTION Gmsh_initialize( obj ) RESULT( ans )
  CLASS( Gmsh_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ) :: ans
  ! Internal variables
  CHARACTER( LEN=* ), PARAMETER :: myName="Gmsh_initialize()"

  ans = 0
  IF( .NOT. obj%isInitiated ) THEN
    !> Graphics
    IF( ASSOCIATED( obj%Graphics )  ) THEN
      CALL e%raiseError(modName//"::"//myName//" - "// &
        "Gmsh%Graphics is already associated; hint: You can try, first Nullifying it")
      ans = -1
    END IF
    ALLOCATE( obj%Graphics ); CALL obj%Graphics%Initiate()
    !> Option
    IF( ASSOCIATED( obj%Option )  ) THEN
      CALL e%raiseError(modName//"::"//myName//" - "// &
        "Gmsh%option is already associated; hint: You can try, first Nullifying it")
      ans = -1
    END IF
    ALLOCATE( obj%Option ); Call obj%Option%Initiate()
    !> FLTK
    IF( ASSOCIATED( obj%FLTK )  ) THEN
      CALL e%raiseError(modName//"::"//myName//" - "// &
        "Gmsh%FLTK is already associated; hint: You can try, first Nullifying it")
      ans = -1
    END IF
    ALLOCATE( obj%FLTK ); CALL obj%FLTK%Initiate()
    !> Onelab
    IF( ASSOCIATED( obj%Onelab )  ) THEN
      CALL e%raiseError(modName//"::"//myName//" - "// &
        "Gmsh%Onelab is already associated; hint: You can try, first Nullifying it")
      ans = -1
    END IF
    ALLOCATE( obj%Onelab ); CALL obj%Onelab%Initiate()
    !> Model
    IF( ASSOCIATED( obj%Model )  ) THEN
      CALL e%raiseError(modName//"::"//myName//" - "// &
        "Gmsh%model is already associated; hint: You can try, first Nullifying it")
      ans = -1
    END IF
    ALLOCATE( obj%Model ); CALL obj%Model%Initiate()
    obj%isInitiated = .TRUE.
  ELSE
    CALL e%raiseError(modName//"::"//myName//" - "// &
      & "Gmsh is already initiated; hint: You can run finalize(), &
      & the initialize()")
  END IF
END FUNCTION Gmsh_initialize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: This function will start the Gmsh engine
!
!# Introduction
!
! This function will start the Gmsh engine.
!
!### Usage
!
!```fortran
!  CALL GMSH_INIT
!```

SUBROUTINE GMSH_INIT
  CALL gmshInitialize( 0, C_NULL_PTR, 1, ierr )
END SUBROUTINE GMSH_INIT

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
!	ierr = obj%finalize()
!```

FUNCTION Gmsh_finalize( obj ) RESULT( ans )
  CLASS( Gmsh_  ), INTENT( INOUT ) :: obj
  INTEGER( I4B ) :: ans

  IF( ASSOCIATED( obj%Graphics ) ) THEN
    DEALLOCATE( obj%Graphics )
  END IF
  IF( ASSOCIATED( obj%Option ) ) THEN
    DEALLOCATE( obj%Option )
  END IF
  IF( ASSOCIATED( obj%FLTK ) ) THEN
    DEALLOCATE( obj%FLTK )
  END IF
  IF( ASSOCIATED( obj%Model ) ) THEN
    IF( ASSOCIATED( obj%Model%Geo ) ) THEN
      DEALLOCATE( obj%Model%Geo )
    END IF
    IF( ASSOCIATED( obj%Model%Occ ) ) THEN
      DEALLOCATE( obj%Model%Occ )
    END IF
    IF( ASSOCIATED( obj%Model%Mesh ) ) THEN
      DEALLOCATE( obj%Model%Mesh )
    END IF
    DEALLOCATE( obj%Model )
  END IF
  obj%Option => null()
  obj%Model => null()
  obj%isInitiated = .FALSE.
  ans = 0
END FUNCTION Gmsh_finalize

!----------------------------------------------------------------------------
!
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
!	CALL GMSH_FINAL
!```

SUBROUTINE GMSH_FINAL
  CALL gmshFinalize( ierr = ierr )
END SUBROUTINE GMSH_FINAL

!----------------------------------------------------------------------------
!                                                                 Finalize
!----------------------------------------------------------------------------

SUBROUTINE Gmsh_finalize_( obj )
  TYPE( Gmsh_  ), INTENT( INOUT ) :: obj
  INTEGER( I4B ) :: ans
  ans = obj%Finalize()
END SUBROUTINE Gmsh_finalize_

!----------------------------------------------------------------------------
!                                                                      Open
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

FUNCTION Gmsh_Open( obj, fileName ) RESULT( ans )
  CLASS( Gmsh_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: fileName
  INTEGER( I4B ) :: ans
  ! Internal variables
  CHARACTER( LEN = maxStrLen ), TARGET :: C_STR
  C_STR = TRIM( fileName ) // C_NULL_CHAR
  CALL gmshOpen( fileName = C_LOC(C_STR), ierr=ierr )
  ans = INT( ierr, KIND=I4B )
END FUNCTION Gmsh_Open

!----------------------------------------------------------------------------
!                                                                     Close
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

FUNCTION Gmsh_Merge( obj, fileName ) RESULT( ans )
  CLASS( Gmsh_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: fileName
  INTEGER( I4B ) :: ans
  ! Internal variables
  CHARACTER( LEN = maxStrLen ), TARGET :: C_STR
  C_STR = TRIM( fileName ) // C_NULL_CHAR
  CALL gmshMerge( fileName = C_LOC(C_STR), ierr=ierr )
  ans = INT( ierr, KIND=I4B )
END FUNCTION Gmsh_Merge

!----------------------------------------------------------------------------
!                                                                     Write
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

FUNCTION Gmsh_Write( obj, fileName ) RESULT( ans )
  CLASS( Gmsh_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: fileName
  INTEGER( I4B ) :: ans
  ! Internal variables
  CHARACTER( LEN = maxStrLen ), TARGET :: C_STR

  C_STR = TRIM( fileName ) // C_NULL_CHAR
  CALL gmshWrite( fileName = C_LOC(C_STR), ierr=ierr )
  ans = INT( ierr, KIND=I4B )
END FUNCTION Gmsh_Write

!----------------------------------------------------------------------------
!                                                                     Clear
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

FUNCTION Gmsh_Clear( obj ) RESULT( ans )
  CLASS( Gmsh_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ) :: ans
  CALL gmshClear( ierr=ierr )
  ans = INT( ierr, KIND=I4B )
END FUNCTION Gmsh_Clear

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Gmsh_Class
#endif
