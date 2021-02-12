MODULE gmsh_Class
  !! Gmsh Class

USE BaseType
USE BaseMethod
USE gmshModel_Class

IMPLICIT NONE

PRIVATE

!----------------------------------------------------------------------------
!                                                                      gmsh_
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This a data type for prepossing and post-processing in Gmsh
!
!
!### Usage
!
! ```fortran
!	type( gmsh_ ) :: gmsh
! ierr = gmsh % initialize
! ierr = gmsh % open( filename )
! ierr = gmsh % merge( filename )
! ierr = gmsh % write( filename )
! ierr = gmsh % clear( filename )
! ierr = gmsh % finalize
! ```

TYPE :: gmsh_
  TYPE( gmshModel_ ), POINTER :: model => NULL( )
  INTEGER( I4B ) :: nsd = 0

  CONTAINS
    PROCEDURE, PUBLIC, PASS( Obj ) :: initialize => gmsh_init
      !! Initialize the gmsh engine
    PROCEDURE, PUBLIC, PASS( Obj ) :: initiate => gmsh_init
      !! Initialize the gmsh engine
    PROCEDURE, PUBLIC, PASS( Obj ) :: finalize => gmsh_final
      !! Closes the gmsh engine
    PROCEDURE, PUBLIC, PASS( Obj ) :: open => gmsh_open
      !! open file to load
    PROCEDURE, PUBLIC, PASS( Obj ) :: merge => gmsh_merge
    PROCEDURE, PUBLIC, PASS( Obj ) :: write => gmsh_write
      !! Write content in a file
    PROCEDURE, PUBLIC, PASS( Obj ) :: clear => gmsh_clear
      !! Clear the content
    PROCEDURE, PUBLIC, PASS( Obj ) :: remesh => gmsh_from_gmsh
      !! generate new gmsh file
END TYPE gmsh_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PUBLIC :: gmsh_
TYPE( gmsh_ ), PUBLIC, PARAMETER :: TypeGmsh = gmsh_( )

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
!! This function generate a gmsh model from the mesh data stored inside

!> authors: Dr. Vikas Sharma
!
! This function generates a gmsh model by reading mesh data stored inside
! `gmsh_` object.

MODULE FUNCTION gmsh_from_gmsh( Obj, gmsh, Nodes ) RESULT( Ans )
  CLASS( gmsh_ ), TARGET, INTENT( INOUT) :: Obj
    !! Old gmsh
  CLASS( gmsh_ ), TARGET, INTENT( INOUT) :: gmsh
    !! new gmsh
  REAL( DFP ), INTENT( IN ) :: Nodes( :, : )
    !! nodes
  INTEGER( I4B ) :: Ans
END FUNCTION gmsh_from_gmsh
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This function will start the gmsh engine
! It allocates obj % model
!
!### Usage
!
! ```fortran
!	ierr = obj % initialize( NSD )
! ```

FUNCTION gmsh_init( Obj, NSD ) RESULT( Ans )
  CLASS( gmsh_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: NSD
  INTEGER( I4B ) :: Ans

  CALL Display( "gmsh:: Initiating gmsh" )
  IF( ASSOCIATED( Obj % model )  ) DEALLOCATE( Obj % model )
  ALLOCATE( obj % model )
  ans = 0
  Obj % NSD = NSD

END FUNCTION gmsh_init

!----------------------------------------------------------------------------
!                                                                 Finalize
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This function will stop the gmsh engine
!
!### Usage
!
! ```fortran
!	ierr = obj % finalize()
! ```

FUNCTION gmsh_final( Obj ) RESULT( Ans )
  CLASS( gmsh_  ), INTENT( INOUT) :: Obj
  INTEGER( I4B ) :: Ans

  CALL Display( "gmsh:: Deallocating gmsh%modelc")
  IF( ASSOCIATED( obj % model ) ) DEALLOCATE( obj % model )
  obj % model => null()
  Ans = 0
  Obj % NSD = 0
END FUNCTION gmsh_final

!----------------------------------------------------------------------------
!                                                                      open
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This function will open a file and read its content
! Based upon the extension of the file it can take different actions
! If file represents a model file then a new model will be created

FUNCTION gmsh_open( Obj, P, F, E ) RESULT( Ans )
  CLASS( gmsh_ ), INTENT( INOUT) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: P
    !! Path of file
  CHARACTER( LEN = * ), INTENT( IN ) :: F
    !! File name
  CHARACTER( LEN = * ), INTENT( IN ) :: E
    !! Extension
  INTEGER( I4B ) :: Ans

  ! main program
  Ans = 0
  IF( trim( E ) .EQ. ".msh" ) THEN

    ! we need to create a model in this case
    IF( .NOT. ASSOCIATED( Obj % model ) ) THEN
      CALL Display( "gmsh::  allocating obj % model" )
      ALLOCATE( Obj % model )
    END IF

    IF( .NOT. ASSOCIATED( Obj % model % mesh ) ) THEN
      CALL Display( "gmsh:: allocating obj % model % mesh" )
      ALLOCATE( Obj % model % mesh )
    ELSE
      CALL Display( "WARNING:: gmsh_Class@Methods.f90" )
      CALL Display( "        gmsh_open()" )
      CALL Display( "        gmh%model%mesh is associated" )
      CALL Display( "           calling deallocateData()" )
      CALL Display( "           buffer is untouched()" )
      CALL Obj % model % mesh % Finalize( )
    END IF

    CALL Display( "gmsh:: making obj % model % mesh object" )
    CALL Obj % model % mesh % initiate( P, F, E, Obj % NSD )

  ELSE IF( trim( E ) .EQ. ".geo" ) THEN
    CALL Display( "ERROR:: gmsh_Class@Methods.f90" )
    CALL Display( "        gmsh_open()" )
    CALL Display( "        Currently .geo file cannot be loaded" )
    STOP
  END IF
END FUNCTION gmsh_open

!----------------------------------------------------------------------------
!                                                                    merge
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This function will open a file and read its content
! Based upon the extension of the file it can take different actions
! IF the file represents a model file then it will merge the content to the
! current model

FUNCTION gmsh_merge( Obj, P, F, E ) RESULT( Ans )
  CLASS( gmsh_ ), INTENT( INOUT) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: P
    !! Path of file
  CHARACTER( LEN = * ), INTENT( IN ) :: F
    !! File name
  CHARACTER( LEN = * ), INTENT( IN ) :: E
    !! Extension
  INTEGER( I4B ) :: Ans

  ! main program
  CALL EqualLine( )
  CALL Display( "ERROR:: gmsh_Class@Methods.f90" )
  CALL Display( "        gmsh_merge()" )
  CALL Display( "        Currently not supported" )
  CALL EqualLine( )
END FUNCTION gmsh_merge

!----------------------------------------------------------------------------
!                                                                     write
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This function will write the data in a file depending upon the extension
! of the file

FUNCTION gmsh_write( Obj, P, F, E ) RESULT( Ans )
  CLASS( gmsh_ ), INTENT( INOUT) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: P
    !! Path of file
  CHARACTER( LEN = * ), INTENT( IN ) :: F
    !! File name
  CHARACTER( LEN = * ), INTENT( IN ) :: E
    !! Extension
  INTEGER( I4B ) :: Ans

  ! Internal variables
  TYPE( File_ ) :: aFile

  IF( .NOT. ASSOCIATED( Obj % model ) ) THEN
    CALL Display( "ERROR:: gmsh_Class@Methods.f90" )
    CALL Display( "        gmsh_write()" )
    CALL Display( "        obj % model is not allocated/added" )
    STOP
  END IF

  IF( trim( E ) .EQ. ".msh" ) THEN
    CALL Display( "gmsh:: opening a file to write .geo file" )
    CALL OpenFileToWrite( aFile, P, F,".geo" )

    CALL Display( "gmsh:: calling gmsh%model%write()" )
    Ans = obj % model % write( aFile % UnitNo )

    WRITE( aFile % UnitNo, "(A)" ) 'Save "' // trim( P ) // trim( F ) &
      & // trim( E ) // '" ;'
    WRITE( aFile % UnitNo, "(A)" ) "Exit; "
    CALL CloseFile( aFile )

    CALL ExecuteCommand( 'gmsh "'// trim(P) // trim(F) // ".geo" //'"', &
      & "gmsh_Class.f90>>gmsh_write()")

  ELSE IF( trim( E ) .EQ. ".geo" ) THEN

    CALL Display( "gmsh:: opening a file to write .geo file" )
    CALL OpenFileToWrite( aFile, P, F, E )

    CALL Display( "gmsh:: calling gmsh%model%write()" )
    Ans = obj % model % write( aFile % UnitNo )
    CALL CloseFile( aFile )

  ELSE

    CALL Display( "ERROR:: gmsh_Class@Methods.f90" )
    CALL Display( "        gmsh_write()" )
    CALL Display( "        Unknown file extension" )
    STOP

  END IF

END FUNCTION gmsh_write

!----------------------------------------------------------------------------
!                                                                    clear
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This function will clear all model and option and create an blank model

FUNCTION gmsh_clear( Obj ) RESULT( Ans )
  CLASS( gmsh_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ) :: Ans
  CALL EqualLine( )
  CALL Display( "     gmsh_Class@Methods.f90" )
  CALL Display( "        gmsh_clear()" )
  CALL Display( "        cleanup done" )
  CALL EqualLine( )
  IF( ASSOCIATED( Obj % model ) ) DEALLOCATE( Obj % model )
  Obj % model => NULL( )
END FUNCTION gmsh_clear

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE gmsh_Class
