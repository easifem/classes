MODULE mshFormat_Class
  !! This module defines a mesh format

USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                                 mshFormat_
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This data type stores gmsh mesh format

TYPE :: mshFormat_
  REAL( DFP ) :: Version = 0.0_DFP
  INTEGER( I4B ) :: FileType = 0, DataSize = 0
  LOGICAL( LGT ) :: isASCII = .FALSE.
  CHARACTER( LEN = 100 ) :: MeshFormat = ""

  CONTAINS
    PROCEDURE, PUBLIC, PASS( Obj ) :: ReadFromFile => fmt_read_file
      !! Read format from a file
    PROCEDURE, PUBLIC, PASS( Obj ) :: WriteToFile => fmt_write_file
      !! Write content to a file
    PROCEDURE, PUBLIC, PASS( Obj ) :: GotoTag => fmt_goto
      !! Goto a tag
    PROCEDURE, PUBLIC, PASS( Obj ) :: Finalize => fmt_deallocatedata
      !! Finalize
END TYPE mshFormat_

PUBLIC :: mshFormat_
TYPE( mshFormat_ ), PUBLIC, PARAMETER :: TypemshFormat = mshFormat_( )

!----------------------------------------------------------------------------
!                                                           mshFormatPointer_
!----------------------------------------------------------------------------

TYPE :: mshFormatPointer_
  CLASS( mshFormat_ ), POINTER :: Ptr => NULL()
END TYPE mshFormatPointer_

PUBLIC :: mshFormatPointer_

!----------------------------------------------------------------------------
!                                                     ReadFromFile@mshFormat
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine reads format from .msh file

!> authors: Dr. Vikas Sharma
!
! This subroutine reads mesh format from .mshfile

MODULE SUBROUTINE fmt_read_file( Obj, mshFile, ierr )
  CLASS( mshFormat_ ), INTENT( INOUT) :: Obj
  TYPE( File_ ), INTENT( INOUT) :: mshFile
  LOGICAL( LGT ), INTENT( INOUT ) :: ierr
END SUBROUTINE fmt_read_file
END INTERFACE

!----------------------------------------------------------------------------
!                                                      WriteToFile@mshFormat
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine writes mesh format to a .msh file

!> authors: Dr. Vikas Sharma
!
! This subroutine writes mesh format to a .msh file

MODULE SUBROUTINE fmt_write_file( Obj, mshFile, Str, EndStr )
  CLASS( mshFormat_ ), INTENT( INOUT ) :: Obj
  TYPE( File_ ), INTENT( INOUT ) :: mshFile
  CHARACTER( LEN = * ), INTENT( IN ), OPTIONAL :: Str, EndStr
END SUBROUTINE fmt_write_file
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@mshFormat
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine display the content of [[mshFormat_]]

MODULE SUBROUTINE fmt_display( Obj, Msg, UnitNo )
  CLASS( mshFormat_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
END SUBROUTINE fmt_display
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE fmt_display
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                          GotoTag@mshFormat
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine search the mesh format tag in the mesh file

!> authors: Dr. Vikas Sharma
!
! This subroutine search the mesh format tag in the mesh file

MODULE SUBROUTINE fmt_goto( Obj, mshFile, ierr )
  CLASS( mshFormat_ ), INTENT( IN ) :: Obj
  TYPE( File_ ), INTENT( INOUT ) :: mshFile
  LOGICAL( LGT ), INTENT( INOUT ) :: ierr
END SUBROUTINE fmt_goto
END INTERFACE

!----------------------------------------------------------------------------
!                                                    DeallocateData@mshFormat
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine clears the content of [[mshFormat_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine clears the content of [[mshFormat_]]

MODULE SUBROUTINE fmt_deallocatedata( Obj )
  CLASS( mshFormat_ ), INTENT( INOUT) :: Obj
END SUBROUTINE fmt_deallocatedata
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE fmt_deallocatedata
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE mshFormat_Class