MODULE mshNodes_Class
  !! This module implements a class for handling nodes in mesh file
USE BaseType
USE GlobalData
USE mshFormat_Class

IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                                 mshNodes_
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This class is defined to handle the nodes in mesh file

TYPE :: mshNodes_
  INTEGER( I4B ) :: numNodes = 0
    !! number of nodes
  INTEGER( I4B ) :: numEntityBlocks = 0 !msh4
    !! number of entity blocks
  INTEGER( I4B ) :: minNodeTag = 0 !msh4
    !! minimum node number
  INTEGER( I4B ) :: maxNodeTag = 0 !msh4
    !! maximum node number
  LOGICAL( LGT ) :: isSparse = .FALSE. !msh4
    !! isSparse

  CONTAINS
    PROCEDURE, PUBLIC, PASS( Obj ) :: Finalize => n_deallocateData
      !! DeallocateData From the object
    PROCEDURE, PUBLIC, PASS( Obj ) :: GotoTag => n_goto
      !! Go to the node tag in mesh file
    PROCEDURE, PUBLIC, PASS( Obj ) :: ReadFromFile => n_read_file
      !! read content from file
    PROCEDURE, PUBLIC, PASS( Obj ) :: WriteToFile => n_write_file
      !! write data to file
END TYPE mshNodes_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PUBLIC :: mshNodes_
TYPE( mshNodes_ ), PUBLIC, PARAMETER :: TypeMshNodes = mshNodes_( )

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: mshNodesPointer_
  CLASS( mshNodes_ ),  POINTER :: Ptr => NULL( )
END TYPE mshNodesPointer_
PUBLIC :: mshNodesPointer_

!----------------------------------------------------------------------------
!                                                         GotoNodes@mshNodes
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine go to the position where nodes are defined

!> authors: Dr. Vikas Sharma
!
! This subroutine go to the position where nodes are defined

MODULE SUBROUTINE n_goto( Obj, mshFile, ierr )
  CLASS( mshNodes_ ), INTENT( IN ) :: Obj
  TYPE( File_ ), INTENT( INOUT ) :: mshFile
  LOGICAL( LGT ), INTENT( INOUT ) :: ierr
END SUBROUTINE n_goto
END INTERFACE

!----------------------------------------------------------------------------
!                                                      ReadFromFile@mshNodes
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine read data from mesh file

!> authors: Dr. Vikas Sharma
!
! This subroutine read data from mesh file

MODULE SUBROUTINE n_read_file( Obj, mshFile, mshFormat, ierr )
  CLASS( mshNodes_ ), INTENT( INOUT ) :: Obj
  TYPE( File_ ), INTENT( INOUT ) :: mshFile
  TYPE( mshFormat_ ), INTENT( INOUT ) :: mshFormat
  LOGICAL( LGT ), INTENT( INOUT ) :: ierr
END SUBROUTINE n_read_file
END INTERFACE

!----------------------------------------------------------------------------
!                                                       WriteToFile@mshNodes
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine write data to a file

!> authors: Dr. Vikas Sharma
!
! This subroutine writes data to a file

MODULE SUBROUTINE n_write_file( Obj, mshFile, mshFormat, Str, EndStr )
  CLASS( mshNodes_ ), INTENT( INOUT ) :: Obj
  TYPE( File_ ), INTENT( INOUT ) :: mshFile
  CHARACTER( LEN = * ), INTENT( IN ), OPTIONAL :: Str, EndStr
  TYPE( mshFormat_ ), INTENT( INOUT ) :: mshFormat
END SUBROUTINE n_write_file
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Display@mshNodes
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine display contents of [[mshNodes_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine displays content of [[mshNodes_]]

MODULE SUBROUTINE n_display( Obj, Msg, UnitNo )
  CLASS( mshNodes_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
END SUBROUTINE n_display
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE n_display
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                    DeallocateData@mshNodes
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine deallocate the data form the instance

!> authors: Dr. Vikas Sharma
!
! This subroutine deallocate the data from instance

MODULE SUBROUTINE n_deallocatedata( Obj )
  CLASS( mshNodes_ ), INTENT( INOUT) :: Obj
END SUBROUTINE n_deallocatedata
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE n_deallocatedata
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

END MODULE mshNodes_Class