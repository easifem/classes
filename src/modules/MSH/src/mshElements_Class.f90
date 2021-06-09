MODULE mshElements_Class
  !! This module defines a class to handle elements in mesh file

USE BaseType
USE GlobalData
USE mshFormat_Class

IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                              mshElements_
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This class handles the elements present in the mesh file

TYPE :: mshElements_
  INTEGER( I4B ) :: numElements = 0
  INTEGER( I4B ) :: numEntityBlocks = 0
  INTEGER( I4B ) :: minElementTag = 0
  INTEGER( I4B ) :: maxElementTag = 0
  LOGICAL( LGT ) :: isSparse = .FALSE.

  CONTAINS
    PROCEDURE, PUBLIC, PASS( obj ) :: Finalize => el_DeallocateData
      !! deallocate data
    PROCEDURE, PUBLIC, PASS( obj ) :: GotoTag => el_goto
      !! go to the tag
    PROCEDURE, PUBLIC, PASS( obj ) :: ReadFromFile => el_read_file
      !! Read data form file
    PROCEDURE, PUBLIC, PASS( obj ) :: WriteToFile => el_write_file
      !! Write data to file
    PROCEDURE, PUBLIC, PASS( obj ) :: ReadElementLine => el_read_elem_line
      !! Read element line
    PROCEDURE, PUBLIC, PASS( obj ) :: TotalElements => el_telements_1
      !! total elements
END TYPE mshElements_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PUBLIC :: mshElements_
TYPE( mshElements_ ), PUBLIC, PARAMETER :: TypemshElements = mshElements_( )

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: mshElementsPointer_
  CLASS( mshElements_ ), POINTER :: Ptr => NULL()
END TYPE mshElementsPointer_
PUBLIC :: mshElementsPointer_

!----------------------------------------------------------------------------
!                                                         GotoTag@mshElement
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine go the location of element in mesh file

!> authors: Dr. Vikas Sharma
!
! This subroutine go the location of element in mesh file

MODULE SUBROUTINE el_goto( obj, mshFile, ierr )
  CLASS( mshElements_ ), INTENT( IN ) :: obj
  TYPE( File_ ), INTENT( INOUT ) :: mshFile
  LOGICAL( LGT ), INTENT( INOUT ) :: ierr
END SUBROUTINE el_goto
END INTERFACE

!----------------------------------------------------------------------------
!                                                    ReadFromFile@mshElement
!----------------------------------------------------------------------------

INTERFACE
! This subroutine reads data from a file

!> authors: Dr. Vikas Sharma
!
! This subroutine reads data from a file

MODULE SUBROUTINE el_read_file( obj, mshFile, mshFormat, ierr )
  CLASS( mshElements_ ), INTENT( INOUT ) :: obj
  TYPE( File_ ), INTENT( INOUT ) :: mshFile
  TYPE( mshFormat_ ), INTENT( INOUT ) :: mshFormat
  LOGICAL( LGT ), INTENT( INOUT ) :: ierr
END SUBROUTINE el_read_file
END INTERFACE

!----------------------------------------------------------------------------
!                                                      WriteToFile@mshElement
!----------------------------------------------------------------------------

INTERFACE
! This subroutine writes the data to a file

!> authors: Dr. Vikas Sharma
!
! This subroutine writes the data to a file

MODULE SUBROUTINE el_write_file( obj, mshFile, mshFormat, Str, EndStr )
  CLASS( mshElements_ ), INTENT( INOUT ) :: obj
  TYPE( File_ ), INTENT( INOUT ) :: mshFile
  CHARACTER( LEN = * ), INTENT( IN ), OPTIONAL :: Str, EndStr
  TYPE( mshFormat_ ), INTENT( INOUT ) :: mshFormat
END SUBROUTINE el_write_file
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Display@mshElements
!----------------------------------------------------------------------------

INTERFACE
! This data displays the content of [[mshElements_]]

!> authors: Dr. Vikas Sharma
!
! This data displays the content of [[mshElements_]]

MODULE SUBROUTINE el_display( obj, Msg, UnitNo )
  CLASS( mshElements_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
END SUBROUTINE el_display
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE el_display
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                ReadElementLine@mshElements
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE el_read_elem_line( obj, ElemNum, ElemType, PhysicalId, &
  & GeometryId, MeshPartitionTags, Nptrs, mshFile  )
  CLASS( mshElements_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( INOUT ), OPTIONAL :: ElemNum, ElemType, &
    & PhysicalId, GeometryId
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ), OPTIONAL :: &
    & MeshPartitionTags(:), Nptrs(:)
  TYPE( File_ ), INTENT( INOUT ) :: mshFile
END SUBROUTINE el_read_elem_line
END INTERFACE

!----------------------------------------------------------------------------
!                                                  TotalElements@mshElements
!----------------------------------------------------------------------------

INTERFACE
! This function returns total number of elements

!> authors: Dr. Vikas Sharma
!
! This function returns total number of elements

MODULE PURE FUNCTION el_telements_1( obj ) RESULT( ans )
  CLASS( mshElements_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION el_telements_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 DeallocateData@mshElements
!----------------------------------------------------------------------------

INTERFACE
! This subroutine deallocates the data from obj

!> authors: Dr. Vikas Sharma
!
! This subroutine deallocates the data from obj

MODULE SUBROUTINE el_deallocatedata( obj )
  CLASS( mshElements_ ), INTENT( INOUT) :: obj
END SUBROUTINE el_deallocatedata
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE el_deallocatedata
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

END MODULE mshElements_Class