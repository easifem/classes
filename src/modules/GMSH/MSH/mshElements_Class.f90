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
    PROCEDURE, PUBLIC, PASS( Obj ) :: Finalize => el_DeallocateData
      !! deallocate data
    PROCEDURE, PUBLIC, PASS( Obj ) :: GotoTag => el_goto
      !! go to the tag
    PROCEDURE, PUBLIC, PASS( Obj ) :: ReadFromFile => el_read_file
      !! Read data form file
    PROCEDURE, PUBLIC, PASS( Obj ) :: WriteToFile => el_write_file
      !! Write data to file
    PROCEDURE, PUBLIC, PASS( Obj ) :: ReadElementLine => el_read_elem_line
      !! Read element line
    PROCEDURE, PUBLIC, PASS( Obj ) :: TotalElements => el_telements_1
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

MODULE SUBROUTINE el_goto( Obj, mshFile, ierr )
  CLASS( mshElements_ ), INTENT( IN ) :: Obj
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

MODULE SUBROUTINE el_read_file( Obj, mshFile, mshFormat, ierr )
  CLASS( mshElements_ ), INTENT( INOUT ) :: Obj
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

MODULE SUBROUTINE el_write_file( Obj, mshFile, mshFormat, Str, EndStr )
  CLASS( mshElements_ ), INTENT( INOUT ) :: Obj
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

MODULE SUBROUTINE el_display( Obj, Msg, UnitNo )
  CLASS( mshElements_ ), INTENT( IN ) :: Obj
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
MODULE SUBROUTINE el_read_elem_line( Obj, ElemNum, ElemType, PhysicalId, &
  & GeometryId, MeshPartitionTags, Nptrs, mshFile  )
  CLASS( mshElements_ ), INTENT( INOUT ) :: Obj
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

MODULE PURE FUNCTION el_telements_1( Obj ) RESULT( Ans )
  CLASS( mshElements_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ) :: Ans
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

MODULE SUBROUTINE el_deallocatedata( Obj )
  CLASS( mshElements_ ), INTENT( INOUT) :: Obj
END SUBROUTINE el_deallocatedata
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE el_deallocatedata
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

END MODULE mshElements_Class