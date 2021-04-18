MODULE GenericList_Class
  USE GlobalData
  IMPLICIT NONE

  INTEGER( I4B ), ALLOCATABLE :: GenericListData( : )

  PRIVATE
  PUBLIC :: GenericList_, GenericListData, getGenericListData

  !---------------------------------------------------------------------------
  !                                                          GenericList_
  !---------------------------------------------------------------------------

  TYPE :: GenericList_
    PRIVATE
    INTEGER, POINTER :: DATA( : ) => NULL( )
    TYPE( GenericList_ ), POINTER :: Next => NULL( )

    CONTAINS
    PROCEDURE, PUBLIC, PASS( obj ) :: Initiate, DeallocateData, SetData, &
      & GetData, getNextNode, InsertNewNode

  END TYPE GenericList_

  INTERFACE getGenericListData
    MODULE PROCEDURE getData
  END INTERFACE getGenericListData

  !---------------------------------------------------------------------------
  !                                                                  Contains
  !---------------------------------------------------------------------------

  CONTAINS

  !---------------------------------------------------------------------------
  !                                                                 Initiate
  !---------------------------------------------------------------------------

  SUBROUTINE Initiate( obj, Data )

    ! Define intent of dummy variables
    CLASS( GenericList_ ), INTENT( INOUT ), TARGET :: obj
    INTEGER( I4B ), INTENT( IN ), OPTIONAL :: Data( : )

    NULLIFY( obj % Next )

    IF( PRESENT( Data ) ) THEN

      ALLOCATE( obj % Data( SIZE( Data ) ) )
      obj % Data = Data

    ELSE

      NULLIFY( obj % DATA )

    END IF

  END SUBROUTINE Initiate

  !---------------------------------------------------------------------------
  !                                                           DeallocateData
  !---------------------------------------------------------------------------

  SUBROUTINE DeallocateData( obj )

    ! Define intent of dummy variables
    CLASS( GenericList_ ), INTENT( INOUT ), TARGET :: obj

    ! Define internal variables
    CLASS( GenericList_ ), POINTER :: Current, Next


    Current => obj

    DO WHILE( ASSOCIATED( Current ) )
      Next => Current % Next
      IF( ASSOCIATED( Current % Data ) ) THEN
        DEALLOCATE( Current % Data )
        NULLIFY( obj % Data )
      END IF

      DEALLOCATE( Current )
      NULLIFY( Current )
      Current => Next
    END DO

  END SUBROUTINE DeallocateData

  !---------------------------------------------------------------------------
  !                                                                 SetData
  !---------------------------------------------------------------------------

  SUBROUTINE SetData( obj, Data )

    ! Define intent of dummy variables
    CLASS( GenericList_ ), INTENT( INOUT ), TARGET :: obj
    INTEGER( I4B ), INTENT( IN ) :: Data( : )

    IF( ASSOCIATED( obj % Data ) ) THEN
      DEALLOCATE( obj % Data )
      NULLIFY( obj % Data )
    END IF

    ALLOCATE( obj % Data( SIZE( Data ) ) )
    obj % Data = Data

  END SUBROUTINE SetData

  !---------------------------------------------------------------------------
  !                                                                   GetData
  !---------------------------------------------------------------------------

  FUNCTION GetData( obj ) RESULT( Data )

    ! Define intent of dummy variables
    CLASS( GenericList_ ), INTENT( INOUT ), TARGET :: obj
    INTEGER( I4B ), POINTER :: Data( : )

    Data => obj % Data

  END FUNCTION GetData

  !---------------------------------------------------------------------------
  !                                                               getNextNode
  !---------------------------------------------------------------------------

  FUNCTION getNextNode( obj ) RESULT( Next )

    ! Define intent of dummy variables
    CLASS( GenericList_ ), INTENT( INOUT ), TARGET :: obj
    TYPE( GenericList_ ), POINTER :: Next
    Next => obj % Next
  END FUNCTION getNextNode


  !---------------------------------------------------------------------------
  !                                                            InsertNewNode
  !---------------------------------------------------------------------------

  SUBROUTINE InsertNewNode( obj, Data )

    ! Define intent of dummy variables
    CLASS( GenericList_ ), INTENT( INOUT ), TARGET :: obj
    INTEGER( I4B ), INTENT( IN ), OPTIONAL :: Data( : )

    ! Define internal variables
    CLASS( GenericList_ ), POINTER :: Next

    ALLOCATE( Next )

    IF( PRESENT( Data ) ) THEN
      ALLOCATE( Next % Data( SIZE( Data ) ) )
      Next % Data = Data
    ELSE
      NULLIFY( Next % Data )
    END IF

    Next % Next => obj % Next
    obj % Next => Next

  END SUBROUTINE InsertNewNode

END MODULE GenericList_Class