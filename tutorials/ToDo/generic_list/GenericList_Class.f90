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
    PROCEDURE, PUBLIC, PASS( Obj ) :: Initiate, DeallocateData, SetData, &
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

  SUBROUTINE Initiate( Obj, Data )

    ! Define intent of dummy variables
    CLASS( GenericList_ ), INTENT( INOUT ), TARGET :: Obj
    INTEGER( I4B ), INTENT( IN ), OPTIONAL :: Data( : )

    NULLIFY( Obj % Next )

    IF( PRESENT( Data ) ) THEN

      ALLOCATE( Obj % Data( SIZE( Data ) ) )
      Obj % Data = Data

    ELSE

      NULLIFY( Obj % DATA )

    END IF

  END SUBROUTINE Initiate

  !---------------------------------------------------------------------------
  !                                                           DeallocateData
  !---------------------------------------------------------------------------

  SUBROUTINE DeallocateData( Obj )

    ! Define intent of dummy variables
    CLASS( GenericList_ ), INTENT( INOUT ), TARGET :: Obj

    ! Define internal variables
    CLASS( GenericList_ ), POINTER :: Current, Next


    Current => Obj

    DO WHILE( ASSOCIATED( Current ) )
      Next => Current % Next
      IF( ASSOCIATED( Current % Data ) ) THEN
        DEALLOCATE( Current % Data )
        NULLIFY( Obj % Data )
      END IF

      DEALLOCATE( Current )
      NULLIFY( Current )
      Current => Next
    END DO

  END SUBROUTINE DeallocateData

  !---------------------------------------------------------------------------
  !                                                                 SetData
  !---------------------------------------------------------------------------

  SUBROUTINE SetData( Obj, Data )

    ! Define intent of dummy variables
    CLASS( GenericList_ ), INTENT( INOUT ), TARGET :: Obj
    INTEGER( I4B ), INTENT( IN ) :: Data( : )

    IF( ASSOCIATED( Obj % Data ) ) THEN
      DEALLOCATE( Obj % Data )
      NULLIFY( Obj % Data )
    END IF

    ALLOCATE( Obj % Data( SIZE( Data ) ) )
    Obj % Data = Data

  END SUBROUTINE SetData

  !---------------------------------------------------------------------------
  !                                                                   GetData
  !---------------------------------------------------------------------------

  FUNCTION GetData( Obj ) RESULT( Data )

    ! Define intent of dummy variables
    CLASS( GenericList_ ), INTENT( INOUT ), TARGET :: Obj
    INTEGER( I4B ), POINTER :: Data( : )

    Data => Obj % Data

  END FUNCTION GetData

  !---------------------------------------------------------------------------
  !                                                               getNextNode
  !---------------------------------------------------------------------------

  FUNCTION getNextNode( Obj ) RESULT( Next )

    ! Define intent of dummy variables
    CLASS( GenericList_ ), INTENT( INOUT ), TARGET :: Obj
    TYPE( GenericList_ ), POINTER :: Next
    Next => Obj % Next
  END FUNCTION getNextNode


  !---------------------------------------------------------------------------
  !                                                            InsertNewNode
  !---------------------------------------------------------------------------

  SUBROUTINE InsertNewNode( Obj, Data )

    ! Define intent of dummy variables
    CLASS( GenericList_ ), INTENT( INOUT ), TARGET :: Obj
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

    Next % Next => Obj % Next
    Obj % Next => Next

  END SUBROUTINE InsertNewNode

END MODULE GenericList_Class