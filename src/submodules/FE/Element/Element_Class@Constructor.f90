SUBMODULE( Element_Class ) Constructor
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE s_display_Obj
  INTEGER( I4B ) :: I
  !
  IF( PRESENT( UnitNo ) ) THEN
    I = UnitNo
  ELSE
    I = stdout
  END IF
  !
  IF( LEN_TRIM( Msg ) .NE. 0 ) THEN
    WRITE( I, "(A)" ) TRIM( Msg )
  END IF
  !
  CALL Blanklines( NOL = 1, UnitNo = I )
  WRITE( I, "(A)" ) "  TYPE :: ELEMENT_"
  WRITE( I, "(A, I4)" ) "  MAT-TYPE :: ", Obj % Mat_Type
  CALL Display( Obj % Nptrs, "  NPTRS :: ")
  CALL Blanklines( NOL = 1, UnitNo = I )
  !
  IF( PRESENT( FullDisp ) ) THEN
    IF( FullDisp ) THEN
      IF( ASSOCIATED( Obj % RefElem ) ) THEN
        CALL Display( Obj % RefElem, "### Reference Element", I )
      END IF
    END IF
  ELSE
    Return
  END IF
  !
END PROCEDURE s_display_Obj

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE m_display_obj
  INTEGER( I4B ) :: i
  LOGICAL :: full

  if( present( UnitNo ) ) then
    i = UnitNo
  else
    i = stdout
  end if

  if( PRESENT( FullDisp ) ) then
    full = FullDisp
  else
    full = .false.
  end if

  CALL Display( Obj = Obj, Msg = Msg, UnitNo = i, FullDisp = Full )
END PROCEDURE m_display_obj

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE m_Initiate_obj
  Obj % Nptrs = Nptrs
  Obj % Mat_Type = Mat_Type
  Obj % RefElem => RefElem
END PROCEDURE m_Initiate_obj


MODULE PROCEDURE m_initiate_from_obj
  Obj % Nptrs = AnotherObj % Nptrs
  Obj % MAT_Type = AnotherObj % Mat_Type
  Obj % RefElem => AnotherObj % RefElem
END PROCEDURE m_initiate_from_obj

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE s_Initiate_obj
  Obj % Nptrs = Nptrs
  Obj % Mat_Type = Mat_Type
  Obj % RefElem => RefElem
END PROCEDURE s_Initiate_obj

!------------------------------------------------------------------------------
!                                                               Element
!------------------------------------------------------------------------------

MODULE PROCEDURE Constructor1
  CALL Obj % Initiate( Nptrs = Nptrs, Mat_Type = Mat_Type, RefElem = RefElem )
END PROCEDURE Constructor1

MODULE PROCEDURE Constructor2
  Obj % Mat_Type = -1
  Obj % Nptrs = [-1]
  Obj % RefElem => NULL( )
END PROCEDURE Constructor2

MODULE PROCEDURE Constructor3
  CALL Obj % Initiate( AnotherObj )
END PROCEDURE Constructor3

!------------------------------------------------------------------------------
!                                                               DeallocateData
!------------------------------------------------------------------------------

MODULE PROCEDURE Deallocate_Data

  IF( ALLOCATED( Obj % Nptrs ) ) DEALLOCATE( Obj % Nptrs )
  Obj % MAT_Type = 0
  Obj % RefElem => NULL( )

END PROCEDURE Deallocate_Data

!------------------------------------------------------------------------------
!                                                              isBoundaryElement
!------------------------------------------------------------------------------

MODULE PROCEDURE m_isBoundaryElement
  IF( NSD .NE. Obj % RefElem % XiDimension ) THEN
    Ans = .TRUE.
  ELSE
    Ans = .FALSE.
  END IF
END PROCEDURE m_isBoundaryElement

!------------------------------------------------------------------------------
!                                                                     f_getNprs
!------------------------------------------------------------------------------

MODULE PROCEDURE m_getNptrs
  IF( ALLOCATED( Obj % Nptrs ) ) THEN
    Nptrs = Obj % Nptrs
  ELSE
    ALLOCATE( Nptrs( 0 ) )
  END IF
END PROCEDURE m_getNptrs

!------------------------------------------------------------------------------
!                                                                     setNptrs
!------------------------------------------------------------------------------

MODULE PROCEDURE m_setNptrs
  Obj % Nptrs = Nptrs
END PROCEDURE m_setNptrs

!----------------------------------------------------------------------------
!                                                            setMaterialType
!----------------------------------------------------------------------------

MODULE PROCEDURE setMaterialType_1
  Obj % Mat_Type = MatType
END PROCEDURE setMaterialType_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Constructor
