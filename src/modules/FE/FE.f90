MODULE FE
USE Element_Class
USE FacetElement_Class

PUBLIC

CONTAINS

!----------------------------------------------------------------------------
!                                                              getFEPointer
!----------------------------------------------------------------------------

FUNCTION getFEPointer( Obj, Nptrs, Mat_Type, RefElem ) RESULT( Ans )
  USE BaseType
  USE GlobalData

  ! Define internal variable
  CLASS( Element_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN) :: Nptrs( : ), Mat_Type
  CLASS( ReferenceElement_ ), TARGET, INTENT( INOUT ) :: RefElem
  CLASS( Element_ ), POINTER :: Ans
  !
  SELECT TYPE( Obj )
  TYPE IS ( Element_ )
    Ans => Element_Pointer( Nptrs, Mat_Type, RefElem )
  TYPE IS ( FacetElement_ )
    Ans => FacetElement_Pointer( Nptrs, Mat_Type, RefElem )
  END SELECT
  !
END FUNCTION getFEPointer

END MODULE FE
