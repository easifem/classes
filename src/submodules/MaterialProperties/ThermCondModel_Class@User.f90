SUBMODULE( ThermCondModel_Class ) User
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                      UserThermCond_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE UserThermCond_Pointer
  ALLOCATE( Ans )
  Ans%getValue => UserThermCond_getval
END PROCEDURE UserThermCond_Pointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE UserThermCond_getval
  Ans = Obj%ConstThermCondVal
END PROCEDURE UserThermCond_getval

END SUBMODULE User