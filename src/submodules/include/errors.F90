!----------------------------------------------------------------------------
!                                                           AssertError1
!----------------------------------------------------------------------------

SUBROUTINE AssertError1(a, myName, msg)
  LOGICAL(LGT), INTENT(IN) :: a
  CHARACTER(*), INTENT(IN) :: myName
  CHARACTER(*), INTENT(IN) :: msg

  IF (.NOT. a) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                      '[INTERNAL ERROR] :: '//msg)
    RETURN
  END IF

END SUBROUTINE AssertError1

!----------------------------------------------------------------------------
!                                                           AssertError1
!----------------------------------------------------------------------------

SUBROUTINE AssertError2(a, b, myName, msg)
  INTEGER(I4B), INTENT(IN) :: a, b
  CHARACTER(*), INTENT(IN) :: myName
  CHARACTER(*), INTENT(IN) :: msg

  IF (a .NE. b) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
  '[INTERNAL ERROR] :: Size error, '//msg//" found a="//tostring(a)//" b = " &
                      //tostring(b))
    RETURN
  END IF

END SUBROUTINE AssertError2
