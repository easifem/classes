#ifdef DEBUG_VER
IF (.NOT. islocal) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: global nodes are not local nodes')
  RETURN
END IF
#endif
