
!----------------------------------------------------------------------------
!                                                           FixOrientOfNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_FixOrientOfNptrs
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_FixOrientOfNptrs()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'Nothing to do here.')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_FixOrientOfNptrs
