#ifdef DEBUG_VER

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
          '[INTERNAL ERROR] :: Either VectorField object is not initiated'// &
                    " or, lis_ptr is not available")
  RETURN
END IF

#endif
