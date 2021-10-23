#PLPLOT
IF( ${PROJECT_NAME} MATCHES "easifemBase" )
  OPTION( USE_PLPLOT OFF )
  IF( USE_PLPLOT )
    LIST( APPEND TARGET_COMPILE_DEF "-DUSE_PLPLOT" )
    IF( UNIX )
      IF(APPLE)
        SET(PLPLOT_INCLUDE_DIR
          "/usr/local/lib/fortran/modules/plplot" )
        SET(PLPLOT_LIBRARY
          "/usr/local/lib/libplplot.dylib" )
        SET(PLPLOT_FORTRAN_LIBRARY
          "/usr/local/lib/libplplotfortran.dylib" )
      ELSE()
        SET(PLPLOT_INCLUDE_DIR
          "$ENV{EASIFEM_EXTPKGS}/lib/fortran/modules/plplot" )
        SET(PLPLOT_LIBRARY
          "$ENV{EASIFEM_EXTPKGS}/lib/libplplot.so" )
        SET(PLPLOT_FORTRAN_LIBRARY
          "$ENV{EASIFEM_EXTPKGS}/lib/libplplotfortran.so" )
      ENDIF()
    ENDIF()
    TARGET_LINK_LIBRARIES(
      ${PROJECT_NAME}
      PUBLIC
      ${PLPLOT_LIBRARY}
      ${PLPLOT_FORTRAN_LIBRARY}
      )
    TARGET_INCLUDE_DIRECTORIES(
      ${PROJECT_NAME}
      PUBLIC
      ${PLPLOT_INCLUDE_DIR}
      )
    MESSAGE( STATUS "PLPLOT_LIBRARY : ${PLPLOT_LIBRARY}" )
    MESSAGE( STATUS "PLPLOT_FORTRAN_LIBRARY : ${PLPLOT_FORTRAN_LIBRARY}" )
    MESSAGE( STATUS "PLPLOT_INCLUDE_DIR : ${PLPLOT_INCLUDE_DIR}" )
  ELSE()
    MESSAGE( STATUS "NOT USING PLPLOT LIBRARIES" )
  ENDIF()
ENDIF()