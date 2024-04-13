SUBMODULE(gmshMesh_Class) BufferMethods
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Generate
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_generate
  TYPE( String ) :: Entry
  CHARACTER( LEN = 1 ) :: semi

  semi = ";"

  IF( .NOT. ASSOCIATED( obj % buffer ) ) THEN
    ALLOCATE( obj % buffer )
  END IF

  Entry = "Mesh " // trim( str( dim, no_sign=.true. ) ) // semi
  CALL Append( obj % buffer, Entry )
END PROCEDURE mesh_generate

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_write
  INTEGER( I4B ) :: ii
  IF( ASSOCIATED( obj % buffer ) ) THEN
    DO ii = 1, obj % buffer % tLine
      WRITE( UnitNo, "(DT)" ) obj % buffer % Line( ii ) % Ptr
    END DO
  END IF
END PROCEDURE mesh_write

END SUBMODULE BufferMethods