SUBMODULE( gmshMesh_Class ) BufferMethods
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

  IF( .NOT. ASSOCIATED( Obj % buffer ) ) THEN
    ALLOCATE( Obj % buffer )
  END IF

  Entry = "Mesh " // trim( str( dim, no_sign=.true. ) ) // semi
  CALL Append( Obj % buffer, Entry )
END PROCEDURE mesh_generate

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_write
  INTEGER( I4B ) :: ii
  IF( ASSOCIATED( Obj % buffer ) ) THEN
    DO ii = 1, Obj % buffer % tLine
      WRITE( UnitNo, "(DT)" ) Obj % buffer % Line( ii ) % Ptr
    END DO
  END IF
END PROCEDURE mesh_write

END SUBMODULE BufferMethods