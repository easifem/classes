program main
  use basetype
  use basemethod
  use penf_stringify
  use mshType

  type( msh4_ ) :: obj
  logical( LGT ) :: ierr
  type( file_ ) :: afile
  INTEGER( I4B ) :: i, j, k, entityDim, entityTag, numNodesInBlock

  Call obj % initiate( "./", "mesh", ".msh" , 2 )
  ! call display( obj % PhysicalNames % numElements, "numElements" )
  ! call display( obj % PhysicalNames % numNodes, "numNodes" )
  CALL OpenFileToWrite( aFile, "./", "delme", ".md" )
  CALL Display( obj % Format, "Format = " )
  CALL Display( obj % PhysicalNames, "PhysicalNames = ", aFile%UnitNo )
  !
  DO i = 1, SIZE( obj % PointEntities )
      CALL Display( obj % PointEntities( i ), &
          & "## PointEntities( "//TRIM( Str( i, .true. ) )//" )", &
          & aFile%UnitNo )
  END DO
  !
  DO i = 1, SIZE( obj % CurveEntities )
      CALL Display( obj % CurveEntities( i ), &
          & "## CurveEntities( "//TRIM( Str( i, .true. ) )//" )", &
          & aFile%UnitNo )
  END DO
  !
  DO i = 1, SIZE( obj % SurfaceEntities )
      CALL Display( obj % SurfaceEntities( i ), &
          & "## SurfaceEntities( "//TRIM( Str( i, .true. ) )//" )", &
          & aFile%UnitNo )
  END DO
  !
  CALL Display( obj % Nodes, &
      & "## Nodes", aFile % UnitNo )
  !
  CALL Display( obj % Elements, &
      & "## Elements", aFile % UnitNo )
end program main
