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
  ! call display( Obj % PhysicalNames % numElements, "numElements" )
  ! call display( Obj % PhysicalNames % numNodes, "numNodes" )
  CALL OpenFileToWrite( aFile, "./", "delme", ".md" )
  CALL Display( Obj % Format, "Format = " )
  CALL Display( Obj % PhysicalNames, "PhysicalNames = ", aFile%UnitNo )
  !
  DO i = 1, SIZE( Obj % PointEntities )
      CALL Display( Obj % PointEntities( i ), &
          & "## PointEntities( "//TRIM( Str( i, .true. ) )//" )", &
          & aFile%UnitNo )
  END DO
  !
  DO i = 1, SIZE( Obj % CurveEntities )
      CALL Display( Obj % CurveEntities( i ), &
          & "## CurveEntities( "//TRIM( Str( i, .true. ) )//" )", &
          & aFile%UnitNo )
  END DO
  !
  DO i = 1, SIZE( Obj % SurfaceEntities )
      CALL Display( Obj % SurfaceEntities( i ), &
          & "## SurfaceEntities( "//TRIM( Str( i, .true. ) )//" )", &
          & aFile%UnitNo )
  END DO
  !
  CALL Display( Obj % Nodes, &
      & "## Nodes", aFile % UnitNo )
  !
  CALL Display( Obj % Elements, &
      & "## Elements", aFile % UnitNo )
end program main
