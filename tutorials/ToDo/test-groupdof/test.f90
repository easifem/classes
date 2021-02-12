PROGRAM MAIN
  ! USE OMP_LIB
  USE GLOBALDATA
  USE IO
  USE GROUPDOF_CLASS
  USE STRING_CLASS
  USE File_Class
  USE UTILITY, ONLY : INT2STR

  TYPE( GROUPDOF_ ) :: Obj, Obj2, Obj3
  
  INTEGER( I4B ) :: tNodes, tDOF, tThreads, iThread, &
      & DummyInt, I, a, iDOF, iNode
  
  REAL( DFP ) :: t1, t2, DummyReal, DummyReal2
  
  INTEGER( I4B ), ALLOCATABLE :: IndxVec( : )
  
  CHARACTER( LEN = 100 ) :: Str
  
  TYPE( String_ ) :: Names( 3 )

  REAL( DFP ), ALLOCATABLE :: Ue( :, :, : ), U0e( :, : ), Vec1( : )

  !.....................................................................

  tNodes = 10

  ! CALL Obj % Initiate( [String("U"), String("V")], [2,2], [2,2] )
  ! CALL Obj % Display( )

  Obj = GroupDOF( tNodes = [tNodes], PhysicalNames = [String("v")], &
    & SpaceComponents=[2], TimeComponents = [2] )
  Obj = 1.0_DFP

  CALL Obj % WriteTimeHistory(  "./", [String( "vx"), String( "vy" ) ], [3,4], [1,5,6], 1.d0 )
  CALL Obj % WriteTimeHistory(  "./", [String( "vx"), String( "vy" ) ], [3,4], [1,5,6], 2.d0 )
  CALL Obj % WriteTimeHistory(  "./", [String( "vx"), String( "vy" ) ], [3,4], [1,5,6], 3.d0 )

END PROGRAM MAIN
