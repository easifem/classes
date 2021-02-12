PROGRAM MAIN
  USE OMP_LIB
  IMPLICIT NONE

  REAL, ALLOCATABLE :: Mat2( :, : ), Vec1( : ), Mat1( :, : )
  INTEGER :: tNodes, tDOF, I, tThreads, iThread, &
      & InnerThreads, J
  DOUBLE PRECISION :: t1, t2
  REAL :: Scale

  WRITE( *, * ) "tDOF :: "
  READ( *, * ) tDOF

  WRITE( *, * ) "Outer Threads :: "
  READ( *, * ) tThreads

  !tDOF = 3
  tNodes = 100000000

  Scale = 2.0

  ALLOCATE( Mat2( tNodes, tDOF ), Mat1( tNodes, tDOF ) )
  Mat2 = 2.0; Mat1 = 1.0

  CALL OMP_SET_NESTED( .TRUE. )
  WRITE( *, * ) "isNested :: ", OMP_GET_NESTED( )

  !$ CALL OMP_SET_NUM_THREADS( tThreads )

  !$ t1 = OMP_GET_WTIME( )
  !$omp PARALLEL
  IF( tDOF .EQ. 1 ) THEN
    !$omp DO
    DO I = 1, tNodes
      Mat2( I, 1 ) = Mat2( I, 1 )  + Scale * Mat1( I, 1 )
    END DO
    !$omp END DO
  ELSE IF( tDOF .EQ. 2 ) THEN
    !$omp DO
    DO I = 1, tNodes
      Mat2( I, 1 ) = Mat2( I, 1 )  + Scale * Mat1( I, 1 )
    END DO
    !$omp END DO NOWAIT
    !$omp DO
    DO I = 1, tNodes
      Mat2( I, 2 ) = Mat2( I, 2 )  + Scale * Mat1( I, 2 )
    END DO
    !$omp END DO
  ELSE IF( tDOF .EQ. 3 ) THEN
    !$omp DO
    DO I = 1, tNodes
      Mat2( I, 1 ) = Mat2( I, 1 )  + Scale * Mat1( I, 1 )
    END DO
    !$omp END DO NOWAIT
    !$omp DO
    DO I = 1, tNodes
      Mat2( I, 2 ) = Mat2( I, 2 )  + Scale * Mat1( I, 2 )
    END DO
    !$omp END DO NOWAIT
    !$omp DO
    DO I = 1, tNodes
      Mat2( I, 3 ) = Mat2( I, 3 )  + Scale * Mat1( I, 3 )
    END DO
    !$omp END DO
  ELSE IF( tDOF .EQ. 4 ) THEN
    !$omp DO
    DO I = 1, tNodes
      Mat2( I, 1 ) = Mat2( I, 1 )  + Scale * Mat1( I, 1 )
    END DO
    !$omp END DO NOWAIT
    !$omp DO
    DO I = 1, tNodes
      Mat2( I, 2 ) = Mat2( I, 2 )  + Scale * Mat1( I, 2 )
    END DO
    !$omp END DO NOWAIT
    !$omp DO
    DO I = 1, tNodes
      Mat2( I, 3 ) = Mat2( I, 3 )  + Scale * Mat1( I, 3 )
    END DO
    !$omp END DO NOWAIT
    !$omp DO
    DO I = 1, tNodes
      Mat2( I, 4 ) = Mat2( I, 4 )  + Scale * Mat1( I, 4 )
    END DO
    !$omp END DO
  ELSE IF( tDOF .EQ. 6 ) THEN
    !$omp DO
    DO I = 1, tNodes
      Mat2( I, 1 ) = Mat2( I, 1 )  + Scale * Mat1( I, 1 )
    END DO
    !$omp END DO NOWAIT
    !$omp DO
    DO I = 1, tNodes
      Mat2( I, 2 ) = Mat2( I, 2 )  + Scale * Mat1( I, 2 )
    END DO
    !$omp END DO NOWAIT
    !$omp DO
    DO I = 1, tNodes
      Mat2( I, 3 ) = Mat2( I, 3 )  + Scale * Mat1( I, 3 )
    END DO
    !$omp END DO NOWAIT
    !$omp DO
    DO I = 1, tNodes
      Mat2( I, 4 ) = Mat2( I, 4 )  + Scale * Mat1( I, 4 )
    END DO
    !$omp END DO NOWAIT
    !$omp DO
    DO I = 1, tNodes
      Mat2( I, 5 ) = Mat2( I, 5 )  + Scale * Mat1( I, 5 )
    END DO
    !$omp END DO NOWAIT
    !$omp DO
    DO I = 1, tNodes
      Mat2( I, 6 ) = Mat2( I, 6 )  + Scale * Mat1( I, 6 )
    END DO
    !$omp END DO
    ELSE
    !$omp DO
    DO J = 1, tDOF
      Mat2( :, J ) = Mat2( :, J )  + Scale * Mat1( :, J )
    END DO
    !$omp END  DO
  END IF
  !$omp END PARALLEL
  
  !$ t2 = OMP_GET_WTIME( )

  CALL OMP_SET_NESTED( .FALSE. )
  WRITE( *, * )"is Nested :: ", OMP_GET_NESTED( )
  
  WRITE( * , * ) "tDOF :: ", tDOF
  WRITE( *, * ) "tNodes :: ", tNodes
  WRITE( *, * ) "CPU-TIME :: ", t2-t1
  
END PROGRAM MAIN
