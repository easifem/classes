PROGRAM MAIN
  USE OMP_LIB
  IMPLICIT NONE

  REAL, ALLOCATABLE :: Vec1( : )
  INTEGER :: I, N, iThread, tThreads
  REAL :: tSum
  DOUBLE PRECISION :: t1, t2

  N = 100000000
  ALLOCATE( Vec1( N ) )
  Vec1 = 1.0
  tSum = REAL( N )

  WRITE( *, * ) "tThreads :: "
  READ( *, * ) tThreads

  !$ CALL OMP_SET_NUM_THREADS( tThreads )
  !$ t1 = OMP_GET_WTIME( )
  !$omp PARALLEL
  !$omp DO REDUCTION( +:tSum )
  DO I = 1, N
    tSum = tSum + ABS( Vec1( I ) )**2
  END DO
  !$omp END DO
  !$omp END PARALLEL
  !$ t2 = OMP_GET_WTIME( )
  
  WRITE( *, * ) tSum / REAL( N )
  WRITE( *, * ) "N        :: ", N
  WRITE( *, * ) "CPU-TIME :: ", t2-t1
END PROGRAM MAIN
