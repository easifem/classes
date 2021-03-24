program main
  use omp_lib
  use globaldata
  use io
  use string_class
  use groupdof_class

  type( groupdof_ ) :: obj1, obj2
  integer( i4b ) :: iNode, iDOF, tDOF, &
      & tThreads, iThread, iStart, iEnd, iWidth, maxtNodes
  integer( i4b ), allocatable :: tNodes( : )
  real( dfp ) :: dotp, t1, t2, final_dp

  
  write( *, * ) "tThreads :: "
  read( *, * ) tThreads
  
  tNodes = [100000000]
  call obj1 % initiate( tNodes, [String("U"), String("V")], [2], [2] )
  call obj2 % initiate( obj1 )
  obj1 = 1.0_dfp
  obj2 = 1.0_dfp
  final_dp = 0.0

  write( *, * ) "Parallel region starting"
  call omp_set_num_threads( tThreads )
  t1 = omp_get_wtime( )
  !$omp parallel
  CALL Obj1 % getDotProduct( Obj2, dotp )
  !$omp end parallel
  t2 = omp_get_wtime( )
  write( *, * ) "Parallel region ended"

  final_dp = dotp
  
  write( *, * ) "N :: ", tNodes
  write( *, * ) "cpu :: ", t2 - t1
  write( *, * ) "dotp :: ", final_dp
  
end program main
