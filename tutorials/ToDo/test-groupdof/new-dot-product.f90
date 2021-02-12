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
  call obj1 % initiate( tNodes, [String("U"), String( "V" ) ], [2], [2] )
  call obj2 % initiate( obj1 )
  obj1 = 1.0_dfp
  obj2 = 1.0_dfp
  final_dp = 0.0

  write( *, * ) "Parallel region starting"
  call omp_set_num_threads( tThreads )
  t1 = omp_get_wtime( )
  !$omp parallel &
  !$omp private( idof, iNode, iStart, iEnd, iwidth, maxtNodes, tDOF, tThreads ) &
  !$omp reduction( + : dotp )
  maxtNodes = MAXVAL( Obj1 % tNodes )
  tDOF = SIZE( Obj1 % DOF )
  iThread = omp_get_thread_num( )
  tThreads = omp_get_num_threads( )
  iWidth =  maxtNodes / tThreads

  iStart = iThread * iWidth + 1
  iEnd = ( iThread + 1 )*iWidth
  if( iThread .eq. tThreads-1 )  iEnd = maxtNodes 
  
  dotp  = 0.0_dfp
  
  do iNode = iStart, iEnd
    do idof = 1, tDOF
      if( iNode .LE. Obj1 % tNodes( idof ) ) then
        dotp = dotp + Obj1 % DOF( idof ) % Val( iNode ) &
            & * Obj2 % DOF( idof ) % Val( iNode )
      end if
    end do
  end do
  
  !$omp end parallel
  t2 = omp_get_wtime( )
  write( *, * ) "Parallel region ended"

  final_dp = dotp
  
  write( *, * ) "N :: ", tNodes
  write( *, * ) "cpu :: ", t2 - t1
  write( *, * ) "dotp :: ", final_dp
  
end program main
