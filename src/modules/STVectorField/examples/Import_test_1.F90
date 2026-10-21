!> author: Vikas Sharma, Ph. D.
! summary: In this example we initiate an instance of vector field by
!          importing data from HDF5 file.
!
! Results:
!
!```txt
! #obj =
! # isInitiated : TRUE
! # name :U
! # fieldType : NORMAL
! # engine :LIS_OMP
! # comm: 0
! # myRank: 0
! # numProcs: 1
! # global_n: 124
! # local_n: 124
! # is: 1
! # ie: 125
! # lis_ptr: 94743042649232
! # domain : ASSOCIATED
! # domains : NOT ALLOCATED
! # tSize : 124
! # # DOF data :
! # Total Physical Variables :1
! # Name : U
! # Space Components : 2
! # Time Components : 2
! # Total Nodes : 31
! # Storage Format : Nodes
! # Value Map :
! --------------
!        1
!       32
!       63
!       94
!      125
! # VAR :U
!  DOF-1 ,   DOF-2 ,   DOF-3 ,   DOF-4 ,
! -------,  -------,  -------,  -------,
! 0.00000,  0.00000,  0.00000,  0.00000,
! 0.00000,  0.00000,  0.00000,  0.00000,
!
! 0.00000,  0.00000,  0.00000,  0.00000,
! 0.00000,  0.00000,  0.00000,  0.00000,
!
! # spaceCompo = 2
! # timeCompo = 2
!```

PROGRAM main
  USE easifemBase
  USE easifemClasses
  TYPE( Domain_ ) :: dom
  TYPE( STVectorField_ ) :: obj
  TYPE( HDF5File_ ) :: meshfile, resultFile
  TYPE( ParameterList_ ) :: param
  INTEGER( I4B ) :: ierr

  CALL FPL_INIT()
  CALL param%initiate()
  CALL resultFile%initiate( filename="./result.h5", mode="READ" )
  CALL resultFile%open()

  !> start creating domain
  CALL meshfile%initiate( filename="./mesh.h5", mode="READ" )
  CALL meshfile%open()
  CALL dom%initiate( hdf5=meshfile, group="" )
  !> end creating domain

  !> start import
  CALL obj%import( hdf5=resultFile, group="/STVectorField1", dom=dom )
  !> end start import

  CALL obj%Display("obj = ")

  CALL obj%Deallocate(); CALL dom%Deallocate()
  CALL meshfile%Deallocate()
  CALL resultFile%Deallocate()
  CALL param%Deallocate(); CALL FPL_FINALIZE()
END PROGRAM main
