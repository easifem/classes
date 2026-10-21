!> author: Vikas Sharma, Ph. D.
! summary: Initiate_test_1
!
! Results:
!
!```txt
! #obj =
! # isInitiated : TRUE
! # name :U
! # fieldType : NORMAL
! # engine :NATIVE_SERIAL
! # comm: 0
! # myRank: 0
! # numProcs: 1
! # global_n: 62
! # local_n: 62
! # is: 0
! # ie: 0
! # lis_ptr: 0
! # domain : ASSOCIATED
! # domains : NOT ALLOCATED
! # tSize : 62
! # # DOF data :
! # Total Physical Variables :1
! # Name : U
! # Space Components : 2
! # Time Components : 1
! # Total Nodes : 31
! # Storage Format : Nodes
! # Value Map :
! --------------
!        1
!       32
!       63
! # VAR :U
!  DOF-1 ,   DOF-2 ,
! -------,  -------,
! 0.00000,  0.00000,
! 0.00000,  0.00000,
! 0.00000,  0.00000,
! 0.00000,  0.00000,
! 0.00000,  0.00000,
! .
! .
! .
! 0.00000,  0.00000,
! # spaceCompo = 2
!```

PROGRAM main
  USE easifemBase
  USE easifemClasses
  TYPE( Domain_ ) :: dom
  TYPE( STVectorField_ ) :: obj
  TYPE( HDF5File_ ) :: meshfile, resultFile
  TYPE( ParameterList_ ) :: param
  INTEGER( I4B ) :: ierr
  INTEGER( I4B ), PARAMETER :: spaceCompo=2_I4B
  INTEGER( I4B ), PARAMETER :: timeCompo=2_I4B
  CHARACTER(*), PARAMETER :: engine = "NATIVE_SERIAL"

  ! import domain
  CALL meshfile%initiate( filename="./mesh.h5", mode="READ" )
  CALL meshfile%open()
  CALL dom%initiate( hdf5=meshfile, group="" )
  CALL meshfile%Deallocate()

  ! set parameters for STVectorField_
  CALL FPL_INIT()
  CALL param%initiate()
  CALL SetSTVectorFieldParam( &
   & param=param, &
    & fieldType=FIELD_TYPE_NORMAL, &
    & name="U", &
    & engine=engine, &
    & spaceCompo=spaceCompo, &
    & timeCompo=timeCompo)

  CALL obj%initiate( param, dom )

  CALL obj%Display("obj = ")

  CALL resultFile%initiate( filename="./result.h5", mode="NEW" )
  CALL resultFile%open()
  CALL obj%export( hdf5=resultFile, group="/STVectorField1")

  CALL obj%Deallocate(); CALL dom%Deallocate()
  CALL resultFile%Deallocate()
  CALL param%Deallocate(); CALL FPL_FINALIZE()
END PROGRAM main
