!> author: Vikas Sharma, Ph. D.
! summary: In this example we test the [Set](./Set.md) method.
!
! Results:
!
!```txt
! # VAR :U
!  DOF-1 ,   DOF-2 ,   DOF-3 ,   DOF-4 ,
! -------,  -------,  -------,  -------,
! 0.39222,  0.32577,  0.38798,  0.37918,
! 0.40475,  0.43177,  0.77874,  0.58285,
! 0.01328,  0.98929,  0.60267,  0.96886,
!
! # spaceCompo = 2
! # timeCompo = 2
!
! value =( :, :, 1 ) =
! ---------------------
!  0.392217  0.387975
!  0.325772  0.379176
! value =( :, :, 2 ) =
! ---------------------
!  0.404755  0.778743
!  0.431771  0.582854
! value =( :, :, 3 ) =
! ---------------------
!  0.013282  0.602673
!  0.989292  0.968863
!```

PROGRAM main
  USE easifemBase
  USE easifemClasses
  TYPE( Domain_ ) :: dom
  TYPE( STVectorField_ ) :: obj
  TYPE( HDF5File_ ) :: meshfile
  TYPE( ParameterList_ ) :: param
  INTEGER( I4B ) :: ierr
  INTEGER( I4B ), PARAMETER :: spaceCompo=2_I4B
  INTEGER( I4B ), PARAMETER :: timeCompo=2_I4B
  REAL( DFP ), allocatable :: value(:,:,:)
  CHARACTER(*), PARAMETER :: engine = "NATIVE_SERIAL"
  INTEGER( I4B ) :: tnodes

  ! import domain
  CALL meshfile%initiate( filename="./mesh.h5", mode="READ" )
  CALL meshfile%open()
  CALL dom%initiate( hdf5=meshfile, group="" )
  CALL meshfile%Deallocate()


  ! set parameters for VectorField_
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

  tnodes = dom%getTotalNodes()
  call reallocate(value, spaceCompo, timeCompo, 3)
  call random_number(value)

  call obj%set(value=value(1,1,:), globalNode=arange(1,3,1), spaceCompo=1, timeCompo=1)
  call obj%set(value=value(1,2,:), globalNode=arange(1,3,1), spaceCompo=1, timeCompo=2)
  call obj%set(value=value(2,1,:), globalNode=arange(1,3,1), spaceCompo=2, timeCompo=1)
  call obj%set(value=value(2,2,:), globalNode=arange(1,3,1), spaceCompo=2, timeCompo=2)
  CALL obj%Display("obj = ")
  call display(value, "value = ")

  CALL obj%Deallocate(); CALL dom%Deallocate()
  CALL param%Deallocate(); CALL FPL_FINALIZE()
END PROGRAM main
