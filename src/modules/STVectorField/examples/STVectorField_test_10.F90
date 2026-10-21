!> author: Vikas Sharma, Ph. D.
! summary: In this example we test the [Set](./Set.md) method.
!
! Results:
!
!```txt
! # VAR :U
!  DOF-1 ,   DOF-2 ,   DOF-3 ,   DOF-4 ,
! -------,  -------,  -------,  -------,
! 0.46750,  0.55129,  0.07768,  0.45851,
! 0.81421,  0.06531,  0.97146,  0.55950,
! 0.07182,  0.96061,  0.15547,  0.89446,
!
! # spaceCompo = 2
! # timeCompo = 2
!
! value =( :, :, 1 ) =
! ---------------------
!  0.467503  0.077684
!  0.551291  0.458512
! value =( :, :, 2 ) =
! ---------------------
!  0.814213  0.971461
!  0.065310  0.559495
! value =( :, :, 3 ) =
! ---------------------
!  0.071824  0.155473
!  0.960612  0.894463
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

  call obj%set(value=value, globalNode=arange(1,3,1))
  CALL obj%Display("obj = ")
  call display(value, "value = ")

  CALL obj%Deallocate(); CALL dom%Deallocate()
  CALL param%Deallocate(); CALL FPL_FINALIZE()
END PROGRAM main
