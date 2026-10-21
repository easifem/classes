!> author: Vikas Sharma, Ph. D.
! summary: Set_test_6
!
! Results:
!
!```txt
! # VAR :U
!  DOF-1 ,   DOF-2 ,   DOF-3 ,    DOF-4 ,
! -------,  -------,  -------,  --------,
! 0.00000,  0.00000,  0.00000,  0.078799,
! 0.00000,  0.00000,  0.00000,  0.013620,
! 0.00000,  0.00000,  0.00000,  0.196062,
!
! # spaceCompo = 2
! # timeCompo = 2
!
! value =
! --------
! 0.078799
! 0.013620
! 0.196062
!```

PROGRAM main
  USE easifemBase
  USE easifemClasses
  TYPE( Domain_ ) :: dom
  TYPE( STVectorField_ ) :: obj
  TYPE( ScalarField_ ) :: sfield
  TYPE( HDF5File_ ) :: meshfile
  TYPE( ParameterList_ ) :: param
  INTEGER( I4B ) :: ierr
  INTEGER( I4B ), PARAMETER :: spaceCompo=2_I4B
  INTEGER( I4B ), PARAMETER :: timeCompo=2_I4B
  REAL( DFP ), allocatable :: value(:)
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

  CALL SetScalarFieldParam( &
    & param=param, &
    & fieldType=FIELD_TYPE_NORMAL, &
    & name="U", &
    & engine='NATIVE_SERIAL')
  CALL sfield%initiate( param, dom )

  tnodes = dom%getTotalNodes()
  call reallocate(value, tnodes)
  call random_number(value)

  call sfield%set(value)

  call obj%set(value=sfield, spaceCompo=2, timeCompo=2)
  CALL obj%Display("obj = ")
  call display(value(1:3), "value = ")

  call sfield%deallocate()
  CALL obj%Deallocate(); CALL dom%Deallocate()
  CALL param%Deallocate(); CALL FPL_FINALIZE()
END PROGRAM main
