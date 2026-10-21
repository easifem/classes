!> author: Vikas Sharma, Ph. D.
! summary: Get_test_1
!
! Results:
!
!```txt
! set value(:,:,3) =
! -------------------
! 0.147476  0.747876
! 0.171333  0.127092
!
! get value(:,:,3) =
! -------------------
!      0.147476
!      0.171333
!      0.747876
!      0.127092
!
! set value(1,1,:) =
! -------------------
!      0.241280
!      0.812112
!      0.498660
!
! get value =
! ------------
!   0.241280
!   0.812112
!   0.498660
!
!      set value(2,:,1:3) =
! ----------------------------
! 0.900714  0.183391  0.119554
! 0.649540  0.061109  0.117385
!
! get value =
! ------------
!   0.900714
!   0.649540
!   0.183391
!   0.061109
!   0.119554
!   0.117385
! .
! .
! .
!
!
!     set value(:,2,1:3) =
! ----------------------------
! 0.965499  0.382799  0.656891
! 0.646576  0.781357  0.206606
! get value =
! ------------
!   0.965499
!   0.646576
!   0.382799
!   0.781357
!   0.656891
!   0.206606
! .
! .
! .
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
  REAL( DFP ), ALLOCATABLE :: svalue(:,:,:)
  REAL( DFP ), ALLOCATABLE :: value(:)
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

  call reallocate(svalue, spaceCompo, timeCompo, tnodes)
  call random_number(svalue)
  call obj%set(value=svalue)

  call obj%get(value=value, globalNode=3)
  call display(svalue(:,:,3), "set value(:,:,3) = ")
  call display(value, "get value = ")

  call obj%get(value=value, spaceCompo=1, timeCompo=1)
  call display(svalue(1,1,:), "set value(1,1,:) = ")
  call display(value, "get value = ")

  call obj%get(value=value, spaceCompo=2)
  call display(svalue(2,:,1:3), "set value(2,:,:) = ")
  call display(value, "get value = ")


  call obj%get(value=value, timeCompo=2)
  call display(svalue(:,2,1:3), "set value(:,2,1:3) = ")
  call display(value, "get value = ")

  CALL obj%Deallocate(); CALL dom%Deallocate()
  CALL param%Deallocate(); CALL FPL_FINALIZE()
END PROGRAM main
