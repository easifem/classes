## Matrix Field



`MatrixField_` is a child of `AbstractMatrixField_`.

It uses NATIVE SERIAL implementation.



## Getting started



### Constructing using initiate method



```fortran
  type( domain_ ) :: dom
  type( MatrixField_ ) :: obj
  type( HDF5File_ ) :: meshfile, hdf5
  type( ParameterList_ ) :: param
  integer( i4b ) :: ierr, tnodes
  call display( "TESTING INITIATE AND DEALLOCATEDATA" )
  CALL FPL_INIT()
  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call dom%initiate( meshfile )
  call meshfile%close()
  call meshfile%deallocateData()
  tnodes = dom%getTotalNodes()
  call param%initiate()
  call setMatrixFieldParam( param, "K", "UNSYM", 3, 2, FIELD_TYPE_NORMAL )
  call obj%initiate( param, dom )
  CALL hdf5%initiate(filename="./matrixField.h5", mode="NEW" )
  CALL hdf5%open()
  CALL obj%export(hdf5=hdf5,group='')
  CALL hdf5%close()
  CALL hdf5%deallocateData()
  call obj%deallocateData()
  call dom%deallocateData()
  call param%deallocateData()
  call FPL_FINALIZE()
```



 



### Constructing by import



Following is the template



| Variable     | Data type | Value    | Comment                                                      |
| ------------ | --------- | -------- | ------------------------------------------------------------ |
| `name`       | String    |          |                                                              |
| `fieldType`  | String    |          |                                                              |
| `matrixProp` | String    |          |                                                              |
| `spaceCompo` |           |          |                                                              |
| `timeCompo`* |           |          |                                                              |
| `restart`*   | CHAR      | `T`, `F` | The default value is False. If it is true then it represents that we are restarting the simulation, in this case more information are necessary. |
|              |           |          |                                                              |

