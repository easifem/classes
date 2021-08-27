

# Linear Solver



## Getting started



### Constructing by initiate

The following code explains how to construct `LinSolver` using `initiate` method



```fortran
  type( LinSolver_ ) :: obj
  type( ParameterList_ ) :: param
  integer( i4b ) :: ierr, tnodes
  call display( "TESTING INITIATE AND DEALLOCATEDATA" )
  call FPL_INIT(); call param%initiate()
  call setLinSolverParam( param=param, solverName=LIS_CG,&
    & preconditionOption=LEFT_PRECONDITION, convergenceIn=convergenceInRes, &
    & convergenceType=relativeConvergence, maxIter=100,relativeToRHS=.TRUE., &
    & KrylovSubspaceSize=20,rtol=1.0D-10,atol=1.0D-10 )
  call obj%initiate(param)
  call obj%Display("LinSolver : ")
  call obj%DeallocateData()
  call param%deallocateData(); call FPL_FINALIZE()
```





### Construction by Import



The template of the file is given below



| Variable             | Data type |                 Value                 |                                                      Comment |
| -------------------- | :-------: | :-----------------------------------: | -----------------------------------------------------------: |
| `engine`             |  String   |            `NATIVE_SERIAL`            | This variable helps us to create correct child of `AbstractLinSolver` class. For other children we have following values reserved  : `NATIVE_OMP`,`NATIVE_MPI`,`PETSC`,`LIS_SERIAL`,`LIS_OMP`,`LIS_MPI` |
| `solverName`         |  String   |                                       |                                           Name of the solver |
| `preconditionOption` |  String   | `LEFT`, `RIGHT`, `LEFT_RIGHT`, `NONE` |                                                              |
| `convergenceIn`      |  String   |        `RESIDUAL`, `SOLUTION`         |                                                              |
| `convergenceType`    |  String   |         `ABSOLUT`, `RELATIVE`         |                                                              |
| `relativeToRHS`      |   Char    |               `T`, `F`                |                 It is used when convergence Type is relative |
| `maxIter`            |    INT    |                                       |                                 Maximum number of iterations |
| `KrylovSubspaceSize` |    INT    |                                       | This is used when GMRES is used, you can set it to 15 to 20. |
| `relativeTolerance`  |   REAL    |                                       |              Tolerance for checking the relative convergence |
| `absoluteTolerance`  |   REAL    |                                       |              Tolerance for checking the absolute convergence |



Following code explains it



Let us generate the `hdf5File_` (hdf5 file) using `export` command as shown below.



```fortran
  type( LinSolver_ ) :: obj
  type( ParameterList_ ) :: param
  type( hdf5File_ ) :: hdf5
  integer( i4b ) :: ierr, tnodes
  call display( "TESTING INITIATE AND EXPORT" )
  call FPL_INIT(); call param%initiate()
  call setLinSolverParam( param=param, solverName=LIS_CG,&
    & preconditionOption=LEFT_PRECONDITION, convergenceIn=convergenceInRes, &
    & convergenceType=relativeConvergence, maxIter=100,relativeToRHS=.TRUE., &
    & KrylovSubspaceSize=20,rtol=1.0D-10,atol=1.0D-10 )
  call obj%initiate(param)
  call hdf5%initiate( filename="./templateLinSolver.hdf5", MODE="NEW" )
  call hdf5%open()
  call obj%export( hdf5, "" )
  call hdf5%close(); call hdf5%deallocateData()
  call obj%DeallocateData()
  call param%deallocateData(); call FPL_FINALIZE()
```



Now let us `import` this information as shown below.



```fortran
  type( LinSolver_ ) :: obj
  type( hdf5File_ ) :: hdf5
  integer( i4b ) :: ierr, tnodes
  call display( "TESTING IMPORT" )
  call hdf5%initiate( filename="./templateLinSolver.hdf5", MODE="READ" )
  call hdf5%open()
  call obj%import(hdf5,"")
  call obj%display("")
  call hdf5%close(); call hdf5%deallocateData()
  call obj%DeallocateData()
```



