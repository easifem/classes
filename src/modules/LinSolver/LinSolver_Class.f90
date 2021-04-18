MODULE LinSolver_Class
  !! This module defines an abstract class for a linear solver
USE GlobalData
USE BaseType

#include "lisf.h"

IMPLICIT NONE

PRIVATE

INTEGER( I4B ), PARAMETER, PUBLIC :: &
  & lis_cg = 1, &
  & lis_bcg=2,  lis_bicg = 2, &
  & lis_cgs = 3, &
  & lis_bcgstab=4, lis_bicgstab = 4, &
  & lis_bcgstabl = 5, lis_bicgstabl = 5, &
  & lis_gpbicg = 6, &
  & lis_tfqmr=7, &
  & lis_omn = 8, lis_fom=8, lis_orthomin = 8, &
  & lis_gmres=9, lis_gmr = 9, &
  & lis_jacobi = 10, &
  & lis_gs = 11, &
  & lis_sor = 12, &
  & lis_bicgsafe = 13, &
  & lis_cr = 14, &
  & lis_bicr = 15, &
  & lis_crs = 16, &
  & lis_bicrstab = 17, &
  & lis_gpbicr = 18, &
  & lis_bicrsafe = 19, &
  & lis_fgmres=20, &
  & lis_idrs = 21, &
  & lis_idr1 = 22, &
  & lis_minres = 23, &
  & lis_cocg = 24, &
  & lis_cocr = 25, &
  & lis_cgnr=26, lis_cgn = 26, &
  & lis_dbcg=27, &
  & lis_dqgmres=28

INTEGER( I4B ), PARAMETER, PUBLIC :: &
  & p_none = 0, &
  & p_jacobi = 1, &
  & p_iluk = 2, &
  & p_ssor = 3, &
  & p_hybrid = 4, &
  & p_is = 5, &
  & p_sainv = 6, &
  & p_saamg = 7, &
  & p_iluc = 8, &
  & p_ilut = 9, &
  & p_ilutp = 10, &
  & p_ilud = 11, &
  & p_iludp = 12, &
  & p_ilu0 = 13

!----------------------------------------------------------------------------
!                                                                 Linsolver_
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! [[linsolver_]] is an abstract class for solving system of linear equation
!
! @note
! It is important to node that [[linsolver_]] is created to build an
! interface between `EASIFEM` library and other existing open-source
! and powerful linear solver libraries.
! @endnote
!
!### Usage
!
! ```fortran
!	CALL obj % Initiate( obj, SolverName, MaxIter, SolverName &
!    & <, diagScale, ipar, fpar> )
! CALL obj % setPrecondition( obj, precondtype <,ipar, fpar> )
! CALL obj % setSparsity( From )
! CALL obj % setDirichletBCNodes( Nptrs, dofs )
! CALL obj % setMatrix( From )
! CALL obj % solve( sol, rhs )
! CALL obj % Display( msg <,unitno > )
! CALL obj % writeResidueHistory( path, prefix, fmt, iter )
! CALL obj % DeallocateData( )
! ```

TYPE, ABSTRACT :: LinSolver_
  INTEGER( I4B ) :: solverName = 0
    !! Solver name
  INTEGER( I4B ) :: ierr = 0
    !! error code returned by the solver
  INTEGER( I4B ) :: tdof = 1
    !! Total number of degrees of freedom per node; default is 1
  INTEGER( I4B ), ALLOCATABLE :: tNodes( : )
    !! Total number of spatial nodes in each dof, size(tNodes) = tdof
  INTEGER( I4B ) :: storageFMT = Nodes_FMT
    !! storageFMT, There are two types of storage format for nodal vectors
    !! Nodes_FMT, DOF_FMT
  INTEGER( I4B ) :: precondType = 0
    !! Name of preconditioner
  INTEGER( I4B ) :: myRank = 0
    !! MPI Rank
  INTEGER( I4B ) :: comm = 0
    !! MPI COMM
  INTEGER( I4B ) :: numproc = 0
    !! Number of processor running
  CHARACTER( LEN = 5 ) :: Matrixprop = "UNSYM"
    !! Matrix Property
  CONTAINS

  PROCEDURE( ls_init ), PUBLIC, DEFERRED, PASS( obj ) :: Initiate
    !! Initiate the object
  PROCEDURE( ls_set_precon ), PUBLIC, DEFERRED, PASS( obj ) :: setPrecondition
    !! Set preconditioner and its properties
  PROCEDURE( ls_set_sparsity ), PUBLIC, DEFERRED, PASS( To ) :: setSparsity
    !! Set sparsity pattern,
    !! Sparsity is also related to the connectivity of the mesh
  PROCEDURE( ls_set_dbc_1 ), PUBLIC, DEFERRED, PASS( obj ) :: set_dbcNodes_1
    !! Set Dirichlet boundary condition information
  PROCEDURE( ls_set_dbc_2 ), PUBLIC, DEFERRED, PASS( obj ) :: set_dbcNodes_2
    !! Set Dirichlet boundary condition information
  GENERIC, PUBLIC :: setDirichletBCNodes => set_dbcNodes_1, set_dbcNodes_2
    !! Set Dirichlet boundary condition information
  PROCEDURE( ls_set_matrix ), PUBLIC, DEFERRED, PASS( To ) :: setMatrix
    !! Set the matrix
  PROCEDURE( ls_solve ), PUBLIC, DEFERRED, PASS( obj ) :: Solve
    !! Solve system of linear equation
  PROCEDURE( ls_display ), PUBLIC, DEFERRED, PASS( obj ) :: Display
    !! Display the content
  PROCEDURE( ls_w_res ), PUBLIC, DEFERRED, PASS( obj ) :: writeResidueHistory
    !! Write the residue history to a file
  PROCEDURE( ls_deallocate ), PUBLIC, DEFERRED, PASS( obj ) :: DeallocateData
    !! Deallocate Data
END TYPE LinSolver_

PUBLIC :: LinSolver_

!> This data type contains pointer to [[linsolver_]]
TYPE :: LinSolverPointer_
  CLASS( LinSolver_ ), POINTER :: Ptr => NULL( )
END TYPE LinSolverPointer_

!----------------------------------------------------------------------------
!                                                                 Sparsekit_
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! [[Sparsekit_]] data type is a container around Yusef Saad's SparseKit
! lib. It is used to solve the linear system with sparse matrices
!
! - Reference : https://www-users.cs.umn.edu/~saad/software/SPARSKIT/
! - This class interface sparsekit and `EASIFEM`
!
!### Usage
!
! ```fortran
!	CALL obj % Initiate( obj, SolverName, MaxIter, SolverName &
!    & <, diagScale, ipar, fpar> )
! CALL obj % setPrecondition( obj, precondtype <,ipar, fpar> )
! CALL obj % setSparsity( From )
! CALL obj % setDirichletBCNodes( Nptrs, dofs )
! CALL obj % setMatrix( From )
! CALL obj % solve( sol, rhs )
! CALL obj % Display( msg <,unitno > )
! CALL obj % writeResidueHistory( path, prefix, fmt, iter )
! CALL obj % DeallocateData( )
! ```
!
!### Solver name
!
!
!### Precondition Name
!
!
!### Todo
!
!@todo
! - Implement `ilutp` ans `iludp` preconditioners
!@endtodo

TYPE, EXTENDS( LinSolver_ ) :: Sparsekit_
  INTEGER( I4B ), ALLOCATABLE :: dbcNptrs ( : )
    !! IDs of nodal variables where Dirichlet boundary condition is imposed
  INTEGER( I4B ), ALLOCATABLE :: dbcIndx( : )
  INTEGER( I4B ), ALLOCATABLE :: dbcJA( : )
  INTEGER( I4B ), ALLOCATABLE :: dbcIA( : )
  INTEGER( I4B ), POINTER :: IA( : ) => NULL( )
    !! CSR format
  INTEGER( I4B ), POINTER :: JA( : ) => NULL( )
    !! CSR format
  INTEGER( I4B ), ALLOCATABLE :: JLU( : )
    !! Internal variable
  INTEGER( I4B ), ALLOCATABLE :: JU( : )
    !! Internal variable
  INTEGER( I4B ), ALLOCATABLE :: IPERM( : )
    !! Internal variable for integer parameters
  INTEGER( I4B ), ALLOCATABLE :: JW( : )
    !! Internal variable for integer parameters
  REAL( DFP ), POINTER :: A( : ) => NULL( )
    !! Entries of sparse matrix A in CSR format
  REAL( DFP ), ALLOCATABLE :: ALU( : )
    !! Internal variable
  REAL( DFP ), ALLOCATABLE :: WK( : )
    !! Internal variable
  REAL( DFP ), ALLOCATABLE :: W( : )
    !! Internal variable
  REAL( DFP ), ALLOCATABLE :: RES( : )
    !! Residual
  REAL( DFP ) :: fpar( 14 ) = 0.0_DFP
    !! Real parameters
  REAL( DFP ) :: droptol = 0.0
    !! drop tolerance for incomplete LU decomposition
  REAL( DFP ) :: permtol = 0.0
    !! tolerance for permutation
  REAL( DFP ) :: alpha = 0.0
    !! alpha
  INTEGER( I4B ) :: ipar( 14 ) = 0
    !! integer parameter
  INTEGER( I4B ) :: lfil = 0
  INTEGER( I4B ) :: mbloc = 0

  CONTAINS

  PROCEDURE, PUBLIC, PASS( obj ) :: Initiate => skit_initiate
    !! Initiate object
  PROCEDURE, PUBLIC, PASS( obj ) :: setPrecondition => skit_setprecond
    !! Set preconditioner properties
  PROCEDURE, PUBLIC, PASS( To ) :: setSparsity => skit_set_sparsity
    !! Set sparsity pattern related information of tangent matrix
  PROCEDURE, PUBLIC, PASS( obj ) :: set_dbcNodes_1 => skit_setDBC_1
    !! Set Information about the Dirichlet boundary nodes
  PROCEDURE, PUBLIC, PASS( obj ) :: set_dbcNodes_2 => skit_setDBC_2
    !! Set Information about the Dirichlet boundary nodes
  PROCEDURE, PUBLIC, PASS( To ) :: setMatrix => skit_setmatrix
    !! Set/link tangent matrix to the linear solver engine
  PROCEDURE, PUBLIC, PASS( obj ) :: Solve => skit_solve
    !! Solve the system of linear equation
  PROCEDURE, PUBLIC, PASS( obj ) :: Display => skit_display
    !! Display the contents
  PROCEDURE, PUBLIC, PASS( obj ) :: writeResidueHistory => skit_write_res_his
    !! Output the residue history
  PROCEDURE, PUBLIC, PASS( obj ) :: DeallocateData => skit_deallocatedata
    !! DeallocateData
END TYPE Sparsekit_

PUBLIC :: Sparsekit_

TYPE( Sparsekit_ ), PUBLIC, PARAMETER :: &
  & TypeSparsekit = Sparsekit_( &
  & dbcNptrs = NULL( ), &
  & dbcIndx = NULL( ), &
  & dbcJA =   NULL( ), &
  & dbcIA = NULL( ), &
  & JLU = null( ), &
  & JU = null( ), &
  & IPERM = null( ), &
  & JW = NULL( ), &
  & ALU = null( ), &
  & WK = null( ), &
  & W = null( ), &
  & RES = null( ) )

TYPE :: SparsekitPointer_
  CLASS( Sparsekit_ ), POINTER :: Ptr => NULL( )
END TYPE SparsekitPointer_

PUBLIC :: SparsekitPointer_

!----------------------------------------------------------------------------
!                                                                      LIS_
!----------------------------------------------------------------------------

TYPE, EXTENDS( LinSolver_ ) :: LIS_
  TYPE( LIS_MATRIX ) :: lis_mat = 0
  TYPE( LIS_VECTOR ) :: lis_rhs = 0
  TYPE( LIS_VECTOR ) :: lis_sol = 0
  TYPE( LIS_VECTOR ) :: lis_res = 0
  TYPE( LIS_PRECON ) :: lis_precon = 0
  TYPE( LIS_SOLVER ) :: lis_solver = 0
  REAL( DFP ), ALLOCATABLE :: A( : )
  INTEGER( I4B ), ALLOCATABLE :: IA( : )
  INTEGER( I4B ), ALLOCATABLE :: JA( : )
  INTEGER( I4B ), ALLOCATABLE :: dbcNptrs ( : )
  INTEGER( I4B ), ALLOCATABLE :: dbcIndx( : )
  INTEGER( I4B ), ALLOCATABLE :: dbcJA( : )
  INTEGER( I4B ), ALLOCATABLE :: dbcIA( : )

  CONTAINS

  PROCEDURE, PUBLIC, PASS( obj ) :: Initiate => lis_initiate
    !! Initiate object
  PROCEDURE, PUBLIC, PASS( obj ) :: setPrecondition => lis_setprecond
    !! Set preconditioner properties
  PROCEDURE, PUBLIC, PASS( To ) :: setSparsity => lis_set_sparsity
    !! Set sparsity pattern related information of tangent matrix
  PROCEDURE, PUBLIC, PASS( obj ) :: set_dbcNodes_1 => lis_setDBC_1
    !! set Information about the Dirichlet boundary nodes
  PROCEDURE, PUBLIC, PASS( obj ) :: set_dbcNodes_2 => lis_setDBC_2
    !! set Information about the Dirichlet boundary nodes
  PROCEDURE, PUBLIC, PASS( To ) :: setMatrix => lis_setmatrix
    !! set/link tangent matrix to the linear solver engine
  PROCEDURE, PUBLIC, PASS( obj ) :: Solve => lis_solve_1
    !! Solve the system of linear equation
  PROCEDURE, PUBLIC, PASS( obj ) :: Display => lis_display
    !! Display the contents
  PROCEDURE, PUBLIC, PASS( obj ) :: writeResidueHistory => lis_write_res_his
    !! Output the residue history
  PROCEDURE, PUBLIC, PASS( obj ) :: DeallocateData => lis_deallocatedata
    !! Deallocate Data
END TYPE LIS_

PUBLIC :: LIS_

TYPE( LIS_ ), PARAMETER, PUBLIC :: &
  & TypeLIS = LIS_( &
  & A = NULL( ), &
  & IA = NULL( ), &
  & JA = NULL( ), &
  & dbcNptrs = NULL( ), &
  & dbcIndx = NULL( ), &
  & dbcJA = NULL( ), &
  & dbcIA = NULL( ) )

TYPE :: LisPointer_
  CLASS( Lis_ ), POINTER :: Ptr => NULL( )
END TYPE LisPointer_

PUBLIC :: LisPointer_

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE ls_init( obj, SolverName, MaxIter, Tol, diagScale, ipar, fpar )
  IMPORT :: LinSolver_, DFP, I4B
  CLASS( Linsolver_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: Tol
  INTEGER( I4B ), INTENT( IN ) :: MaxIter
  INTEGER( I4B ), INTENT( IN ) :: SolverName
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: ipar( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: diagScale
  REAL( DFP ) , OPTIONAL, INTENT( IN ) :: fpar( : )
END SUBROUTINE ls_init
END INTERFACE

!----------------------------------------------------------------------------
!                                                            setPrecondioning
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE ls_set_precon( obj, precondtype, ipar, fpar )
  IMPORT :: LinSolver_, DFP, I4B
  CLASS( LinSolver_ ), INTENT( INOUT) :: obj
  INTEGER( I4B ), INTENT( IN ) :: precondtype
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: ipar( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: fpar( : )
END SUBROUTINE ls_set_precon
END INTERFACE

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE ls_set_sparsity( From, To )
  IMPORT :: LinSolver_, SparseMatrix_
  CLASS( LinSolver_ ), INTENT( INOUT) :: To
  TYPE( SparseMatrix_ ), INTENT( IN ), TARGET :: From
END SUBROUTINE ls_set_sparsity
END INTERFACE

!----------------------------------------------------------------------------
!                                                        setDirichletBCNodes
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE ls_set_dbc_1(  obj, Nptrs, dofs )
  IMPORT :: LinSolver_, I4B
  CLASS( LinSolver_ ), INTENT( INOUT) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : ), dofs( : )
END SUBROUTINE ls_set_dbc_1
END INTERFACE

ABSTRACT INTERFACE
SUBROUTINE ls_set_dbc_2(  obj, Nptrs, dofs )
  IMPORT :: LinSolver_, IntVector_, I4B
  CLASS( LinSolver_ ), INTENT( INOUT) :: obj
  TYPE( IntVector_ ), INTENT( IN ) :: Nptrs( : )
  INTEGER( I4B ), INTENT( IN ) :: dofs( : )
END SUBROUTINE ls_set_dbc_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    Convert
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE ls_set_matrix( From, To )
  IMPORT :: LinSolver_, SparseMatrix_
  CLASS( LinSolver_ ), INTENT( INOUT) :: To
  TYPE( SparseMatrix_ ), INTENT( IN ), TARGET :: From
END SUBROUTINE ls_set_matrix
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Solve
!----------------------------------------------------------------------------

! sol contains the initial guess
ABSTRACT INTERFACE
SUBROUTINE ls_solve( obj, sol, rhs )
  IMPORT :: LinSolver_, DFP
  CLASS( LinSolver_ ), INTENT( INOUT) :: obj
  REAL( DFP ), INTENT( INOUT) :: sol( : )
  REAL( DFP ), INTENT( INOUT ) :: rhs( : )
END SUBROUTINE ls_solve
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE ls_display( obj, msg, unitno )
  IMPORT :: LinSolver_, I4B
  CLASS( LinSolver_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: Unitno
END SUBROUTINE ls_display
END INTERFACE

!----------------------------------------------------------------------------
!                                                          WriteResidueHisory
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE ls_w_res( obj, path, prefix, fmt, iter )
  IMPORT :: LinSolver_, I4B
  CLASS( LinSolver_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: path, prefix, fmt
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: iter
END SUBROUTINE ls_w_res
END INTERFACE

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE ls_deallocate( obj )
  IMPORT :: LinSolver_
  CLASS( LinSolver_ ), INTENT( INOUT) :: obj
END SUBROUTINE ls_deallocate
END INTERFACE

!-----------------------------------------------------------------------------
!                                                           Initiate@Methods
!-----------------------------------------------------------------------------

INTERFACE
!! Initiate [[sparsekit_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine initiate the [[sparsekit_]] object
!
! - It sets the name of the solver
! - It sets the parameters related to the solver
!
! If name of the solver is `lis_gmres`, `lis_fgmres`, `lis_dqgmres`,
! or `lis_om` then `ipar(1)` denotes the number of restarts required in
! these algorithms. Default value is set to 20.

MODULE SUBROUTINE skit_initiate( obj, SolverName, MaxIter, Tol, &
  & diagScale, ipar, fpar )
  CLASS( Sparsekit_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: Tol
  INTEGER( I4B ), INTENT( IN ) :: MaxIter
  INTEGER( I4B ), INTENT( IN ) :: SolverName
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: ipar( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: diagScale
  REAL( DFP ) , OPTIONAL, INTENT( IN ) :: fpar( : )
END SUBROUTINE skit_initiate
END INTERFACE

!> Generic subroutine to initiate [[sparsekit_]]
INTERFACE Initiate
  MODULE PROCEDURE skit_initiate
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                  setPrecondioning@Methods
!----------------------------------------------------------------------------

INTERFACE
!! Set preconditioners in [[sparsekit_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine set the preconditioner required to solve system of
! linear equations by using the
!
! - For `precondType=p_ilut` `ipar( 1 )` denotes number of fills and `fpar(1)`
! denotes the dropping tolerance
! - For `p_ilutp`:
!   - `ipar(1)` number of fills default is 10
!   - `ipar(2)` mbloc, default is size of problem
!   - `fpar(1)` drop tolerance, default is 1.0E-4
!   - `fpar(2)` permutation tolerance, default is 0.5
! - For `p_ilud`
!   - `fpar(1)` denotes drop tolerance
!   - `fpar(2)` denotes value of alpha
! - For `p_iludp`
!   - `ipar(1)` denotes mbloc
!   - `fpar(1)` denotes drop tolerance
!   - `fpar(2)` denotes value of alpha
!   - `fpar(3)` denotes permutation tolerance

MODULE SUBROUTINE skit_setprecond( obj, precondtype, ipar, fpar )
  CLASS( Sparsekit_ ), INTENT( INOUT) :: obj
  INTEGER( I4B ), INTENT( IN ) :: precondtype
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: ipar( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: fpar( : )
END SUBROUTINE skit_setprecond
END INTERFACE

!----------------------------------------------------------------------------
!                                                      setSparsity@Methods
!----------------------------------------------------------------------------

INTERFACE
!! Set sparsity pattern in [[sparsekit_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine set the sparsity pattern in [[sparsekit_]]

MODULE SUBROUTINE skit_set_sparsity( From, To )
  CLASS( Sparsekit_ ), INTENT( INOUT) :: To
  TYPE( SparseMatrix_ ), INTENT( IN ), TARGET :: From
END SUBROUTINE skit_set_sparsity
END INTERFACE

!----------------------------------------------------------------------------
!                                              setDirichletBCNodes@Methods
!----------------------------------------------------------------------------

INTERFACE
!! set Dirichlet boundary condition information

!> authors: Dr. Vikas Sharma
!
! This subroutine set the Dirichlet boundary condition in the linear solver
! In this case all DOFs have the same dirichlet nodes pointers
! `Nptrs` denotes the dirichlet node numbers
! `storageFMT` can be `DOF_FMT` or `Nodes_FMT`

MODULE SUBROUTINE skit_setDBC_1(  obj, Nptrs, dofs )
  CLASS( Sparsekit_ ), INTENT( INOUT) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : )
  INTEGER( I4B ), INTENT( IN ) :: dofs( : )
END SUBROUTINE skit_setDBC_1
END INTERFACE

INTERFACE
!! set Dirichlet boundary condition information

MODULE SUBROUTINE skit_setDBC_2(  obj, Nptrs, dofs )
  CLASS( Sparsekit_ ), INTENT( INOUT) :: obj
  TYPE( IntVector_ ), INTENT( IN ) :: Nptrs( : )
  INTEGER( I4B ), INTENT( IN ) :: dofs( : )
END SUBROUTINE skit_setDBC_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Convert@Methods
!----------------------------------------------------------------------------

INTERFACE
!! set Matrix
MODULE SUBROUTINE skit_setmatrix( From, To )
  CLASS( Sparsekit_ ), INTENT( INOUT) :: To
  TYPE( SparseMatrix_ ), INTENT( IN ), TARGET :: From
END SUBROUTINE skit_setmatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Solve@Methods
!----------------------------------------------------------------------------

! sol contains the initial guess
INTERFACE
MODULE SUBROUTINE skit_solve( obj, sol, rhs )
  CLASS( Sparsekit_ ), INTENT( INOUT) :: obj
  REAL( DFP ), INTENT( INOUT) :: sol( : )
  REAL( DFP ), INTENT( INOUT ) :: rhs( : )
END SUBROUTINE skit_solve
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@Sparsekit
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE skit_display( obj, msg, unitno )
  CLASS( Sparsekit_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: Unitno
END SUBROUTINE skit_display
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE skit_display
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                WriteResidueHisory@Sparsekit
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE skit_write_res_his( obj, path, prefix, fmt, iter )
  CLASS( Sparsekit_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: path, prefix, fmt
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: iter
END SUBROUTINE skit_write_res_his
END INTERFACE

!----------------------------------------------------------------------------
!                                                   DeallocateData@Sparsekit
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE skit_deallocatedata( obj )
  CLASS( Sparsekit_ ), INTENT( INOUT) :: obj
END SUBROUTINE skit_deallocatedata
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE skit_deallocatedata
END INTERFACE DeallocateData

PUBLIC :: DeallocateData


!----------------------------------------------------------------------------
!                                                        Initiate@Sparsekitt
!----------------------------------------------------------------------------

!<--- tInit : denotes the diagonal scaling 0-> no; 1->Right; 2-> symm
!<--- ipar( 1 ) : contains ell or restart or irestart
!<--- fpar( 1 ) : contains value of omega
INTERFACE
MODULE SUBROUTINE lis_initiate( obj, SolverName, MaxIter, Tol, &
  & diagScale, ipar, fpar )
  CLASS( LIS_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: Tol
  INTEGER( I4B ), INTENT( IN ) :: MaxIter, SolverName
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: ipar( : ), diagScale
  REAL( DFP ) , OPTIONAL, INTENT( IN ) :: fpar( : )
END SUBROUTINE lis_initiate
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE lis_initiate
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                 setPreconditioning@Methods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE lis_setprecond( obj, precondtype,  ipar, fpar )
  CLASS( LIS_ ), INTENT( INOUT) :: obj
  INTEGER( I4B ), INTENT( IN ) :: precondtype
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: ipar( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: fpar( : )
END SUBROUTINE lis_setprecond
END INTERFACE

!----------------------------------------------------------------------------
!                                                        setSparsity@Methods
!----------------------------------------------------------------------------

!<--- allocate obj % A, obj % IA, obj % JA
!<--- set size of obj % lis_rhs, lis_sol, lis_mat
!<--- set all values of lis_rhs and lis_sol to zero
!<--- set csr
INTERFACE
MODULE SUBROUTINE lis_set_sparsity( From, To )
  CLASS( LIS_ ), INTENT( INOUT) :: To
  TYPE( SparseMatrix_ ), INTENT( IN ), TARGET :: From
END SUBROUTINE lis_set_sparsity
END INTERFACE

!----------------------------------------------------------------------------
!                                                setDirichletBCNodes@Methods
!----------------------------------------------------------------------------

!<--- initiate dbcNptrs, dbcJA, dbcIndx, dbcIA
INTERFACE
MODULE SUBROUTINE lis_setDBC_1(  obj, Nptrs, dofs )
  CLASS( LIS_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : )
  INTEGER( I4B ), INTENT( IN ) :: dofs( : )
END SUBROUTINE lis_setDBC_1
END INTERFACE

INTERFACE
MODULE SUBROUTINE lis_setDBC_2(  obj, Nptrs, dofs )
  CLASS( LIS_ ), INTENT( INOUT) :: obj
  TYPE( IntVector_ ), INTENT( IN ) :: Nptrs( : )
  INTEGER( I4B ), INTENT( IN ) :: dofs( : )
END SUBROUTINE lis_setDBC_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Convert@Methods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE lis_setmatrix( From, To )
  CLASS( LIS_ ), INTENT( INOUT) :: To
  TYPE( SparseMatrix_ ), INTENT( IN ), TARGET :: From
END SUBROUTINE lis_setmatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Solve@Methods
!----------------------------------------------------------------------------

! sol contains the initial guess
INTERFACE
MODULE SUBROUTINE lis_solve_1( obj, sol, rhs )
  CLASS( LIS_ ), INTENT( INOUT) :: obj
  REAL( DFP ), INTENT( INOUT) :: sol( : )
  REAL( DFP ), INTENT( INOUT ) :: rhs( : )
END SUBROUTINE lis_solve_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Display@Methods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE lis_display( obj, msg, unitno )
  CLASS( LIS_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE lis_display
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE lis_display
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                WriteResidueHistory@Methods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE lis_write_res_his( obj, path, prefix, fmt, iter )
  CLASS( LIS_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: path, prefix, fmt
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: iter
END SUBROUTINE lis_write_res_his
END INTERFACE

!----------------------------------------------------------------------------
!                                                      DeallocateData@Methods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE lis_deallocatedata( obj )
  CLASS( LIS_ ), INTENT( INOUT) :: obj
END SUBROUTINE lis_deallocatedata
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE lis_deallocatedata
END INTERFACE DeallocateData

END MODULE LinSolver_Class