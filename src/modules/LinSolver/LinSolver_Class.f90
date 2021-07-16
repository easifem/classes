MODULE LinSolver_Class
  !! This module defines an abstract class for a linear solver
USE GlobalData
USE BaseType
#if USE_LIS
#include "lisf.h"
#endif
IMPLICIT NONE
PRIVATE
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_cg = 1
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_bcg=2
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_bicg = 2
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_cgs = 3
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_bcgstab=4
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_bicgstab = 4
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_bcgstabl = 5
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_bicgstabl = 5
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_gpbicg = 6
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_tfqmr=7
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_omn = 8
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_fom=8
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_orthomin = 8
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_gmres=9
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_gmr = 9
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_jacobi = 10
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_gs = 11
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_sor = 12
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_bicgsafe = 13
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_cr = 14
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_bicr = 15
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_crs = 16
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_bicrstab = 17
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_gpbicr = 18
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_bicrsafe = 19
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_fgmres=20
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_idrs = 21
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_idr1 = 22
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_minres = 23
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_cocg = 24
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_cocr = 25
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_cgnr=26
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_cgn = 26
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_dbcg=27
INTEGER( I4B ), PARAMETER, PUBLIC :: lis_dqgmres=28
INTEGER( I4B ), PARAMETER, PUBLIC :: p_none = 0
INTEGER( I4B ), PARAMETER, PUBLIC :: p_jacobi = 1
INTEGER( I4B ), PARAMETER, PUBLIC :: p_iluk = 2
INTEGER( I4B ), PARAMETER, PUBLIC :: p_ssor = 3
INTEGER( I4B ), PARAMETER, PUBLIC :: p_hybrid = 4
INTEGER( I4B ), PARAMETER, PUBLIC :: p_is = 5
INTEGER( I4B ), PARAMETER, PUBLIC :: p_sainv = 6
INTEGER( I4B ), PARAMETER, PUBLIC :: p_saamg = 7
INTEGER( I4B ), PARAMETER, PUBLIC :: p_iluc = 8
INTEGER( I4B ), PARAMETER, PUBLIC :: p_ilut = 9
INTEGER( I4B ), PARAMETER, PUBLIC :: p_ilutp = 10
INTEGER( I4B ), PARAMETER, PUBLIC :: p_ilud = 11
INTEGER( I4B ), PARAMETER, PUBLIC :: p_iludp = 12
INTEGER( I4B ), PARAMETER, PUBLIC :: p_ilu0 = 13

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
  INTEGER( I4B ) :: storageFMT = NODES_FMT
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


END MODULE LinSolver_Class