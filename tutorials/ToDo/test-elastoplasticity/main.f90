! Testing elasto-plastic kernel
!-------------------------------------------------------------------

PROGRAM MAIN
	USE EASIFEM
	USE Kernel_Class
	USE SmallStrainElasticity_Class

	IMPLICIT NONE

	TYPE( SmallStrainElasticity_ ) :: Omega
	TYPE( IterationData_ ) :: LISIterData, NewtonIterData
	TYPE( File_ ) :: aFile
	TYPE( File_ ), ALLOCATABLE :: tsnFiles( : )
	INTEGER( I4B ) :: NSD, tSteps, iTS, i, tsize, iTer, tDOF
	INTEGER( I4B ), ALLOCATABLE :: ResultNodes( : )
	REAL( DFP ) :: dt, t1, t2, Vin1( 3 ) = 0.0, Vin2( 3 )=0.0, Err, NormErr
	REAL( DFP ), ALLOCATABLE :: InputVelocity( : )
	LOGICAL( LGT ) :: Converged

	!----------------------------------------------------------------------------
	NSD = 2;
	LISIterData = IterationData( MaxIter = 400, Tolerance = 1.0d-4 )
	NewtonIterData = IterationData( MaxIter = 100, Tolerance = 5.0d-3 )
	CALL aFile % OpenFileToRead( "./GroundMotion/", "GroundVelocity", ".txt" )
	READ( aFile % UnitNo, * ) tSteps; READ( aFile % UnitNo, * ) dt
	ALLOCATE( InputVelocity( tSteps ) )
	DO iTS = 1, tSteps
		READ( aFile % UnitNo, * ) t1, InputVelocity( iTS )
	END DO
	!----------------------------------------------------------------------------
	CALL Omega % Initiate( NSD, dt )
	CALL Omega % SetMaterialProperty( [40.0d+9, 28.0d+9], "E" )
	CALL Omega % SetMaterialProperty( [0.2_DFP, 0.2_DFP], "Nu" )
	CALL Omega % SetMaterialProperty( [2551.0_DFP, 2347.0_DFP], "Rho" )
	CALL Omega % setMSHFile( Tag = String([ "./mesh1/", "dam-soil", ".msh" ]) )
	CALL Omega % setNodes( )
	CALL Omega % setMesh( )
	CALL Omega % setxViscousBoundary( Tag = [ String( "VerticalABC" ) ] )
	CALL Omega % setyViscousBoundary( Tag = [ String( "HorizontalABC" ) ] )
	CALL Omega % setAlgorithm( [ vSTFEM, NewtonMethod] )
	CALL Omega % SetConstitutiveParameter( )
	CALL Omega % setDOF( )
	CALL Omega % setRHS( )
	CALL Omega % setLinearSolver( IterData = LISIterData )
	CALL Omega % setTangentMatrix( )

	tDOF = ( .tNodes. Omega % Nodes ) * ( Omega % NSD ) * ( Omega % NNT )

	DO iTS = 1, 50 !tStep
		WRITE( *, "(A, I6)" ) "iTS :: ", iTS; CALL EqualLine( ); CALL BlankLines( )
		t1 = ( iTS - 1 ) * dt
		t2 = t1 + dt
		Vin1( 1 ) = InputVelocity( iTS )
		Vin2( 1 ) = InputVelocity( iTS + 1)

		CALL Omega % InitiateTangentMatrix( )

		CALL Omega % InitiateRHS( )

		CALL Omega % AddEarthQuakeMotion( Omega % yABC, Omega % yABCMat, Vin1, Vin2 ) !<--

		CALL NewtonIterData % setErrorAtStart( 1.0_DFP )
		CALL NewtonIterData % DisplayIterationStartMessage( )

		! Start Newton loop
		DO iTer = 1, NewtonIterData % MaxIter
			CALL NewtonIterData % setIterationNumber( iTer )
			CALL Omega % RHS % ApplyDBC( )
			CALL Omega % RHS % getLpNorm( NormErr, 2 )
			NormErr  = NormErr / tDOF
			CALL NewtonIterData % SetErrorAtEnd( NormErr )
			WRITE( *, * ) "NormErr:: ", NormErr

			IF( NewtonIterData % isConverged( ) ) THEN
				CALL NewtonIterData % DisplayConvergenceMessage( )
				NewtonIterData = .TRUE.
				Converged = .TRUE.
				EXIT
			ELSE
				Converged = .FALSE.
				CALL Omega % Solve( )
				CALL Omega % UpdateIteration( )
			END IF

			IF( iTer .EQ. 1 ) CALL NewtonIterData % setErrorAtStart( NormErr )

		END DO

		IF( Converged ) THEN
			CALL Omega % UpdateTimeStep( )
		ELSE
			WRITE( *, "(A)" ) "No convergence program stopped"
			STOP
		END IF

	END DO

END PROGRAM MAIN
