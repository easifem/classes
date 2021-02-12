    cmd = "mkdir -p "//trim( path )
    call executecommand( cmd, "mshType@msh4.f90>>WriteVectorNodeData()" )

    IF( PRESENT( nodes ) ) THEN
      call obj%writemesh( path = '',filename = filename, extension = '.msh', &
        & nodes=nodes )
      call OpenFileToAppend( obj=afile, path = '', filename = filename, &
        & extension = '.msh' )
      unitno = afile%unitno
    ELSE
      call OpenFileToWrite( obj = afile, path = '',  &
        & filename = filename, extension = '.msh' )
      unitno = aFile % unitno
      write( unitno, "(A)") "$MeshFormat"
      write( unitno, "(A)") trim( obj % format % meshformat )
      write( unitno, "(A)") "$EndMeshFormat"
    END IF

    write( unitno, "(A)") "$NodeData"
    !<--- tStrings
    write( unitno, "(A)") "1"
    write( unitno, "(A)") '"'//trim( name )//'"'
    !<--- tRealTags
    write( unitno, "(A)") "1"
    write( unitno, "(A)") "0.0"
    !<--- tIntTags
    write( unitno, "(A)" ) "3"
    !<--- Set the time step number or Iteration number
    if( size( indx ) .eq. 1 ) then
      write( unitno, "(a)" ) trim( int2str( indx( 1 ) ) )
    else
      write( unitno, "(a)" ) trim( int2str( indx( 2 ) ) )
    end if

    is = ( itime  - 1 ) * tspace + 1
    ie = is + tspace - 1

    if( spacecompo ) then
      write( unitno, "(a)" ) "3"
      write( unitno, "(I6)" ) dofobj % map( iname, 6 )

      if( size( local_nptrs ) .EQ. 1 ) then

        do i = 1, dofobj % map( iname, 6 )

          call getarrayvalues( v= vals, val = x, obj = dofobj, &
            & dofno = dofs( is:ie ), &
            & storagefmt = Nodes_FMT, nptrs = [i] )

          val( 1 : tsize ) = vals( 1 : tsize )
          WRITE( unitno, '(I6, 2X, 3G14.6)' ) i, val

        end do

      else

        do i = 1, dofobj % map( iname, 6 )

          call getarrayvalues( v= vals, val = x, obj = dofobj, &
            & dofno = dofs( is:ie ), &
            & storagefmt = Nodes_FMT, nptrs = [local_nptrs(i)] )

          val( 1 : tsize ) = vals( 1 : tsize )
          WRITE( unitno, '(I6, 2X, 3G14.6)' ) i, val

        end do
      end if

    else

      write( unitno, "(a)" ) "1"
      write( unitno, "(I6)" ) dofobj % map( iname, 6 )

      if( size(local_nptrs) .EQ. 1 ) then

        do i = 1, dofobj % map( iname, 6 )
          call getarrayvalues( v= vals, val = x, obj = dofobj, &
            & dofno = dofs( is:ie ), &
            & storagefmt = Nodes_FMT, nptrs = [i] )
          WRITE( unitno, '(I6, 2X, G14.6)' ) i, vals( 1 )
        end do

      else

        do i = 1, dofobj % map( iname, 6 )
          call getarrayvalues( v= vals, val = x, obj = dofobj, &
            & dofno = dofs( is:ie ), &
            & storagefmt = Nodes_FMT, nptrs = [local_nptrs(i)] )
          WRITE( unitno, '(I6, 2X, G14.6)' ) i, vals( 1 )
        end do

      end if

    end if
    write( unitno, "(a)") "$EndNodeData"
    call closefile ( afile )