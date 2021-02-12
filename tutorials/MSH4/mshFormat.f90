program main
use basetype
use basemethod
use mshType

type( mshFormat_ ) :: obj
type( File_ ) :: aFile
logical( LGT ) :: ierr

call openFileToRead( aFile, "./", "mesh", ".msh" )
call obj % ReadFromFile( aFile, ierr )
call display( obj, "obj")
end program main