program main
        use basetype
        use basemethod
        implicit none

        type( voigtrank2tensor_ ) :: obj
        real( dfp ) :: v3( 3 ), v4( 4 ), v6( 6 )

        call random_number( v3 )
        call random_number( v4 )
        call random_number( v6 )

        obj = voigtrank2tensor(v3, stresstypevoigt)
        call display( obj, "v3" )
        obj = voigtrank2tensor(v4, stresstypevoigt)
        call display( obj, "v4" )
        obj = voigtrank2tensor(v6, straintypevoigt)
        call display( obj, "v6" )

end program main
