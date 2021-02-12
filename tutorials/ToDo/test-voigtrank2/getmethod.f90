program main
        use basetype
        use basemethod
        implicit none

        block
                type( voigtrank2tensor_ ) :: obj
                real( dfp ) :: v3( 3 ), v4( 4 ), v6( 6 )

                call random_number( v3 ); call random_number( v4 ); call random_number( v6 )
                call initiate( obj, v3, StressTypeVoigt )
                call display( obj, "obj" )

                call convert( from = obj, to = v3 )
                call display( v3, "v3" )
        end block
end program main
