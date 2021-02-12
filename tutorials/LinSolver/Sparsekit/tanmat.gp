# Gnuplot script file
# Author :: Vikas Sharma
# From :: SparseMatrix_Method@Constructor.f90>>Spy()
set terminal postscript eps enhance color font 'Helvetica,10'
set output './tanmat.eps'
set xlabel 'I'
set ylabel 'J'
set size ratio -1
set title 'nnz = 28'
set xrange[1:10]
set yrange[10:1]
plot'./tanmat.txt' with points pt 5 ps 0.5
