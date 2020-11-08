# COC473 - Álgebra Linear Computacional @ ECI/UFRJ
# Macros
f95c = gfortran
blas = -llapack -lblas
args = -O2 -ffpe-summary=none -fno-range-check $(blas)

# Begin Makefile
all: pagerank

pagerank: utillib.o plotlib.o pagerank.o
	$(f95c) -o pagerank $(args) utillib.o plotlib.o pagerank.o

plottest: utillib.o plotlib.o plottest.o
	$(f95c) -o plottest $(args) utillib.o plotlib.o plottest.o

%.mod: %.o %.f95
	$(f95c) -c $(args) $<

%.o: %.f95
	$(f95c) -c $(args) $<

clean:
	rm -f *.o *.mod pagerank plot/*.dat plot/*.plt
# End Makefile