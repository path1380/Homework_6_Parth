#!/bin/bash
#SBATCH --account=ucbclass2_summit1
#SBATCH --nodes=1
#SBATCH --time=1:00:00
#SBATCH --qos=debug
#SBATCH --partition=shas

module load intel/17.4
ifort -qopenmp -O3 vadd_loop.f90 -qopt-report -qopt-report-phase=vec
./a.out
