#!/bin/bash
# converts a lammps dump into a simple text file containing potential energy only
# eg my files look like
#      ITEM: TIMESTEP                                
#      0                                             
#      ITEM: NUMBER OF ATOMS                         
#      1638400                                       
#      ITEM: BOX BOUNDS pp pp pp                     
#      -4.9550866446472721e+00 6.3799420664458569e+02
#      0.0000000000000000e+00 2.0257251840000001e+02 
#      0.0000000000000000e+00 2.0257251840000001e+02 
#      ITEM: ATOMS type x y z c_cpe                  
#      1 114.565 0.00747166 1.37621 -8.90259         
#      1 117.721 0.0127243 1.37939 -8.89688          
#      1 120.87 0.0216775 1.38363 -8.90116           
#      1 124.013 0.0182541 1.38316 -8.90378          
#      1 127.16 0.0187868 1.39148 -8.89816           
# etc
# This script outputs line 4 ( number of atoms ) and column 5 ( potential energy )                                                    
awk 'NR==4{print $0}NR>9{print $5}' $1 > $1.pe.dat 