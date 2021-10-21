!
!   compile this code with:
!       gfortran -ffree-line-length-256  NBAX_StringTokenizers.f90 Lib_CommandLineArguments.f90 thermalConductivity.f90 -o thermalConductivity.exe
!   run this code with:
!       thermalConductivity.exe
!

!       MIT License
!
!       Copyright (c) 2021 Daniel Robert Mason, CCFE, UKAEA, Abingdon, Oxfordshire, OX14 3DB, UK
!
!
!       Permission is hereby granted, free of charge, to any person obtaining a copy
!       of this software and associated documentation files (the "Software"), to deal
!       in the Software without restriction, including without limitation the rights
!       to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
!       copies of the Software, and to permit persons to whom the Software is
!       furnished to do so, subject to the following conditions:
!
!       The above copyright notice and this permission notice shall be included in all
!       copies or substantial portions of the Software.
!
!       THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!       IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
!       FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
!       AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
!       LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
!       OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
!       SOFTWARE.
!    



    program thermalConductivity
!---^^^^^^^^^^^^^^^^^^^^^^^^^^^
!*      compute the thermal conductivity according to the method in 
!*      "An estimate for thermal diffusivity in highly irradiated tungsten using MolecularDynamics simulation"
!*      D.R. Mason et al (2021)
!*
!*      The most important input is the potential energies, which should be in a simple text file
!*      line 1 :        number_of_atoms                                                                                         
!*      line 2+:        e1 e2 e3 ...
!*

        use Lib_CommandLineArguments            !   used to read command line arguments and produce a simple -h option
        use iso_fortran_env
        implicit none



    !---    input parameters - note that these initial values are for tungsten, in order to help interpret the paper.
        character(len=256)              ::      filename = ""               !   input file containing energies
        
        real(kind=real64)               ::      b0           = 2.7411d0     !   nearest neighbour separation                     ( A )
        real(kind=real64)               ::      e0           = -8.90d0      !   energy per atom perfect lattice, inc thermal expansion, exc 3/2 kT ( eV )
        real(kind=real64)               ::      Delta        = 0.029d0      !   scaling of Maxwell-Boltzmann energy spread with temperature sigma^2 = Delta kB T (eV)
        
        real(kind=real64)               ::      S_0          = 2.32d0       !   impurity scattering rate r_imp = S0 |E| ( 1/fs/eV )
        real(kind=real64)               ::      S_1          = 1.154d-4     !   e-ph scattering rate r_eph = S1 T       ( 1/fs/K )
        real(kind=real64)               ::      S_2          = 1.209d-7     !   e-e scattering rate ree = S2 T^2        ( 1/fs/K^2 )           
        real(kind=real64)               ::      ceonT        = 8.6611d-9    !   electronic heat capacity per atom       ( ev/K^2 )
        real(kind=real64)               ::      vF           = 9.50d0       !   Fermi velocity                          ( A/fs )
        
        real(kind=real64)               ::      T            = 300d0        !   temperature                             (K)        
        logical                         ::      MDsim        = .true.       !   true if we expect thermal variation 3/2 kT, false if CG data local minimum 
        real(kind=real64)               ::      EMIN = -0.5d0 , EMAX = 3.0d0    !   maximum and minimum excess energy per atom eV
        integer                         ::      NBIN = 700                      !   number of bins        
        logical                         ::      opHist       = .false.      !   output the full histogram?        
        
        type(CommandLineArguments)      ::      cla
        
        
        
    !---    physical constants
        real(kind=real64),parameter     ::      KB = 0.00008617d0           !   Boltzmann's constant (eV/K)    
        real(kind=real64),parameter     ::      EVFSATOWM = 1.602d6         !   eV/fs 1/A 1/K in W/m/K
        real(kind=real128)              ::      sqrt2onPI = sqrt( 0.5_real128/atan(1.0_real128) )
        real(kind=real64),parameter     ::      PI = 3.141592653590d0           !    
        
    !---    properties read from input file
        integer                         ::      nAtoms                      !   number of atoms    
        real(kind=real64),dimension(:),allocatable  ::      xe              !   array storing potential energy of atoms
        
        
        
    !---    derived constants
        real(kind=real64)               ::      kappa_const                 !   thermal conductivity constant 1/3 (ce/T) T vF^2/Omega0
        real(kind=real64)               ::      deltaE                      !   histogram energy window 
        real(kind=real64)               ::      sigma                       !   Maxwell-Boltzmann broadening
        
        
    !---    dummy variables
        logical                         ::      ok 
        integer                         ::      ii,ioerr,bb
        real(kind=real64)               ::      ee,rho,kk,rr,e2Bar
        
        
    !---    output variables
        real(kind=real64)                   ::      nDefects        !   statistically determined count of atoms with athermal pot eng
        real(kind=real64)                   ::      rBar            !   mean scattering rate
        real(kind=real64)                   ::      eBar            !   mean excess potential energy
        real(kind=real64)                   ::      kappa           !   thermal conductivity x volume per atom   
        real(kind=real64)                   ::      alpha           !   thermal diffusivity
        real(kind=real64)                   ::      eVar            !   variance of potential energy
        integer,dimension(:),allocatable    ::      hist            !   histogram of atom potential energy counts
        
        
        
        
    !---    read in command line arguments
        cla = CommandLineArguments_ctor(20)
        call setProgramDescription( cla, "thermalConductivity.exe - computes thermal conductivity from potential energies &             
                                          \n     Note that kappa needs to be divided by volume per atom (in A^3) to give answer in standard units       &
                                          \n     This keeps thermal diffusivity independent of simulation volume." )
        call setProgramVersion( cla, "2.0" )
        call setCategories(cla,(/ "required    ","potential   ","simulation  ","scattering  ","histogram   " /))
        
        call get( cla,"f"    ,filename     ,LIB_CLA_REQUIRED,"      input filename                                                                 ",1 )  
        call get( cla,"Delta",Delta        ,LIB_CLA_OPTIONAL,"scaling of Maxwell-Boltzmann energy spread with temperature sigma^2 = Delta kB T (eV)",2 )  
        call get( cla,"b0"   ,b0           ,LIB_CLA_OPTIONAL,"   nearest neighbour spacing               ( A )                                     ",2 )  
        call get( cla,"e0"   ,e0           ,LIB_CLA_OPTIONAL,"   energy per atom perfect lattice, inc thermal expansion, exc 3/2 kT ( eV )         ",2 )  
        call get( cla,"T"    ,T            ,LIB_CLA_OPTIONAL,"    temperature                             (K)                                      ",3 )  
        call get( cla,"MD"   ,MDsim        ,LIB_CLA_OPTIONAL,"   true if we expect thermal variation 3/2 kT, false if CG data local minimum        ",3 )  
        call get( cla,"S_0"  ,S_0          ,LIB_CLA_OPTIONAL,"  impurity scattering rate r_imp = S0 |E| ( 1/fs/eV )                                ",4 )  
        call get( cla,"S_1"  ,S_1          ,LIB_CLA_OPTIONAL,"  e-ph scattering rate r_eph = S1 T       ( 1/fs/K )                                 ",4 )  
        call get( cla,"S_2"  ,S_2          ,LIB_CLA_OPTIONAL,"  e-e scattering rate ree = S2 T^2        ( 1/fs/K^2 )                               ",4 )  
        call get( cla,"ceonT",ceonT        ,LIB_CLA_OPTIONAL,"electronic heat capacity constant       ( ev/K^2 )                                   ",4 )  
        call get( cla,"vF"   ,vF           ,LIB_CLA_OPTIONAL,"   Fermi velocity                          ( A/fs )                                  ",4 )  
        call get( cla,"EMIN" ,EMIN         ,LIB_CLA_OPTIONAL," minimum excess energy per atom          ( eV )                                      ",5 )  
        call get( cla,"EMAX" ,EMAX         ,LIB_CLA_OPTIONAL," maximum excess energy per atom          ( eV )                                      ",5 )  
        call get( cla,"NBIN" ,NBIN         ,LIB_CLA_OPTIONAL," number of histogram bins                                                            ",5 )  
        call get( cla,"hist" ,opHist       ,LIB_CLA_OPTIONAL," output the full histogram?                                                          ",5 )  
        
        call report(cla)
        if (.not. allRequiredArgumentsSet(cla)) stop
        if (hasHelpArgument(cla)) stop
        call delete(cla)
        
        
        
        
    !---    compute derived constants from input variables
        kappa_const = (ceonT/3)*T*vF*vF 
        sigma = sqrt( Delta*KB*T )
        deltaE = (EMAX - EMIN)/NBIN
        
        
        
    !---    output a friendly message about the input parameters
        print *,""
        print *,"thermalConductivity.exe info - input parameters"
        print *,"file  : ",filename    
        print *,"Delta : ",Delta       
        print *,"b0    : ",b0          
        print *,"e0    : ",e0          
        print *,"T     : ",T           
        print *,"MD    : ",MDsim       
        print *,"S_0   : ",S_0         
        print *,"S_1   : ",S_1         
        print *,"S_2   : ",S_2         
        print *,"ceonT : ",ceonT       
        print *,"vF    : ",vF          
        print *,"EMIN  : ",EMIN        
        print *,"EMAX  : ",EMAX        
        print *,"NBIN  : ",NBIN      
        print *,""  
        
        
        
        
    !---    read in input file
        inquire(file=trim(filename),exist=ok)
        if (.not. ok) then
            print *,"thermalConductivity.exe error - could not fine input file """//trim(filename)//""""
            stop
        end if
        open(unit=400,file=trim(filename),action="read")
            read(unit=400,fmt=*,iostat=ioerr) nAtoms
            if (ioerr==0) then
                print *,"thermalConductivity.exe info - reading ",nAtoms," atom energies ... "
            else
                print *,"thermalConductivity.exe error - input file format should be"
                print *,"[number of atoms]"
                print *,"e1 e2 e3 ... eN"  
                stop
            end if
            allocate(xe(nAtoms))
            read(unit=400,fmt=*,iostat=ioerr) xe            !   note that we read in total potential energy...
            if (ioerr==0) then
                print *,"... done"
            else
                print *,"thermalConductivity.exe error - input file format should be"
                print *,"[number of atoms]"
                print *,"e1 e2 e3 ... eN"  
                stop
            end if                
        close(unit=400)               
        
    !   remove expected potential energy per atom, in order to find _excess_ energy only   
        xe = xe - e0            
        
        
    !---    construct histogram 
        allocate(hist(0:NBIN))
        hist = 0                   
        eBar = 0.0d0
        e2Bar = 0.0d0
        do ii = 1,nAtoms
            ee = xe(ii)                             !   energy of atom i                                
            bb = nint( (ee - EMIN)/deltaE )         !   find correct bin for energy
            bb = max( 0, min( NBIN,bb ) )           !   limit bin range
            hist(bb) = hist(bb) + 1
            eBar = eBar + ee
            e2Bar = e2Bar + ee*ee
        end do
        eBar = eBar / nAtoms
        e2Bar = e2Bar / nAtoms
        print *,"thermalConductivity.exe info - number of atoms in MIN bin (",EMIN,") = ",hist(0)
        print *,"thermalConductivity.exe info - number of atoms in MAX bin (",EMAX,") = ",hist(NBIN)
        
        
        
        
    !---    compute number of defects and average scattering rate
        print *,""
        if (opHist) write(*,fmt='(5a16)')   "energy","<n>","n","n_defect","r"
        do bb = 0,NBIN
            ee = EMIN + deltaE*bb
            
            rho = BroadenedMaxwellBoltzmann( ee,T,sigma )*deltaE*nAtoms 
            call computeDefectsFromHist( hist(bb),nAtoms,ee,deltaE,T, sigma, b0,vF,S_0,S_1,S_2 , kk,rr )
            
            nDefects = nDefects + kk
            rBar = rBar + rr
            
            if (opHist) write(*,fmt='(2f16.6,i16,2f16.6)') ee,rho,hist(bb),kk,rr
            
        end do
        rBar = rBar/nAtoms
        if (opHist) print *,""
        
    !---    compute thermal conductivity x volume per atom - divide by Omega0 to get "correct" units
        kappa = kappa_const / rBar
        
    !---    compute thermal diffusivity
        alpha = kappa /(3*kB)
        
    !---    compute excess potential energy variance
        eVar = max(0.0d0, e2Bar - eBar*eBar)            !   max function to avoid rounding errors.
        
        
        
    !---    output answer
        print *,""
        write(*,fmt='(3(a,i16))')   "atom count             ",nAtoms
        
        write(*,fmt='(3(a,g16.6))') "mean excess energy     ",eBar ,"             +/- ",sqrt( eVar )," eV"
        if (MDsim) &            
        write(*,fmt='(3(a,g16.6))') "thermal energy 3/2 kT  ",1.5d0*kB*T," eV,     delta = ",eBar-1.5d0*kB*T," eV"
        
        write(*,fmt='(3(a,g16.6))') "excess energy variance ",eVar ," eV^2 "
        write(*,fmt='(3(a,g16.6))') "mean scattering rate   ",rBar ," 1/fs"
        write(*,fmt='(3(a,g16.6))') "therm. cond. x Omega0  ",kappa," eV/fs A^2 1/K = ",kappa*EVFSATOWM," Omega0 W/m/K "
        write(*,fmt='(3(a,g16.6))') "thermal diffusivity    ",alpha," A^2/fs        = ",alpha*1.0d-5   ," m^2/s"
        write(*,fmt='(3(a,g16.6))') "number of defects      ",nDefects,"               = ",nDefects*100.0/nAtoms   ,"%"
        
        
        
        
        
        print *,""
        print *,"done"
        print *,""
        
        
    contains
!---^^^^^^^^





        pure real(kind=real64) function r_i( b0,vF,e,s_0,s_1,s_2,T )    
    !---^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    !*      return the athermal atom scattering rate
            real(kind=real64),intent(in)            ::      b0              !   nearest neighbour separation
            real(kind=real64),intent(in)            ::      vF              !   fermi velocity
            real(kind=real64),intent(in)            ::      S_0,S_1,S_2     !   scattering rate constants
            real(kind=real64),intent(in)            ::      T               !   temperature
            real(kind=real64),intent(in)            ::      e               !   excess potential energy
        
        
            r_i = b0*( S_0*abs(e) + T*(S_1 + T*S_2) ) + vF
            r_i = vF*( S_0*abs(e) + T*(S_1 + T*S_2) ) / r_i            
            
            return
        end function r_i
            
        
        pure real(kind=real64) function r_theta( b0,vF,s_1,s_2,T )    
    !---^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    !*      return the thermal atom scattering rate
            real(kind=real64),intent(in)            ::      b0              !   nearest neighbour separation
            real(kind=real64),intent(in)            ::      vF              !   fermi velocity
            real(kind=real64),intent(in)            ::      S_1,S_2         !   scattering rate constants
            real(kind=real64),intent(in)            ::      T               !   temperature
        
        
            r_theta = b0*( T*(S_1 + T*S_2) ) + vF
            r_theta = vF*( T*(S_1 + T*S_2) ) / r_theta    
            
            return
        end function r_theta
        

            
        subroutine computeDefectsFromHist( n,nAtoms,E,dE,T, sigma, b0,vF,S_0,S_1,S_2 , kbar,r )
    !---^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 
    !*      given that there are nAtoms in the system, 
    !*      and that n have energy between E and E+dE
    !*      find the expected thermal count from the broadened M-B distribution
    !*      and the thermal and defect scattering rates.
    !*      compute the expected scattering rate and the expected defect count
            integer,intent(in)                  ::      n,nAtoms                        !   atom counts
            real(kind=real64),intent(in)        ::      E,dE                            !   energy window          
            real(kind=real64),intent(in)        ::      T                               !   temperature
            real(kind=real64),intent(in)        ::      sigma, b0,vF,S_0,S_1,S_2        !   constants 
            real(kind=real64),intent(out)       ::      kbar                            !   expected defect count
            real(kind=real64),intent(out)       ::      r                               !   expected rate
            
            real(kind=real64)           ::      nBar  
            real(kind=real64)           ::      rtheta,ri
             
            
            rtheta = r_theta( b0,vF,s_1,s_2,T )    
            ri = r_i( b0,vF,e,s_0,s_1,s_2,T )
            
        !---    default assumption: all defects...
            nBar = 0 ; kbar = n
            r = kbar*ri
            
        !---    ... unless energy is zero or MD data
            if (MDsim) then
                nBar = nAtoms*BroadenedMaxwellBoltzmann( E,T,sigma )*dE
                call kDefects( n,nBar,rtheta,ri , kbar, r)
            else if (abs(E) < 1.0d-8) then
                nBar = nAtoms
                kbar = 0
                r = n*rtheta
            end if
                 
            
            return
        end subroutine computeDefectsFromHist
            
    
        pure real(kind=real64) function BroadenedMaxwellBoltzmann( e,T,sigma ) 
    !---^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 
    !*      convolution of Maxwell-Boltzmann distribution with Gaussian width sigma
            real(kind=real64),intent(in)            ::      e           !   energy
            real(kind=real64),intent(in)            ::      T           !   temperature
            real(kind=real64),intent(in)            ::      sigma       !   energy broadening
            
            real(kind=real64)               ::      pp                      
            real(kind=real128)              ::      dd,ee,beta  
            
            real(kind=real128)              ::      sqrt2s  
            
            if (T>0.0d0) then
                beta = 1/(kB*T)
            else
                pp = 0
                if (e==0) pp = 1.0d0
            end if
             
            dd = e/sigma
            ee = e - 2*beta*sigma*sigma
            sqrt2s = sqrt( 2.0_real128 )*sigma
            
            pp = exp( -dd*dd/2 )*sqrt2onPI*sigma*ee                                                       &
               + exp( -beta*(ee+e) )*(sigma*sigma + ee*ee)*(1 + erf( ee/sqrt2s ))  
            
            
            pp = pp * 2*beta*beta*beta                       
            
            BroadenedMaxwellBoltzmann = max(pp,0.0d0)        !   possible for rounding error to give < 0 for very low T    
                                
            return
        end function BroadenedMaxwellBoltzmann        
        
        
        
          subroutine kDefects( n,nBar,rtheta,ri , kbar, r)
    !---^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 
    !*      find the probability that k atoms are defects, given that n are observed, and Nbar are expected to be thermal
    !*          = Poiss(n-k,Nbar) / sum_k=0^n Poiss(n-k,Nbar)
    !*      use this to construct the expected defect count and the rate.
            integer,intent(in)                  ::      n               !   actual count of atoms in energy window
            real(kind=real64),intent(in)        ::      Nbar            !   expected count of atoms in energy window
            real(kind=real64),intent(in)        ::      rtheta,ri       !   thermal and athermal scattering rates
            
            real(kind=real64),intent(out)       ::      kbar            !   number of defects
            real(kind=real64),intent(out)       ::      r               !   scattering rate
            
            
            integer             ::      kk
            real(kind=real128)  ::      ee,dd,iNbar,rr
            
        !---    quick escape if there are no atoms 
            kbar = 0
            r = 0                       
            if (n==0) return
            
            
        !   to find the denominator, start with Poiss(n,Nbar) = Nbar^n exp(-Nbar)/n! = exp( n log Nbar - log(n!) - Nbar )
        !   then Poiss(n-1,Nbar) = Poiss(n,Nbar) * n / Nbar etc
            if (Nbar>1.0d-8) then
                
                if (n>Nbar*4) then
                    
                    kbar = (n-Nbar)
                    r = ri*kbar + rtheta*Nbar
                    
                 
              !   
                    
                    
                else        
                
                    !   Poiss( n-k,Nbar ) = Nbar^(n-k) Exp[ -Nbar ] / (n-k)!
                    !   Log( Poiss ) = (n-k) Log( Nbar ) - Nbar - StirlingsApprox(n-k)
                
                    
                    ee = n*log( Nbar ) - Nbar - StirlingsApprox(n)      !   log Poiss[n,Nbar]
                    ee = exp( ee )                                      !   Poiss[n,Nbar]
                    
                    if (ee<tiny(1.0d0)) then
                        !   such a small probability we are going to end up with problems. Use another approx.
                        if (Nbar>1000) then
                            if (n>Nbar+1000) then
                                kbar = n - Nbar 
                                r = kbar*ri + Nbar*rtheta
                            else if (n<Nbar-1000) then
                                kbar = 0
                                r = n*rtheta
                            else
                                    
                                !   Poiss( n-k,Nbar ) ~ Normal( n-k; mu=Nbar,sigma=Nbar^2 )
                                
                                dd = n*(1 - n/(2*Nbar)) - Nbar/2
                                
                                if (dd > -1000) then
                                  dd = sqrt( Nbar/(2*PI) )*exp( dd )
                                else
                                  dd = 0
                                end if        
                                
                                dd = dd - exp( - Nbar/2 )
                                dd = sqrt( Nbar/(2*PI) )*dd 
                                    
                                    
                                kbar = dd + (n-Nbar)*( erf( (n-Nbar)/sqrt(2*Nbar) ) + erf( sqrt(Nbar/2) ) )/2
                                !write(*,fmt='(a,i8,100g16.6)') "kDefects ",n,nBar,ri,rtheta,n*(1 - n/(2*Nbar)) - Nbar/2,dd,kbar
                                                  
                                r = kbar*ri + (n-kbar)*rtheta
                            end if                            
                        else
                            !   not sure what the problem is.
                            if (n>Nbar) then
                                kbar = n - Nbar 
                                r = kbar*ri + Nbar*rtheta
                            else if (n<=Nbar) then
                                kbar = 0
                                r = n*rtheta
                            end if
                        end if
                        return
                    end if
                    
                    dd = ee
                    iNbar = 1/Nbar
                    r  = ee*n*rtheta
                    !write(*,fmt='(a,i8,100g16.6)') "kDefects ",n,nBar,ri,rtheta,n*log( Nbar ) - Nbar - StirlingsApprox(n),ee,r
                     
                    do kk = 1,n                             !   k is the number of defects
                        ee = ee * iNbar * (n+1-kk)          !   now ee = Poiss(n-k,Nbar)            
                        dd = dd + ee                 
                        kbar = kbar + kk*ee
                        
                        r = r + ee*((n-kk)*rtheta + kk*ri)
                                  
                    end do
                    
                    if (dd>0) then
                        dd = 1/dd
                    else
                        dd = 0
                    end if
                     
                    kbar = kbar * dd
                    r = r * dd
                    
                    !write(*,fmt='(a,i8,100g16.6)') "kDefects ",n,nBar,ri,rtheta,kbar,r
                end if
                                   
            else if (n>Nbar) then
            
                kbar = (n-Nbar)
                r = ri*kbar + rtheta*Nbar
                           
            else
            
                r = rtheta*Nbar
                       
            end if
             
            return
        end subroutine kDefects
                
        
        pure function StirlingsApprox(n) result(lognfact)
    !---^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    !*      returns log(n!)
            integer,intent(in)              ::      n
            real(kind=real64)               ::      lognfact
            real(kind=real64)   ::      xx
            real(kind=real64),dimension(0:20),parameter     ::      KFACT =                                                     &
                    (/  1.0d0,  1.0d0,2.0d0,6.0d0,24.0d0,                                                                       &
                                120.0d0,720.0d0,5040.0d0,40320.0d0,                                                             &
                                362880.0d0,3628800.0d0,3.99168d+7,4.790016d+8,                                               &
                                6.227020800d+9,8.71782912d+10,1.307674368d+12,2.0922789888d+13,                                &
                                3.55687428096d+14,6.402373705728d+15,1.21645100408832d+17,2.43290200817664d+18 /)
            if (n <= 1) then
                lognfact = 0
            else if (n <= 20) then
                lognfact = log( KFACT(n) )
            else if (n <= 1000) then         
                xx = log(real(n,kind=real64))
                lognfact = n*(xx-1.0d0) + xx/2 + 0.91893853320467d0 
                xx = 1.0d0/n
                lognfact = lognfact + (xx/12)*(1 - (xx*xx/30))
            else 
                xx = log(real(n,kind=real64))
                lognfact = n*(xx-1.0d0) + xx/2
            end if
            return
        end function StirlingsApprox       
            
    end program thermalConductivity
        
        
        
            
        
        
                                                                             

