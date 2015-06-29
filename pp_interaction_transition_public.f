	Program pp_interaction_transition_public

	implicit none
	integer bin,nEdec,nEbin,nE_Pr_bin,writ,test,nE_Prdec
	parameter (bin=50000)
	Real*8 Emin,Emax,Estep
	Real*8 m_proton,m_pion,m_muon
	Real*8 E(0:bin)
	Real*8 E_Pr(0:bin),E_Pr_min,E_Pr_max,E_Prstep
	integer i,i_p,i_transition,i_transitione,i_transition_numu
c for the subroutine
	Real*8 Pr_spectrum(0:bin),J_Pr,alpha1,alpha2,E_0,E_cut,Norm
	Real*8 x(0:bin),xb(0:bin),r
	Real*8 E_Pion(0:bin),q_pion1(0:bin),q_pion2(0:bin)
	real*8 sigma_pp(0:bin),c,n_H,mb_cm2,L(0:bin)
c pion0 decay
	Real*8 Ph_spectrum(0:bin),Ph_spectrum_tot(0:bin)
        Real*8 Ph_spectrum_tot_low(0:bin),Ph_spectrum_low(0:bin)
	Real*8 n_tildag,n_tildag_fixed,Ph_spectrum_tot_high(0:bin)
	Real*8 Ph_spectrum_transhigh,Ph_spectrum_translow
c pion charged decay
	Real*8 Nu_mu_spectrum(0:bin),Nu_mu_spectrum_tot(0:bin)
	Real*8 Nu_mu_spectrum_tot_low(0:bin)
	Real*8 Nu_mu_spectrum_tot_high(0:bin)
	Real*8 Nu_tot_low1(0:bin),Nu_tot_low2(0:bin)
c muon decay
	Real*8 Elec_spectrum(0:bin),Elec_spectrum_tot(0:bin)
	Real*8 Nu_Elec_spectrum_tot(0:bin)
	Real*8 Nu_Elec_spectrum_tot_low(0:bin)
        Real*8 Elec_spectrum_tot_low(0:bin)
	Real*8 Elec_spectrum_tot_high(0:bin)
	Real*8 n_e,n_nue, n_tildae,n_tildanue,n_tildanumu
	Real*8 n_tildae_fixed,n_tildanue_fixed,n_tildanum_fixed,
     -Elec_spectrum_translow,Elec_spectrum_transhigh,
     -Nu_Elec_spectrum_translow,Nu_Elec_spectrum_transhigh,
     -Nu_mu_spectrum_translow,Nu_mu_spectrum_transhigh
c summed spectra
	Real*8 Nu_allflavor_spectrum_tot(0:bin)
	Real*8 Nu_mu_tot_spectrum_tot(0:bin)
	Real*8 Nu_mu_tot_spectrum_tot_low(0:bin)
	Real*8 E_M_spectrum_tot(0:bin),E_p_av
	real*8 Flux_nu,Flux_ph,Flux_ph0,Flux_ph0_test, Flux_nu_mu,Flux_pr
c compute the energy density
	real*8 array(0:bin),sum,facteur(0:bin),facteur_tot,seuil(0:bin)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     DESCRIPTION OF THE CODE  CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

ccccccccccccccccccccccccccccccccccccccccccccc
c to compile:
c gfortran -g -fimplicit-none pp_interaction_transition_public.f
c ./a.out
ccccccccccccccccccccccccccccccccccccccccccccc

c following the article: Kelner, Aharonian, Bugayov (2005):

c In Kelner et al (2006), the authors parametrized the secondary particle spectra of the secondary particle having an energy larger than 0.1TeV. The spectra of particles of energy lower than 0.1 TeV is obtained as the result of a superposition of delta functions. The normalization of the low energy end of the spectra is adjusted to the high energy part of the spectrum. In Kelner et al (2006), the normalization is given only for 3 values of the primary proton spectra indices (2, 2.5, 3). 

c In proton-proton interactions, both negatively and positively charged pions are produced in about the same amount, therefore, in Kelner et al (2006), the parametrizations of the secondary particles and of their antiparticles are the same. 

cccccccccccccccccccccccccccccccccccccccccccc
c aim of the program: 

c Based on the parametrization of Kelner et al (2006), this code produces the secondary particle spectra over the entire energy range (above and below E=0.1TeV) by adjusting the low energy part (E<0.1TeV) of the spectra to the high energy end of the spectra for any value of the primary proton spectral index. Following Kelner et al (2006), the spectra for the antiparticles are the same as the one for particles.

c Spectra are computed for each secondary particle separately and total spectra are computed as the sum of the individual spectra. Here I chose to compute the electromagnetic spectra, as the sum of the contributions of the photons produced by pion0 decay + the contribution of the electrons produced in the muon decay (assuming that the electrons lose all their energy radiatively). I also produced the spectra of the neutrinos of all flavors (muonic + electronic) and the total spectra of the muonic neutrinos (produced in the charged pion decay and in the muon decay).

c This code also determines the flux of the secondary particles produced in proton-proton interactions.

ccccccccccccccccccccccccccccccccccccccccccccc
c assumption: low density medium: no interactions of the pions before decays
ccccccccccccccccccccccccccccccccccccccccccccc




CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     START OF THE CODE   CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     MAIN CODE   CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


CCCCCCCCCCCCCCCCCCCCCCCCC  CONSTANTS 

c light speed in cm/s
	c=3.e10
c conversion mb cm^2
	mb_cm2=1.e-27
c mass particles in TeV
	m_pion=1.37e-4
	m_proton=0.938e-3
	m_muon=1.05e-4
	r=(m_muon/m_pion)**2.
	r=0.573


CCCCCCCCCCCCCCCCCCCCCCCCC  PARAMETERS TO FIX:

c 1) choice of the parameters of the proton spectrum: J_Pr=Norm*(E_Pr(i_p)/E_0)**(-alpha1)*exp(-(E_Pr(i_p)/E_cut)
**(alpha2)) (following the notations of Kelner et al (2006), Eq (72).)
	alpha1=2.
	E_cut=1.e3
        Norm=1.
	alpha2=1.
	E_0=1.
	print*,'proton spectrum parameters: spectral index',alpha1,
     -'cutoff energy [TeV]:',E_cut,'normalisation [1/(TeV cm^3)]:',Norm


c 2) binning the Energy of the primary protons in TeV
        E_Pr_min=m_proton
	E_Pr_max=1.e11
	nE_Prdec=1000
	E_Prstep=real(1./nE_Prdec)
	nE_Pr_bin=int(log10(E_Pr_max/E_Pr_min)*nE_Prdec)	
	
	E_Pr(1)=E_Pr_min
	do i_p=2,nE_Pr_bin
	   E_Pr(i_p)=E_Pr(i_p-1)*10.**(1./real(nE_Prdec))
	enddo
       

c 3) density of soft protons in cm-3
	n_H=1e-2

c 4) binning the Energy of the secondary particles in TeV
	Emin=1.e-9
	Emax=1.e9
	nEdec=100
	nEbin=int(log10(Emax/Emin)*nEdec)
	Estep=(log10(Emax/Emin)/nEbin)
	E(1)=Emin

	do i=2,nEbin
	   E(i)=E(i-1)*10.**Estep   	  
	enddo


c 5) in subroutine EDensity: choice of the energy range for the computation of the energy density of the secondary particles
cccccccccccccccccccccccccccccccccccccccccccccc

c initialisation of the variables
	
	do i=1,nE_Pr_bin
	   x(i)=0.
	   xb(i)=0.
	   L(i)=0.
	   Ph_spectrum(i)=0. 
	   Ph_spectrum_tot(i)=0.
	   Ph_spectrum_low(i)=0.
	   Ph_spectrum_tot_low(i)=0.
	   Nu_mu_spectrum_tot(i)=0.
	   Nu_mu_spectrum(i)=0.
	   Pr_spectrum(i)=0.
	   sigma_pp(i)=0.
	   Elec_spectrum_tot(i)=0.
	   Elec_spectrum_tot_high(i)=0.
	   Elec_spectrum_tot_low(i)=0.
	   Elec_spectrum(i)=0.
	   Nu_Elec_spectrum_tot(i)=0.
	   Nu_Elec_spectrum_tot_low(i)=0.
	   Nu_allflavor_spectrum_tot(i)=0.
	   Nu_mu_tot_spectrum_tot(i)=0.
	   Nu_mu_spectrum_tot_low(i)=0.
	   Nu_mu_spectrum_tot_high(i)=0.
	   E_M_spectrum_tot(i)=0.
	   q_pion1(i)=0.
	   q_pion2(i)=0.
	   Nu_tot_low1(i)=0.
	   Nu_tot_low2(i)=0.	   
	enddo

	Flux_ph0=0.
	Flux_ph=0.
	Flux_nu=0.
	sum=0.
	J_Pr=0.
	Ph_spectrum_transhigh=0.
	Ph_spectrum_translow=0.
	Elec_spectrum_transhigh=0.
	Elec_spectrum_translow=0.
	Nu_Elec_spectrum_transhigh=0.
	Nu_Elec_spectrum_translow=0.
	Nu_mu_spectrum_transhigh=0.
	Nu_mu_spectrum_translow=0.
	n_tildag_fixed=0.
	n_tildae_fixed=0.
	n_tildanue_fixed=0.
	n_tildanum_fixed=0.

c call the subroutines
C to produce the primary proton spectrum corresponding to the parameters fixed above (in PARAMETERS TO FIX)
	call Proton(E_Pr,i_p,nE_Pr_bin,nE_Prdec,Pr_spectrum,J_Pr,L,
     -sigma_pp,mb_cm2,bin,alpha1,alpha2,m_proton,E_cut,E_Pion,q_pion1,
     -q_pion2,n_H,c,E_0,E_Pr_min,E_Pr_max,Norm,E_Prstep,E_p_av)
	

	   do i_p=1,nE_Pr_bin	
	      if (E_Pr(i_p).gt.m_proton) then		
	      do i=1,nEbin

		 n_tildag=1.
		 n_tildae=1
		 n_tildanue=1.
		 n_tildanumu=1.

c photons spectra from the pion0 decay
		 call Pion0_Photon(bin,i,i_p,E_Pr,E,Ph_spectrum_tot
     -,L,c,n_H,Pr_spectrum,mb_cm2,alpha1,m_pion,m_proton,
     -nEbin,nE_Pr_bin,E_Pion,q_pion1,q_pion2,Ph_spectrum_tot_low,
     -Ph_spectrum_tot_high,i_transition,E_Prstep,facteur,facteur_tot,
     -seuil,E_p_av,n_tildag,Ph_spectrum_transhigh,Ph_spectrum_translow)
		
c muon neutrinos spectra from the charged pions decay	
		 call Pion_Nu(bin,i,i_p,E_Pr,E,Nu_mu_spectrum_tot,
     -sigma_pp,L,c,n_H,Pr_spectrum,mb_cm2,E_Prstep
     -,alpha1,m_pion,m_proton,nEbin,nE_Pr_bin,E_Pion,q_pion1,q_pion2,r,
     -n_tildanumu,Nu_mu_spectrum_tot_low,Nu_mu_spectrum_tot_high,
     -Nu_mu_spectrum_translow,Nu_mu_spectrum_transhigh,
     -i_transition_numu,Nu_tot_low1,Nu_tot_low2)
		     
c electron, electron neutrino and muon neutrino spectra from the muon decay
		 call Mu_e(bin,i,i_p,E_Pr,E,Elec_spectrum_tot,
     -Nu_Elec_spectrum_tot,sigma_pp,L,c,n_H,Pr_spectrum,mb_cm2
     -,alpha1,m_pion,m_proton,nEbin,nE_Pr_bin,E_Pion,q_pion1,q_pion2,r,
     -Elec_spectrum_tot_low,Elec_spectrum_tot_high,
     -Nu_Elec_spectrum_tot_low,E_Prstep,n_tildae,n_tildanue,
     -Elec_spectrum_translow,Elec_spectrum_transhigh,
     -Nu_Elec_spectrum_translow,Nu_Elec_spectrum_transhigh,
     -i_transitione)

	      enddo
	      
	   endif
	   enddo

	   n_tildag_fixed=Ph_spectrum_transhigh/
     -Ph_spectrum_translow
	    n_tildae_fixed=Elec_spectrum_transhigh/
     -Elec_spectrum_translow
	    n_tildanue_fixed=Nu_Elec_spectrum_transhigh/
     -Nu_Elec_spectrum_translow
	    n_tildanum_fixed=Nu_mu_spectrum_transhigh/
     -Nu_mu_spectrum_translow
		
	   do i=1,nEbin
	      Ph_spectrum_tot(i)=0. 
	      Ph_spectrum_tot_low(i)=0. 
	      Ph_spectrum_tot_high(i)=0. 
	      Elec_spectrum_tot(i)=0.
	      Elec_spectrum_tot_low(i)=0.
	      Elec_spectrum_tot_high(i)=0.
	      Nu_Elec_spectrum_tot(i)=0.
	      Nu_Elec_spectrum_tot_low(i)=0.
	      Nu_mu_spectrum_tot(i)=0.
	      Nu_mu_spectrum_tot_low(i)=0.
	      Nu_mu_spectrum_tot_high(i)=0.
	      Nu_tot_low1(i)=0.
	      Nu_tot_low2(i)=0.
	   enddo

	   do i_p=1,nE_Pr_bin	
	      if (E_Pr(i_p).gt.m_proton) then		
		 do i=1,nEbin
		    call Pion0_Photon(bin,i,i_p,E_Pr,E,Ph_spectrum_tot
     -,L,c,n_H,Pr_spectrum,mb_cm2,alpha1,m_pion,m_proton,
     -nEbin,nE_Pr_bin,E_Pion,q_pion1,q_pion2,Ph_spectrum_tot_low,
     -Ph_spectrum_tot_high,i_transition,E_Prstep,facteur,facteur_tot,
     -seuil,E_p_av,n_tildag_fixed,Ph_spectrum_transhigh,
     -Ph_spectrum_translow)
		    
		     call Mu_e(bin,i,i_p,E_Pr,E,Elec_spectrum_tot,
     -Nu_Elec_spectrum_tot,sigma_pp,L,c,n_H,Pr_spectrum,mb_cm2
     -,alpha1,m_pion,m_proton,nEbin,nE_Pr_bin,E_Pion,q_pion1,q_pion2,r,
     -Elec_spectrum_tot_low,Elec_spectrum_tot_high,
     -Nu_Elec_spectrum_tot_low,E_Prstep,n_tildae_fixed,n_tildanue_fixed,
     -Elec_spectrum_translow,Elec_spectrum_transhigh,
     -Nu_Elec_spectrum_translow,Nu_Elec_spectrum_transhigh,
     -i_transitione)


		     call Pion_Nu(bin,i,i_p,E_Pr,E,Nu_mu_spectrum_tot,
     -sigma_pp,L,c,n_H,Pr_spectrum,mb_cm2,E_Prstep
     -,alpha1,m_pion,m_proton,nEbin,nE_Pr_bin,E_Pion,q_pion1,q_pion2,r,
     -n_tildanum_fixed,Nu_mu_spectrum_tot_low,Nu_mu_spectrum_tot_high,
     -Nu_mu_spectrum_translow,Nu_mu_spectrum_transhigh,
     -i_transition_numu,Nu_tot_low1,Nu_tot_low2)
		     
		     
		 call Nu_allflavor(nEbin,bin,i,i_p,E_Pr,E,Elec_spectrum_tot,
     -Nu_Elec_spectrum_tot,sigma_pp,L,c,n_H,Pr_spectrum,
     -Nu_mu_spectrum_tot,Nu_allflavor_spectrum_tot,
     -Nu_mu_tot_spectrum_tot,
     -Ph_spectrum_tot,E_M_spectrum_tot,q_pion2,
     -Nu_mu_tot_spectrum_tot_low,Nu_mu_spectrum_tot_low,
     -Elec_spectrum_tot_low)

		 enddo
		 
	      endif
	   enddo

c computation of the energy flux(E^2dN/dE) for the secondary particles:	 
	
      call EDensity(Nu_allflavor_spectrum_tot,E,i,sum,nEbin,bin,
     -Estep)
      Flux_nu=sum
      print*, 'Flux all neutrino in TeV', Flux_nu
	
      call EDensity(E_M_spectrum_tot,E,i,sum,nEbin,bin,Estep)
      Flux_ph=sum
      print*, 'Flux e-m in TeV', Flux_ph
      
 


c writing the spectra into files: energy [TeV], "Flux" [1/(cm3 TeV s)]
      writ=1

      if (writ.eq.1) then
        open(unit=7, file='proton_spectrum.dat', 
     -status='unknown')
	open(unit=6, file='photon_spectrum.dat', 
     -status='unknown')
        open(unit=5, file='nu_allflavor_spectrum.dat', 
     -status='unknown')
	
	do i_p=1,nE_Pr_bin	  
	   write(7,*)E_Pr(i_p),Pr_spectrum(i_p)
	enddo

	do i=1,nEbin
	   write(6,*)E(i),Ph_spectrum_tot(i)
           write(5,*)E(i),Nu_allflavor_spectrum_tot(i)
	enddo

	close(5) 
	close(6)
	close(7)

      endif
	
      END


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     SUBROUTINES   CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccc Primary Proton Spectrum ccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	Subroutine Proton(E_Pr,i_p,nE_Pr_bin,nE_Prdec,Pr_spectrum,J_Pr,L,
     -sigma_pp,mb_cm2,bin,alpha1,alpha2,m_proton,E_cut,E_Pion,q_pion1,
     -q_pion2,n_H,c,E_0,E_Pr_min,E_Pr_max,Norm,E_Prstep,E_p_av)
      
	
	integer bin,i_p,nE_Pr_bin,nE_Prdec

	Real*8 E_Pr(0:bin),J_Pr,mb_cm2,m_proton,E_Pr_low(0:bin),E_Prstep
	Real*8 Pr_spectrum(0:bin),L(0:bin),sigma_pp(0:bin)
	Real*8 E_Pion(0:bin),q_pion1(0:bin),q_pion2(0:bin)
	Real*8 E_Pr_min, E_Pr_max,Estep,n_H,c,n_g,E_p_av
	Real*8 Norm,alpha1,alpha2,E_0,E_cut,Eth,v_Pr(0:bin)
	REal*8 Integrated_energydist_CR_TeV2,Integrated_numberdist_CR_TeV


	Eth=1.22e-3	

	Integrated_energydist_CR_TeV2=0.
	Integrated_numberdist_CR_TeV=0.
	E_p_av=0.

	do i_p=1,nE_Pr_bin
	   E_Pion(i_p)=0.17*(E_Pr(i_p)-m_proton)	       
	   E_Pr_low(i_p)=m_proton+(E_Pion(i_p)/0.17)
	   v_Pr(i_p)=c*(1.-(m_proton/E_Pr(i_p))**2.)**(0.5)
	enddo

	do i_p=1,nE_Pr_bin
c condition low energy part
	   if (E_Pr(i_p).lt.1.e3) then	      
	      L(i_p)=log(E_Pr_low(i_p))	      
	      J_Pr=Norm*(E_Pr_low(i_p)/E_0)**(-alpha1)*
     -exp(-(E_Pr_low(i_p)/E_cut)**(alpha2))
	      Pr_spectrum(i_p)=J_Pr
c condition on the cross section
	      if (E_Pr_low(i_p).gt.Eth) then
		 sigma_pp(i_p)=(34.+1.88*L(i_p)+0.25*L(i_p)**2.)*
     -(1.-(Eth/E_Pr_low(i_p))**3.)**2.*(mb_cm2)
	      else
		 sigma_pp(i_p)=0.	 
	      endif
c choice of the speed of the protons: relativistic ot not:	      
c	      q_pion1(i_p)=J_Pr*sigma_pp(i_p)*v_Pr(i_p)*n_H/0.17
	      q_pion1(i_p)=J_Pr* sigma_pp(i_p)*c*n_H
	      	
	   endif
	   

c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c for the high energy part	  
	   if (E_Pr(i_p).gt.0.1) then
	      L(i_p)=log(E_Pr(i_p))
	      J_Pr=Norm*(E_Pr(i_p)/E_0)**(-alpha1)*exp(-(E_Pr(i_p)/E_cut)
     -**(alpha2))
	      Pr_spectrum(i_p)=J_Pr
	     
c condition on the cross section
	      if (E_Pr(i_p).gt.Eth) then
		 sigma_pp(i_p)=(34.+1.88*L(i_p)+0.25*L(i_p)**2.)*
     -(1.-(Eth/E_Pr(i_p))**3.)**2.*(mb_cm2)
		
	      else
		 sigma_pp(i_p)=0.
	      endif
	      
	      q_pion2(i_p)=J_Pr*sigma_pp(i_p)*c*n_H 

	   endif
c computation of the averaged energy of the proton distribution
	   Integrated_energydist_CR_TeV2=Integrated_energydist_CR_TeV2+
     -Pr_spectrum(i_p)*(1.-10.**(-E_Prstep))*(E_Pr(i_p))**2.

	   Integrated_numberdist_CR_TeV=Integrated_numberdist_CR_TeV
     -+Pr_spectrum(i_p)
     -*(1.-10.**(-E_Prstep))*(E_Pr(i_p))

	   E_p_av=Integrated_energydist_CR_TeV2/
     -Integrated_numberdist_CR_TeV

c endo loop proton energy
	enddo
	end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccc Photons Spectrum the From Pion0 Decay ccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	Subroutine Pion0_Photon(bin,i,i_p,E_Pr,E,Ph_spectrum_tot
     -,L,c,n_H,Pr_spectrum,mb_cm2,alpha1,m_pion,m_proton,
     -nEbin,nE_Pr_bin,E_Pion,q_pion1,q_pion2,Ph_spectrum_tot_low,
     -Ph_spectrum_tot_high,i_transition,E_Prstep,facteur,facteur_tot,
     -seuil,E_p_av,n_g,Ph_spectrum_transhigh,Ph_spectrum_translow)

	     
	integer bin,i,i_p,n,nEbin,nE_Pr_bin, i_transition
	Real*8 beta,k,c,n_H,B,Estep,mb_cm2,n_g,alpha1,m_pion,m_proton
	Real*8 Eseuil_pion(0:bin),q_pion1(0:bin),q_pion2(0:bin)
	Real*8 sigma_pp(0:bin),Pr_spectrum(0:bin),L(0:bin),E_Pion(0:bin)
	Real*8 x(0:bin),xb(0:bin),E_Pr(0:bin),E(0:bin),seuil(0:bin)
	Real*8 Ph_spectrum(0:bin),Ph_spectrum_tot(0:bin),f(0:bin),Power
	Real*8 Ph_spectrum_tot_low(0:bin),Ph_spectrum_low(0:bin),E_Prstep
	Real*8 facteur(0:bin),facteur_tot,E_p_av,n_g_fixed
	Real*8 Ph_spectrum_transhigh,Ph_spectrum_translow
	Real*8 Ph_spectrum_tot_high(0:bin)

	Power=0.
	Eseuil_pion(i)=0.
	seuil(i)=0.
	Eseuil_pion(i)=E(i)+(m_pion**2./(4.*E(i)))
	seuil(i)=m_proton+Eseuil_pion(i)/(0.17)

	
c different parametrizations for the two energy ranges

c low energy part	   
	if (E_Pr(i_p).lt.100.) then	  
	   x(i)=E(i)/E_Pion(i_p)
	   
	   if (x(i).lt.1) then

	      f(i)=1.

	      if(E_Pr(i_p).gt.seuil(i)) then
		 Ph_spectrum_tot_low(i)=Ph_spectrum_tot_low(i)+2.*f(i)*
     -(q_pion1(i_p)/0.17)*n_g*(1.-(10.**(-E_Prstep)))*
     -E_Pr(i_p)*(0.17)*
     -(((E_Pr(i_p)-m_proton)*(0.17))**2.-(m_pion)**2.)**(-0.5)
	      endif
	   	
	   endif
	endif
c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

c high energy part   
	if (E_Pr(i_p).gt.0.01) then

	   x(i)=E(i)/E_Pr(i_p)
	   if (x(i).lt.1) then
	      beta=(1.79+0.11*L(i_p)+0.008*L(i_p)*L(i_p))**(-1.)
	      xb(i)=x(i)**beta
	      B=1.30+0.14*L(i_p)+0.011*L(i_p)*L(i_p)       
	      k=(0.801+0.049*L(i_p)+0.014*L(i_p)*L(i_p))**(-1)

	      Ph_spectrum(i)=B*(log(x(i))/x(i))*
     -((1.-xb(i))/(1.+k*xb(i)*
     -(1-xb(i))))**4.*((1./log(x(i)))-((4.*beta*xb(i))/(1.-xb(i)))-
     -((4.*k*beta*xb(i)*(1.-2*xb(i)))/(1.+k*xb(i)*(1.-xb(i)))))
	      Ph_spectrum_tot_high(i)=Ph_spectrum_tot_high(i)+
     -q_pion2(i_p)*Ph_spectrum(i)*(1.-(10.**(-E_Prstep)))

	   endif

	endif
 
	if (E(i).gt.0.05.and.E(i).lt.0.06) then
	   i_transition=i
	endif
	
	
	if (i.lt.i_transition) then
	   Ph_spectrum_tot(i)= Ph_spectrum_tot_low(i)
	else 
	   Ph_spectrum_tot(i)= Ph_spectrum_tot_high(i)	   
	endif


	if (i_p.eq.nE_Pr_bin) then
	   if (i.eq.i_transition) then
	      Ph_spectrum_translow=Ph_spectrum_tot_low(i)	
	      Ph_spectrum_transhigh=Ph_spectrum_tot_high(i)	
	   endif
	endif
	
	end



cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccc Muonic Neutrino Spectrum From Pion Decay cccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	Subroutine Pion_Nu(bin,i,i_p,E_Pr,E,Nu_mu_spectrum_tot,
     -sigma_pp,L,c,n_H,Pr_spectrum,mb_cm2,E_Prstep
     -,alpha1,m_pion,m_proton,nEbin,nE_Pr_bin,E_Pion,q_pion1,q_pion2,r,
     -n_nul,Nu_mu_spectrum_tot_low,Nu_mu_spectrum_tot_high,
     -Nu_mu_spectrum_translow,Nu_mu_spectrum_transhigh,
     -i_transition_numu,Nu_tot_low1,Nu_tot_low2)

	     
	integer bin,i,i_p,nEbin,nE_Pr_bin,i_transition_numu
	Real*8 beta,k,c,n_H,B,E_Prstep,mb_cm2,alpha1,m_pion,m_proton
	Real*8 sigma_pp(0:bin),Pr_spectrum(0:bin),L(0:bin)
	Real*8 x(0:bin),xb(0:bin),E_Pr(0:bin),E(0:bin),E_Pion(0:bin)
	Real*8 Nu_mu_spectrum(0:bin),Nu_mu_spectrum_tot(0:bin)
        Real*8 Nu_mu_spectrum_tot_high(0:bin),seuil1(0:bin)
	Real*8 Nu_tot_low1(0:bin),Nu_tot_low2(0:bin)
        Real*8 Nu_mu_spectrum_tot_low(0:bin),Eseuil_pion(0:bin)
	Real*8 q_pion1(0:bin),q_pion2(0:bin),r,seuil2(0:bin),n_nul
c for the low energy part	
	Real*8 g_nu(0:bin),h1_nu(0:bin),h2_nu(0:bin),f_nu(0:bin)
     -,Nu_mu_spectrum_translow,Nu_mu_spectrum_transhigh

	Eseuil_pion(i)=0.
	seuil1(i)=0.
	seuil2(i)=0.

	Eseuil_pion(i)=m_pion
	seuil1(i)=m_proton+Eseuil_pion(i)/(0.427*0.17)
	seuil2(i)=m_proton+Eseuil_pion(i)/0.17
c low energy part
	if (E_Pr(i_p).lt.100.) then
	   x(i)=E(i)/E_Pion(i_p)



	   g_nu(i)=(3.-2.*r)*(9.*x(i)**2.-6.*log(x(i))-4.*x(i)**3.-5.)
     -/(9.*(1.-r)**2.) 
	   h1_nu(i)=((3.-2.*r)/(9.*(1.-r)**2.))*
     -(9.*r*r-6.*log(r)-4.*r**3.-5.)  
	   h2_nu(i)=(1.+2.*r)*(r-x(i))*(9.*(r+x(i))-4.*
     -(r**2.+r*x(i)+x(i)**2.))/(9.*r**2.)
	   if (x(i).lt.r) then
	      f_nu(i)=h1_nu(i)+h2_nu(i)
	   else
	      f_nu(i)=g_nu(i)
	   endif


	   if (x(i).lt.1) then
	      if (E_Pr(i_p).gt.seuil1(i)) then

		 Nu_tot_low1(i)=Nu_tot_low1(i)+
     -(2./0.427)*
     -(q_pion1(i_p)/0.17)*n_nul*(1.-(10.**(-E_Prstep)))*
     -E_Pr(i_p)*(0.17)*
     -(((E_Pr(i_p)-m_proton)*(0.17))**2.-(m_pion)**2.)**(-0.5)
	      endif
	      if (E_Pr(i_p).gt.seuil2(i)) then
		 Nu_tot_low2(i)=Nu_tot_low2(i)+
     -(2.)*f_nu(i)*
     -(q_pion1(i_p)/0.17)*n_nul*(1.-(10.**(-E_Prstep)))*
     -E_Pr(i_p)*(0.17)*
     -(((E_Pr(i_p)-m_proton)*(0.17))**2.-(m_pion)**2.)**(-0.5)		
	      endif


	      Nu_mu_spectrum_tot_low(i)=
     -Nu_tot_low1(i)+Nu_tot_low2(i)
	   endif
	endif
c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

c high energy part
	if (E_Pr(i_p).gt.0.01) then
	   x(i)=E(i)/E_Pr(i_p)
	   x(i)=x(i)/0.427
	   if (x(i).lt.1) then
	      beta=(1.67+0.111*L(i_p)+0.0038*L(i_p)*L(i_p))**(-1.)
	      xb(i)=(x(i))**beta
	      B=1.75+0.204*L(i_p)+0.010*L(i_p)*L (i_p)       
	      k=1.07-0.086*L(i_p)+0.002*L(i_p)*L(i_p)
		    
	      
	      Nu_mu_spectrum(i)=B*(log(x(i))/x(i))*((1.-xb(i))/(1.+k*xb(i)
     -*(1-xb(i))))**4.*((1./log(x(i)))-((4.*beta*xb(i))/(1.-xb(i)))-
     -((4.*k*beta*xb(i)*(1.-2*xb(i)))/(1.+k*xb(i)*(1.-xb(i)))))
	      Nu_mu_spectrum_tot_high(i)=Nu_mu_spectrum_tot_high(i)+
     -q_pion2(i_p)*Nu_mu_spectrum(i)*(1.-(10.**(-E_Prstep)))
		  
	   endif
	endif

	if (E(i).gt.0.05.and.E(i).lt.0.06) then
	   i_transition_numu=i
	endif
	
	
	if (i.lt.i_transition_numu) then
	   Nu_mu_spectrum_tot(i)=Nu_mu_spectrum_tot_low(i)
	else 
	   Nu_mu_spectrum_tot(i)=Nu_mu_spectrum_tot_high(i)	   
	endif


	if (i_p.eq.nE_Pr_bin) then
	   if (i.eq.i_transition_numu) then
	      Nu_mu_spectrum_translow=Nu_mu_spectrum_tot_low(i)	
	      Nu_mu_spectrum_transhigh=Nu_mu_spectrum_tot_high(i)	
	   endif
	endif


	end



cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccc Muonic Neutrinos, Electronic Neutrinos and Electrons Spectrum From Muon Decay ccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


	Subroutine Mu_e(bin,i,i_p,E_Pr,E,Elec_spectrum_tot,
     -Nu_Elec_spectrum_tot,sigma_pp,L,c,n_H,Pr_spectrum,mb_cm2
     -,alpha1,m_pion,m_proton,nEbin,nE_Pr_bin,E_Pion,q_pion1,q_pion2,r,
     -Elec_spectrum_tot_low,Elec_spectrum_tot_high,
     -Nu_Elec_spectrum_tot_low,E_Prstep,n_e,n_nue,
     -Elec_spectrum_translow,Elec_spectrum_transhigh,
     -Nu_Elec_spectrum_translow,Nu_Elec_spectrum_transhigh,
     -i_transitione)
     
	     

	integer bin,i,i_p,nEbin,nE_Pr_bin,i_transitione
	Real*8 beta,k,c,n_H,B,E_Prstep,mb_cm2,n_e,alpha1,m_pion,m_proton
	Real*8 sigma_pp(0:bin),Pr_spectrum(0:bin),L(0:bin)
	Real*8 x(0:bin),xb(0:bin),E_Pr(0:bin),E(0:bin)
	Real*8 Elec_spectrum(0:bin),Elec_spectrum_tot(0:bin)
	Real*8 Nu_Elec_spectrum(0:bin),Nu_Elec_spectrum_tot(0:bin)
	Real*8 Nu_Elec_spectrum_tot_low(0:bin)
        Real*8 Elec_spectrum_tot_low(0:bin)
	Real*8 Elec_spectrum_tot_high(0:bin)
	Real*8 debug,q_pion2(0:bin),r,E_Pion(0:bin),q_pion1(0:bin)
	Real*8  g_e(0:bin),h1_e(0:bin),h2_e(0:bin),f_e(0:bin)
	Real*8 g_nue(0:bin),h1_nue,h2_nue(0:bin),f_nue(0:bin)
	Real*8 seuil(0:bin),Eseuil_pion(0:bin),n_nue
	Real*8 Elec_spectrum_translow,Elec_spectrum_transhigh
	Real*8 Nu_Elec_spectrum_translow,Nu_Elec_spectrum_transhigh


c low energy part
	Eseuil_pion(i)=0.
	seuil(i)=0.

	Eseuil_pion(i)=m_pion/(r)
	seuil(i)=m_proton+Eseuil_pion(i)/0.17
	E_pion(i_p)=(E_Pr(i_p)-m_proton)*0.17

	if (E_Pr(i_p).lt.100.) then
	   x(i)=E(i)/E_pion(i_p)
	   if (x(i).lt.1) then
	      g_e(i)=(3.-2.*r)*(9.*x(i)**2.-6.*log(x(i))-4.*x(i)**3.-5.)
     -/(9.*(1.-r)**2.) 
	      h1_e(i)=((3.-2.*r)/(9.*(1.-r)**2.))*
     -(9.*r*r-6.*log(r)-4.*r**3.-5.)  
	      h2_e(i)=(1.+2.*r)*(r-x(i))*(9.*(r+x(i))-4.*
     -(r**2.+r*x(i)+x(i)**2.))/(9.*r**2.)
	      if (x(i).lt.r) then
		 f_e(i)=h1_e(i)+h2_e(i)
	      else
		 f_e(i)=g_e(i)
	      endif
	      g_nue(i)=2.*(1.-x(i))*(6.*(1.-x(i))**2.+
     -r*(5.+5.*x(i)-4.*x(i)**2.)+6.*r*log(x(i)))/(3.*(1.-r)**2.)
	      h1_nue=2.*((1.-r)*(6.-7.*r+11.*r**2.-4.*r**3.)+
     -6.*r*log(r))/(3.*(1.-r)**2.)
	      h2_nue(i)=(2.*(r-x(i))*
     -(7.*r**2.-4.*r**3.+7.*x(i)*r-4.*x(i)*r**2.-2.*x(i)**2.-4.*
     -x(i)**2.*r))/(3.*r**2.)
	      if (x(i).lt.r) then
		 f_nue(i)=h1_nue+h2_nue(i)
	      else
		 f_nue(i)=g_nue(i)
	      endif


	      if(E_Pr(i_p).gt.seuil(i)) then

		 Elec_spectrum_tot_low(i)=Elec_spectrum_tot_low(i)+2.*f_e(i)*
     -(q_pion1(i_p)/0.17)*n_e*(1.-(10.**(-E_Prstep)))*
     -E_Pr(i_p)*(0.17)*
     -(((E_Pr(i_p)-m_proton)*0.17)**2.-(m_pion)**2.)**(-0.5)


		 Nu_Elec_spectrum_tot_low(i)=Nu_Elec_spectrum_tot_low(i)+
     -2.*f_nue(i)*(q_pion1(i_p)/0.17)*(1.-(10.**(-E_Prstep)))*n_nue*
     -E_Pr(i_p)*(0.17)*
     -(((E_Pr(i_p)-m_proton)*0.17)**2.-(m_pion)**2.)**(-0.5)

	      endif

	   endif

	endif
	   
c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	   
c high energy part
	if (E_Pr(i_p).gt.0.01) then
	   debug=0.201+0.062*L(i_p)+0.00042*L(i_p)*L(i_p)
	   x(i)=E(i)/(E_Pr(i_p))
	   if (x(i).lt.1) then
	      if (debug.gt.0) then   

		 beta=1./(debug)**(0.25)    
		 xb(i)=(x(i))**beta
		 B=(69.5+2.65*L(i_p)+0.3*L(i_p)*L(i_p))**(-1.)
		 k=(0.279+0.141*L(i_p)+0.0172*L(i_p)*L(i_p))/
     -(0.3+(2.3+L(i_p))**2.)
	   

	     
		 Elec_spectrum(i)=B*((1.+k*(log(x(i)))**2.)**3./
     -(x(i)*(1.+0.3*(xb(i))**(-1))))*(-log(x(i)))**5.
	   
		 Elec_spectrum_tot_high(i)=Elec_spectrum_tot_high(i)+
     -q_pion2(i_p)*Elec_spectrum(i)*(1.-(10.**(-E_Prstep)))
    


	      else
		 debug=-debug
		 beta=1./(debug)**(0.25)    
		 xb(i)=(x(i))**beta
		 B=(69.5+2.65*L(i_p)+0.3*L(i_p)*L(i_p))**(-1.)
		 k=(0.279+0.141*L(i_p)+0.0172*L(i_p)*L(i_p))/
     -(0.3+(2.3+L(i_p))**2.)
	 
	   

		 Elec_spectrum(i)=B*((1.+k*(log(x(i)))**2.)**3./
     -(x(i)*(1.+0.3*(xb(i))**(-1))))*(-log(x(i)))**5.
	   
		 Elec_spectrum_tot_high(i)=Elec_spectrum_tot_high(i)+
     -q_pion2(i_p)*Elec_spectrum(i)*(1.-(10.**(-E_Prstep)))  


	      endif

	   endif
	endif


	if (E(i).gt.0.05.and.E(i).lt.0.06) then
	   i_transitione=i
	endif


	if (i.lt.i_transitione) then
	   Elec_spectrum_tot(i)=Elec_spectrum_tot_low(i)
	   Nu_Elec_spectrum_tot(i)= Nu_Elec_spectrum_tot_low(i)
	else 
	   Elec_spectrum_tot(i)= Elec_spectrum_tot_high(i)
	   Nu_Elec_spectrum_tot(i)= Elec_spectrum_tot_high(i)
	endif

	if (i_p.eq.nE_Pr_bin) then

	if (Elec_spectrum_tot_low(i).ne.0.and.
     -Elec_spectrum_tot_high(i).ne.0.) then
	   if  (i.eq.i_transitione) then
	      Elec_spectrum_translow=Elec_spectrum_tot_low(i)	
	      Elec_spectrum_transhigh=Elec_spectrum_tot_high(i)
	   endif
	endif
	endif



	if (i_p.eq.nE_Pr_bin) then
	   if(Nu_Elec_spectrum_tot_low(i).ne.0..and.
     -Elec_spectrum_tot_high(i).ne.0.) then
	      if  (i.eq.i_transitione) then
		 Nu_Elec_spectrum_translow=Nu_Elec_spectrum_tot_low(i)	
		 Nu_Elec_spectrum_transhigh=Elec_spectrum_tot_high(i)
	      endif	 	      
	   endif
	endif
	end



	



cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccc COMPUTATION OF THE TOTAL PARTICLE SPECTRA cccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	Subroutine Nu_allflavor(nEbin,bin,i,i_p,E_Pr,E,Elec_spectrum_tot,
     -Nu_Elec_spectrum_tot,sigma_pp,L,c,n_H,Pr_spectrum,
     -Nu_spectrum_tot,Nu_allflavor_spectrum_tot,Nu_mu_tot_spectrum_tot,
     -Ph_spectrum_tot,E_M_spectrum_tot,q_pion2,
     -Nu_mu_tot_spectrum_tot_low,Nu_spectrum_tot_low,
     -Elec_spectrum_tot_low)


	integer bin,i,i_p,nEbin
	Real*8 beta,k,c,n_H,B,Estep
	Real*8 sigma_pp(0:bin),Pr_spectrum(0:bin),L(0:bin)
	Real*8 x(0:bin),xb(0:bin),E_Pr(0:bin),E(0:bin)
	Real*8 Ph_spectrum_tot(0:bin),Elec_spectrum_tot(0:bin)
	Real*8 Nu_spectrum_tot(0:bin),Nu_Elec_spectrum_tot(0:bin)
	Real*8 Nu_allflavor_spectrum_tot(0:bin)
	Real*8 Nu_mu_tot_spectrum_tot(0:bin)
	Real*8 E_M_spectrum_tot(0:bin),Nu_spectrum_tot_low(0:bin)
	Real*8 q_pion2(0:bin),Elec_spectrum_tot_low(0:bin)
	Real*8 Nu_mu_tot_spectrum_tot_low(0:bin)

c computation of the spectra of the neutrino spectra of all flavor (muon neutrino from charged pion decay and muon+electron neutrinos from muon decay)
        Nu_allflavor_spectrum_tot(i)=Nu_spectrum_tot(i)+
     -2.*Elec_spectrum_tot(i)
c computation of the total muon neutrino spectra (from pion and muon decay)	
        Nu_mu_tot_spectrum_tot(i)=Nu_spectrum_tot(i)+
     -Elec_spectrum_tot(i)
c computation of the spectra produced by the electrons and the photons produced in the pion 0 decay	
        E_M_spectrum_tot(i)=Elec_spectrum_tot(i)+Ph_spectrum_tot(i)


	
	end


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccc  COMPUTATION OF THE ENERGY DENSITY OF THE SECONDARY PARTICLES ccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	Subroutine EDensity(array,E,i,sum,nEbin,bin,Estep)

	integer bin,i,nEbin
	Real*8 array(0:bin),sum,E(0:bin),Estep,Power,sum_epsilon 
	Real*8 epsilon,E_range,Emin_int,Emax_int

	sum=0.

c choice of the energy range for the computation of the energy density of the secondary particles: 
	Emin_int=2.e-4
        Emax_int=1.e-1

	do i=2,nEbin
	   if (E(i).gt.Emin_int.and.E(i).lt.Emax_int) then

	      sum=sum+array(i)*E(i)**2.*(1.-(10.**(-Estep)))
	      E_range=E_range+E(i)**1.*(1.-(10.**(-Estep)))

	   endif
	      
	enddo

	end
