!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: URS1_DES                                               !
!                                                                      !
!  Purpose: This routine is called within the discrete phase time loop !
!  after the source terms have been calculated but before they are     !
!  applied. The user may insert code in this routine or call user      !
!  defined subroutines.                                                !
!                                                                      !
!  This routine is called from the time loop, but no indices (fluid    !
!  cell or particle) are defined.                                      !
!                                                                      !
!  Author: J.Musser                                   Date: 06-Nov-12  !
!                                                                      !
!  Comments:                                                           !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE USR1_DES

      Use des_rxns
      Use des_thermo
      Use discretelement
      Use run
      Use usr
!**************************************op***************************************************
      USE GET_STL_DATA_MOD
      use stl_preproc_des
      USE run, only: time, dt, tstop, NSTEP
      USE bc, only: BC_V_g     
      USE set_bc0_flow_mod, only: set_bc0_flow  
      USE discretelement, only : grav, FC, TOW, MAX_PIP, PIJK, DES_VEL_NEW, OMEGA_NEW, FC, DES_USR_VAR, F_drag, F_contact, F_LIVE

      USE constant, only : gravity, gravity_x, gravity_y, gravity_z, Pi 
!**************************************op***************************************************      

      IMPLICIT NONE
!**************************************op***************************************************
      DOUBLE PRECISION, DIMENSION(3) :: WALL_VIB 
      INTEGER :: NP, IJK
      WALL_VIB(1) = 0.0d0
      WALL_VIB(2) = 0
!      WALL_VIB(2) = 0.0d0
      WALL_VIB(3) = 0.0d0
      IF (time<= 0.4d0) THEN
            bc_v_g(1) = 0.30d0
      ELSE
            bc_v_g(1) = 0.30d0
      ENDIF
      call set_bc0_flow 

! TRANSLATE STL files       
      is_1_group_id = GET_GROUP_ID('is_0001.stl')
      CALL Translate_STL(is_1_group_id, WALL_VIB)
! Bin the STL to the DES grid (brute force, this could be optimizedto only rebin
! the moving facets).       
      CALL BIN_FACETS_TO_DG

      DO NP=1,MAX_PIP
            IF(PIJK(NP,5)==2) THEN
             DES_VEL_NEW(NP,1)=0.0D0
             DES_VEL_NEW(NP,2)= 0
!             DES_VEL_NEW(NP,2)=0.0D0
             DES_VEL_NEW(NP,3)=0.0D0
             OMEGA_NEW(NP,:)=0.0D0
            ENDIF
      ENDDO

         DO NP = 1,MAX_PIP
            DES_USR_VAR(2,NP)=F_contact(NP,1)
            DES_USR_VAR(3,NP)=F_contact(NP,2)
            DES_USR_VAR(4,NP)=F_contact(NP,3)
            DES_USR_VAR(5,NP)=F_drag(NP,1)
            DES_USR_VAR(6,NP)=F_drag(NP,2)
            DES_USR_VAR(7,NP)=F_drag(NP,3)
            DES_USR_VAR(8,NP)=F_LIVE(NP,1)
            DES_USR_VAR(9,NP)=F_LIVE(NP,2)
            DES_USR_VAR(10,NP)=F_LIVE(NP,3)				  
         ENDDO	

!**************************************op***************************************************

      RETURN
      END SUBROUTINE USR1_DES
