MODULE CALC_DRAG_DES_MOD
CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CALC_DRAG_DES                                           !
!                                                                      !
!  Purpose: This subroutine is called from DES routines. It calls      !
!  functions that calculate the drag force acting on particles. No     !
!  field variables are updated.                                        !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE CALC_DRAG_DES

      use discretelement, only: DES_CONTINUUM_COUPLED
      use discretelement, only: DES_CONTINUUM_HYBRID
      use discretelement, only: DES_EXPLICITLY_COUPLED
      use discretelement, only: DRAG_FC, FC, MAX_PIP
      use drag_gs_des0_mod, only: drag_gs_des0
      use drag_gs_des1_mod, only: drag_gs_des1
      use drag_ss_dem_noninterp_mod, only: drag_ss_dem_noninterp, drag_ss_tfm_noninterp
      use functions, only: IS_NORMAL
      use particle_filter, only: DES_INTERP_GARG
      use particle_filter, only: DES_INTERP_SCHEME_ENUM
!**************************************op***************************************************
      use discretelement, only: F_contact, F_drag, F_LIVE, DES_USR_VAR
      use randomno	
!**************************************op***************************************************                 

      IMPLICIT NONE

! Local variables
!---------------------------------------------------------------------//
      INTEGER :: II
!......................................................................!

! Apply the drag force calculated by the gas phase.
      IF(DES_EXPLICITLY_COUPLED) THEN

         IF(DES_CONTINUUM_COUPLED) THEN
          
!$omp parallel do default(none) private(II) &
!$omp shared(FC, DRAG_FC, MAX_PIP,F_LIVE, F_contact, F_drag)
           
            DO II = 1, MAX_PIP
               IF(IS_NORMAL(II)) &
!**************************************op*************************************************** 
                  F_contact(II,:)=FC(II,:)
                  F_drag(II,:)=DRAG_FC(II,:)                          
                  FC(II,:) = FC(II,:) + DRAG_FC(II,:)
                     IF(ABS(F_contact(II,1))>0.0D0) THEN
                        FC(II,1)=FC(II,1)+F_LIVE(II,1)
                        FC(II,2)=FC(II,2)+F_LIVE(II,2)
                        FC(II,3)=FC(II,3)+F_LIVE(II,3)
                     ENDIF

            	!DES_USR_VAR(2,II)=F_contact(II,:)
            	!DES_USR_VAR(3,II)=F_contact(II,:)
            	!DES_USR_VAR(4,II)=F_contact(II,:)
            	!DES_USR_VAR(5,II)=F_drag(II,:)
           	!DES_USR_VAR(6,II)=F_drag(II,:)
            	!DES_USR_VAR(7,II)=F_drag(II,:)
            	!DES_USR_VAR(8,II)=F_LIVE(II,:)
            	!DES_USR_VAR(9,II)=F_LIVE(II,:)
            	!DES_USR_VAR(10,II)=F_LIVE(II,:)	
!**************************************op***************************************************                   
            ENDDO
!$omp end parallel do
         ENDIF


      ELSE

! Calculate gas-solids drag force on particle
         IF(DES_CONTINUUM_COUPLED) THEN
            SELECT CASE(DES_INTERP_SCHEME_ENUM)
            CASE(DES_INTERP_GARG) ; CALL DRAG_GS_DES0
            CASE DEFAULT; CALL DRAG_GS_DES1
            END SELECT
         ENDIF

! Calculate solids-solids drag force on particle.
         IF(DES_CONTINUUM_HYBRID) THEN
            SELECT CASE(DES_INTERP_SCHEME_ENUM)
            CASE DEFAULT; CALL DRAG_SS_DEM_NONINTERP
            END SELECT
         ENDIF
      ENDIF

      RETURN

   END SUBROUTINE CALC_DRAG_DES

END MODULE CALC_DRAG_DES_MOD
