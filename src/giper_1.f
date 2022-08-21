C     MAIN PROGRAM FOR GEO VARIANT - 1
      EXTERNAL VRGEO,ARG
	double precision ag,bg
	common/GIPER/ag,bg
	common/ellips/ae,be
      COMMON/PROG/ Y(26),YR(26),YY(26),Z(26),RR(26)
      COMMON/RAZM/ N1,N2,N3,N4
      COMMON/FIZP/ RMS,ROG,G,SR,G0,RKG
      COMMON/RS/ RS(6)
      common/chastota/omeg_sob
      COMMON/MUP/ MU(10)
      COMMON/ RIS/ TPV(7,8),RPV(6,9)
      COMMON/CHAS/ CHA(6)
      DOUBLE PRECISION W0,W1,TPS,H,R,D,D1,FG,CHA,Z1,D2,
     1WW,GG,E0,E1,E2,E3,EE,ARG,GI,ENERGY,Hreal,R0
      COMMON/UM/ WW(96),GG(106),E0(106),E1(106),E2(106),E3(106),EE(106)
     1,yrad(106)
      COMMON/ MEN/ MM(10),NN(10),MN(6)
      COMMON/GEO/ H,R,D,D1,D2,Z1,Hreal,R0
	DIMENSION QAA(6,6,6),QBB(3,3,3,3),y10(10)
      COMMON/BC/ GAC(6,6,10),DEC(3,3,3,10),HHC(3,3,3,3,10),
     1VV1(10,10),VV2(10,10,10),VV3(10,10,10,10),UU1(3,10),UU2(3,10,10),
     2UU3(3,10,10,10),UU4(3,10,10,10,10),WW1(10),WW2(10),WW3(6,6,10),
     3WW4(3,3,3,10),VZ2(6,6,10),VZ3(3,3,3,10),UZ2(3,10,10),
     4UZ3(3,6,6,10),UZ4(3,3,3,3,10),PN(110,6),PN1(110,6)
	COMMON /EY/ EPSY,C,EPSYT
      COMMON/EP/GAV(10,10,10),ANN(10),GB0(10,6,10),GB1(10,3,10),
     1DB1(10,3,3,10),HB(3,3,3,3,10),BB1(10,10),BF2(10,10),BF3(3),
     2GF3(10,10,10),DF4(10,10,10,10),ALX1(10),ALY1(10),ALZ1(10),
     3ALX2(10),ALY2(10),BEX2(10,10),BEY2(10,10),GAX3(10,10,10),
     4GAY3(10,10,10),GAZ3(10,10,10),DEX4(10,10,10,10),DEY4(10,10,10,10),
     5DEZ4(10,10,10,10),DES(3,3,3,10),ALS(10),BES(10,10),DE4(3,3,3,10),
     6BEX3(10,10),BEY3(10,10),BEZ3(10,10)
      DOUBLE PRECISION FS, FGP, FGPP
	common/period/Per1,akk,YSIL

C     Command line arguments (CLA block):
C        external force XSILA y = CLAA * sin(omeg_sob*CLAOR * T)

C        CLAAE:
C        CLABE:
C        CLAOR: omega ratio for XSILA
C        CLAA:  amplitude for XSILA
C        CLATK: time range (0, CLATK)
C        CLAH:  liquid height in the tank (wrt ae, be in ellipse case)

      REAL CLAAE, CLABE, CLAOR, CLAA, CLATK, CLAH
      COMMON /CLA/ CLAAE, CLABE, CLAOR, CLAA, CLATK, CLAH

      CHARACTER(LEN=20) :: BUFFER

      CALL GETARG(1, BUFFER)
      READ(BUFFER, *) CLAAE
      CALL GETARG(2, BUFFER)
      READ(BUFFER, *) CLABE

      CALL GETARG(3, BUFFER)
      READ(BUFFER, *) CLAOR
      CALL GETARG(4, BUFFER)
      READ(BUFFER, *) CLAA
      CALL GETARG(5, BUFFER)
      READ(BUFFER, *) CLATK
      CALL GETARG(6, BUFFER)
      READ(BUFFER, *) CLAH

  !    hyperboloid
      ag=1.
	bg=1. 
      ae = 1.
	  be = 1.

      ae = CLAAE
      be = CLABE

      C=5.
!	C=0.
      CALL WGL
	
      N3=3
C   N3 is number of modes studied unti 3-rd order
      N2=6
C   N2 is number of modes studied unti 2-nd order
      N1=10
C Total number of modes
      N4=N1+3
      PI=3.1415926
      N5=1
C Counter for Runge-Kutta step
      N9=2*N4
      N10=10
      N10=5
C   Period of printing wave position
      H=1.D0
      H=CLAH
	R0=0.
!      H=2.D0
C   Depth of liquid
!      H=0.3D0
      R=1.D0
!	R=2.D0
      ROG=1.E+3
      G0=9.81
      RKG=1.
      G=G0*RKG
      T=0.
      HH=0.01 
      TK=50.
      TK=CLATK

      MU(3)=0
      MU(1)=1
      MU(2)=0
 
C If MU(I)=0, motion in this direction is restricted
      DO 1 I=1,N9
      YY(I)=0.
      Z(I)=0.
    1 RR(I)=0.
      NF=23
      NF=22
C NF is number of coordinate functions entrained for solving the linear problem
      CALL VUGEO(H,R)
C   Calculation of integrals in angular direction
	open(777,file='res.txt')
!	open(551,file='coef.txt')
!	open(888,file='resm.xls')



	open(891,file='ampli.txt')
	open(892,file='perem.txt')
	open(893,file='sily.txt')

! ???????????? ??????? ??? ? - ?????? ????? ??????????? obr.xls
! ??? ? yrad.xls, ??????????????? ?? ???????????
!	open(333,file='en.xls')
!     rf0right - ????? ?? ?????
!     psf0right - ????????? ??????????? 
!	open(111,file='rf0right.xls')
!	open(222,file='psf0right.xls')
!     open(711,file='rf0left.xls')
!	open(722,file='psf0left.xls')
!	open(700,file='psf0.xls')
!	open(707,file='psfw.xls')
!      open(1077,file='time_psf.xls')
      open(299,file='ksi.txt')


!	open(1011,file='rf0right-180.xls')
!	open(1022,file='psf0right-180.xls')
!      open(1071,file='rf0left-180.xls')
!	open(1072,file='psf0left-180.xls')
	
!	open(122,file='ps0_ps2_ps3.xls')
!      WRITE (889,104),MN
!  104 FORMAT(10X,' M      ', 7I4)
!      WRITE (777,106),CHA
!      WRITE (889,106),CHA
   
  106 FORMAT(3X,'Frequencies',6f12.5)
      WRITE (888,106),CHA
      CALL VLGEO(N1,N2,N3,NF)
      CALL VMGEO(N1,N2,N3,NF)
      CALL VPGEO(N1,N2,N3,NF)
      CALL VNGEO(N1,N2,N3)
      PRINT 401
  401 FORMAT (3x, 'proshla VL,VM,VP,VN')
!      WRITE(777,401)
!      DO 10 J=1,7
      DO 10 J=1,8
      W0=PI*0.25*(J-1)
      DO 10 I=1,7           
   10 TPV(I,J)=TPS(I,W0)
!      DO 11 J=1,5
!      W0=0.25*(J-1)
      DO 11 J=1,9
      W0=0.125*(J-1)
 
      IF(J.EQ.1) W0=0.01
      DO 11 I=1,6
   11 RPV(I,J)=FS(0,1,W0,0.D0,I,NF)

!      WRITE (777,402) 
!  402 FORMAT(13X,' ARRAY RPV',/)
      
!  202 FORMAT(13X,' ARRAY ',/)
!      WRITE (777,201),VV1

!  226 FORMAT(960x,960f8.3)
 
!!!!!!!!!!!!!!!!!!!!!!!!!!
!     ???  h=1
! 1110 FORMAT(13X,' Beta(1,1)= (MU1 = 0,6325 ) = ',F12.4)
!      WRITE (551,1110),VV1(1,1)
! 1111 FORMAT(13X,' Beta(2,2)= (MU1 = 0,6325 = ) ',F12.4)
!      WRITE (551,1111),VV1(2,2)
! 1112 FORMAT(13X,' Beta(3,3)= (MU0 = 0,8207 = ) ',F12.4)
!      WRITE (551,1112),VV1(3,3)

! 1113 FORMAT(13X,' Beta(4,4)= (MU2 = 0,2951 = )',F12.4)
!      WRITE (551,1113),VV1(4,4)
! 1114 FORMAT(13X,' Beta(5,5)= (MU2 = 0,2951 = )',F12.4)
!      WRITE (551,1114),VV1(5,5)
 
      
! 1119 FORMAT(13X,' VV2(1,2,4)= ( D3 = 0,6403) = ',F12.4)
! 1120 FORMAT(13X,' VV2(1,3,1)= ( D6 =-0,0433) = ',F12.4)
! 1121 FORMAT(13X,' VV2(1,1,3)= ( D5 = 1,0390) = ',F12.4)
! 1122 FORMAT(13X,' VV2(1,4,2)= (-D4 = 0,0560) = ',F12.4)
! 1123 FORMAT(13X,' VV2(1,5,1)= ( D4 =-0,0560) = ',F12.4)
! 1124 FORMAT(13X,' VV2(1,1,5)= (-D3 =-0,6403) = ',F12.4)
! 1125 FORMAT(13X,' VV2(2,3,2)= (-D6 = 0,0433) = ',F12.4)
! 1126 FORMAT(13X,' VV2(2,2,3)= ( D5 = 1,0390) = ',F12.4)
! 1127 FORMAT(13X,' VV2(2,4,1)= (-D4 = 0,0560) = ',F12.4)
! 1128 FORMAT(13X,' VV2(2,5,2)= (-D4 = 0,0560) = ',F12.4)
! 1129 FORMAT(13X,' VV2(2,1,4)= ( D3 = 0,6403) = ',F12.4)
! 1130 FORMAT(13X,' VV2(2,2,5)= ( D3 = 0,6403) = ',F12.4)
! 1131 FORMAT(13X,' VV2(3,2,2)= ( D6 =-0,0433) = ',F12.4)
! 1132 FORMAT(13X,' VV2(3,1,1)= ( D6 =-0,0433) = ',F12.4)
! 1133 FORMAT(13X,' VV2(4,2,1)= (-D4 = 0,0560) = ',F12.4)
! 1134 FORMAT(13X,' VV2(4,1,2)= (-D4 = 0,0560) = ',F12.4)
! 1135 FORMAT(13X,' VV2(5,2,2)= (-D4 = 0,0560) = ',F12.4)
! 1136 FORMAT(13X,' VV2(5,1,1)= ( D4 =-0,0560) = ',F12.4)
!      WRITE (551,1119),VV2(1,2,4)
!      WRITE (551,1120),VV2(1,3,1)
!	WRITE (551,1121),VV2(1,1,3)
!	WRITE (551,1122),VV2(1,4,2)   
!	WRITE (551,1123),VV2(1,5,1)   
!	WRITE (551,1124),VV2(1,1,5)   
!	WRITE (551,1125),VV2(2,3,2)   
!	WRITE (551,1126),VV2(2,2,3)   
!	WRITE (551,1127),VV2(2,4,1)   
!	WRITE (551,1128),VV2(2,5,2)   
!	WRITE (551,1129),VV2(2,1,4)   
!	WRITE (551,1130),VV2(2,2,5)   
!	WRITE (551,1131),VV2(3,2,2)
!	WRITE (551,1132),VV2(3,1,1)
!	WRITE (551,1133),VV2(4,2,1)
!	WRITE (551,1134),VV2(4,1,2)
!	WRITE (551,1135),VV2(5,2,2)
!	WRITE (551,1136),VV2(5,1,1)
!
! 1137 FORMAT(13X,' Delta(1,1,1,1)= (D1 = 0,3920 ) = ',F12.4)
!      WRITE (551,1137),VV3(1,1,1,1)
! 1138 FORMAT(13X,' Delta(1,1,2,2)= (D2 =-0,2416 ) = ',F12.4)
!      WRITE (551,1138),VV3(1,1,2,2)
! 1139 FORMAT(13X,' Delta(1,2,1,2)+Delta(1,2,2,1)=(D1-D2=0,6340)=',F12.4)
!      WRITE (551,1139),VV3(1,2,1,2)+VV3(1,2,1,2)
! 1140 FORMAT(13X,' Delta(2,1,1,2)-Delta(2,2,1,2)=(D1-D2=0,6340)=',F12.4)
!      WRITE (551,1140),VV3(2,1,1,2)+VV3(2,1,2,1)
! 1141     FORMAT(13X,' Delta(1,2,1,2)= (D2 = ) = ',F12.4)
!      WRITE (551,1141),VV3(1,2,1,2)     
! 1142    FORMAT(13X,' Delta(1,2,2,1)= (D2 = ) = ',F12.4)
!      WRITE (551,1142),VV3(1,2,2,1)     
! 1143    FORMAT(13X,' Delta(2,1,1,2)= (D2 = xsila) = ',F12.4)
!      WRITE (551,1143),VV3(2,1,1,2)     
! 1144    FORMAT(13X,' Delta(2,2,1,2)= (D2 = ) = ',F12.4)
!      WRITE (551,1144),VV3(2,1,2,1)     
!
!            
! 1202 FORMAT(13X,'/ ARRAY vv2',/)    
! 1203 FORMAT(13X,'/ ARRAY vv3',/)  
  
!	PRINT 202
!	WRITE (551, 202)
      
!	PRINT 201,VV1
!      WRITE (551,201),VV1
!      PRINT 1202
!	WRITE (551,1202)
     
!      WRITE (551,201),VV2

!	WRITE (551,1203)
	      
!       WRITE (551,201),VV3
!!!!!!!!!!!!!!!!!!!!!!!!!
 
!  201 FORMAT(3X,10F12.7)
!      PRINT 202
!   	WRITE (777,202)
!      DO 81 I=1,N2
!      DO 81 J=1,N2
!      DO 81 K=1,N2
!   81 QAA(J,I,K)=VV2(I,J,K)/VV1(J,J)
!      DO 82 I=1,N3
!      DO 82 J=1,N3
!      DO 82 K=1,N3
!      DO 82 L=1,N3
!   82 QBB(J,I,K,L)=VV3(I,J,K,L)/VV1(J,J)
!      PRINT 201,QAA
!      WRITE (777,201), QAA
!	PRINT 202
!	WRITE (777,202)
!      PRINT 201,QBB
!      WRITE (777,201), QBB
!      PRINT 202
!	WRITE (777,202)
	 
      RMG=PI*ROG*GI(-H,0.D0,ARG)
      RMR=0.2*RMG
      RMS=RMR+RMG
      R1=FG(0.D0)
      DO 2 I=1,N9
    2 Y(I)=0.
 !     Y(2)=0.15 *R1
!      Y(3)=0.05*R1




	PRINT 410
!	WRITE (777,410)
  410 FORMAT (2x, 'doshla do runge-kutta')
  	PRINT 411, Y
!	WRITE (777,411), Y
  411 FORMAT (2x, 10F10.7)


      omeg_par=sqrt(g*WW2(2)/VV1(2,2))
	omeg_sob=omeg_par/sqrt(1.-UU1(1,2)**2*rog/(RMG+RMR)/VV1(2,2))
	Per1=2.*PI/omeg_sob
	a12=(rog/(RMR+RMG))*UU1(1,2)
!      a12=0.
	print *, rog,RMR,RMG,VV1(2,2),WW2(2),UU1(1,2)
      print *, omeg_par,omeg_sob,Per1,a12
!	pause


   13 CALL VRRKKS(T,Y,HH,N9,VRGEO,YY,RR,Z)
         CALL VENERGY(T,Y,N9,N1)
      TT=N5*HH

 !     PRINT 420, Y(2),Y(3)
!	WRITE (777,420), Y(2),Y(3)
  420 FORMAT (2x, f14.5, 3x, F14.7)
!	WRITE (777,101),TT
	Print 101,TT
!      WRITE (890,102),TT
  101 FORMAT(5X,'Time  T=',F7.3)

!      WRITE (777,102) ,Y
!      WRITE (999,102) ,Y(25)
!	WRITE (891,102) ,Y(1)
!	WRITE (892,102) ,Y(2)
!	WRITE (893,102) ,Y(3)
!	WRITE (894,102) ,Y(4)
!	WRITE (895,102) ,Y(5)
!	WRITE (896,102) ,Y(6)
!	WRITE (897,102) ,Y(7)
!	WRITE (898,102) ,Y(8)
!	WRITE (899,102) ,Y(9)
!     WRITE (900,102) ,Y(10)
!	WRITE (901,802) ,tt, Y(11),tt
!	WRITE (902,102) ,Y(12)
!	WRITE (903,102) ,Y(13)
!	WRITE (491,102) ,Y(14)
!	WRITE (492,102) ,Y(15)
!	WRITE (493,102) ,Y(16)
!	WRITE (494,102) ,Y(17)
!	WRITE (495,102) ,Y(18)
!	WRITE (496,102) ,Y(19)
!	WRITE (497,102) ,Y(20)
!	WRITE (498,102) ,Y(21)
!	WRITE (499,102) ,Y(22)
	Print 102,Y
  102 FORMAT(3X,13F9.4)  
      
	do 432 i=1,10
  432 y10(i)=Y(i)
	
	write (891,1891) TT,Y10
 1891 format (11f12.5)

      tf=akk*Per1


      do 332 i=1,10
	if (TT.le.TF) vr=YSIL*TT
	if (TT.gt.TF) vr=YSIL*TF
  332 continue


      write (892,1892) TT,Y(11),Y(12),Y(13),Y(24),Y(25),Y(26),VR
 1892 format (8f12.5)
      
      write (893,1893) TT,RS
 1893 format (7f12.5)

      F0=FG(0.d0)
      F1=FGP(0.d0)
      F2=FGPp(0.d0)
	G1=F0**2
      G2=F1*F0/pi   !!!
      G3=(F1**2+F0*F2)/3./pi   !!!
      S1=0.
      S2=0.
      DO 1023 I=1,N2
 1023 S1=S1+Y(I)**2*ANN(I)
      DO 1229 I=1,N3
      DO 1229 J=1,N3
      DO 1229 K=1,N3
 1229 S2=S2+Y(I)*Y(J)*Y(K)*GAV(I,J,K)
      S0=-(S1*G2+S2*G3)/PI/G1
      aksi=y(2)+Y(3)+y(5)+y(6)+y(8)+y(10)+S0
      
	
	print 802,tt,aksi,S0
	WRITE (299,802) , tt, aksi, s0
  802 format(3f25.5)
!      WRITE (777,103),RS
!	WRITE (811,102),RS(4)
!      WRITE (812,102),RS(5)
!	WRITE (813,102),RS(6)
  103 FORMAT(13X,6F10.4)
      IF(N5.EQ.1) CALL VIDGEO(T,Y,N9,N1)
      N8=MOD(N5,N10)
      IF(N8.EQ.0) CALL VIDGEO(T,Y,N9,N1)
      IF(TK-T) 4,4,5
    5 T=TT
!      CALL VENERGY(T,Y,N9,N1)
      N5=N5+1
      GO TO 13
    4 WRITE (777,105)
  105 FORMAT(/5X,'Termination by time TK')
      STOP
      END






      DOUBLE PRECISION FUNCTION TPS(I,X)
      DOUBLE PRECISION X,Y,Z,X0
      Z=2.*X
      X0=3.*X
      Y=0.
      IF(I.EQ.1) Y=DSIN(X)
      IF(I.EQ.2) Y=DCOS(X)
      IF(I.EQ.3) Y=1.
      IF(I.EQ.4) Y=DSIN(Z)
      IF(I.EQ.5) Y=DCOS(Z)
      IF(I.EQ.6) Y=DSIN(X0)
      IF(I.EQ.7) Y=DCOS(X0)
      TPS=Y
      RETURN
      END

      DOUBLE PRECISION FUNCTION TPSP(I,X)
      DOUBLE PRECISION X,Y,Z,X0
      Y=0.
      Z=2.*X
      X0=3.*X
      IF(I.EQ.1) Y=DCOS(X)
      IF(I.EQ.2) Y=-DSIN(X)
      IF(I.EQ.3) Y=0.
      IF(I.EQ.4) Y=2.*DCOS(Z)
      IF(I.EQ.5) Y=-2.*DSIN(Z)
      IF(I.EQ.6) Y=3.*DCOS(X0)
      IF(I.EQ.7) Y=-3.*DSIN(X0)
      TPSP=Y
      RETURN
      END

      FUNCTION YSILA(T)
	COMMON /EY/ EPSY,C,EPSYT
	S1=0.
      S2=-C*EPSY
	cc=c/5.
      s3=0.
	if(EPSY.ge.0.0) s3=-cc*EPSY
!      Y=5000.0
!      Y=5.
 !     Y=1.
!      Y=0.
       Y=0.9
       TAU=0.5
      S1=Y 
      IF( T.GE.TAU) S1=0.
 !    YSILA=Y
      YSILA=0.
      RETURN
      END

      FUNCTION XSILA(T)
	common/period/Per1,akk,YSIL
	common/chastota/omeg_sob

      REAL CLAAE, CLABE, CLAOR, CLAA, CLATK, CLAH
      COMMON /CLA/ CLAAE, CLABE, CLAOR, CLAA, CLATK, CLAH
	akk=0.4
      tf=akk*Per1
	YSIL=0.7
	Y=YSIL
	if(t.ge.tf) Y=0.
!      vibration 
      om=omeg_sob*0.5
      om=omeg_sob*CLAOR
	A=0.5
      A=CLAA
      y=A*sin(om*T)

	XSILA=Y
      RETURN
      END

      FUNCTION ZSILA(T)
      COMMON/FIZP/ RMS,ROG,G,SR,G0,RKG
      Y=0.
      Y=G+0.7*sin(6.24*T)
	Y=G+0.7*sin(4.*T)
	Y=0.
      ZSILA=Y
      RETURN
      END

      DOUBLE PRECISION FUNCTION ARG(X)
      IMPLICIT REAL*8 (A-H,O-Z)
      ARG=FG(X)**2
      RETURN
      END

      FUNCTION INDX(K,M)
      I=K+M-1
      IF(M.EQ.0) I=I+1
      INDX=I
      RETURN
      END


      SUBROUTINE VUGEO(H,W)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/MEN/ MM(10),NN(10),MN(6)
      COMMON/FORMA/ PSF(25,6),PS0(25),PSZ(6),AF0,AF1
      IPR=1
!      IPR=0
!     PSZ() - ????????? ?????????? ?? ??????     
      PSZ(1)=0.25
      PSZ(1)=0.25
      PSZ(2)=0.15
      PSZ(3)=0.15
      PSZ(4)=0.15
      PSZ(5)=0.1
      PSZ(6)=0.1
      DO 10 I=1,6
C      PSZ(I)=0.3
      PSZ(I)=0.2
!      PSZ(I)=0.
!      PSZ(I)=0.15
   10 CONTINUE

      M=1
      NF1=1
      NF2=4
!     ??????  ??????? ??? ????????? ?????? ?=1 ? 1-?? ?????? ?? ??????? 
!     F1=R1*SINX ;F2=R1*COSX
!     ??? ????????? ?????? ?=1 ? 4-?? ?????? ?? ???????:   F9=R4*SINX ;F10=R4*COSX
 	
	CALL VFORMG(H,W,M,NF1,NF2,IPR)
      M=0
      NF1=2
      NF2=6
!     ??? ????????? ?????? ?=0
!     ??? 2-?? ??????????? ?????? F3=R2
!     ??? 6-?? ??????????? ?????? F8=R6
      CALL VFORMG(H,W,M,NF1,NF2,IPR)
      M=2
      NF1=3
      NF2=0
!     ??? ????????? ?????? ?=2 ? 3-?? ?????? ?? ??????? 
!     F4=R3*SIN2X ;F5=R3*COS2X  
      CALL VFORMG(H,W,M,NF1,NF2,IPR)
      M=3
      NF1=5
!     ??? ????????? ?????? ?=3 ? 5-?? ?????? ?? ??????? 
!     F6=R5*SIN3X ;F7=R5*COS3X  
      CALL VFORMG(H,W,M,NF1,NF2,IPR)
!     NN(10)- ????? ??????? R ?? ???????
!     MM(10)- ????? ?????????????????? ????? 
!     MN(NF1) - ???????? ??????,??????????????? ??????? ?? ??????? NF1 ? NF2
!     F(I)=R(NN(I))*TPS(MM(I))
!     TPS() - ?????????? ?????????????????? ????? ??????? F
      NN(1)=1
      MM(1)=1
!     ????????????? M=1;NF1=1;F1=R1*SINX
      NN(2)=1
      MM(2)=2
!     ????????????? M=1;NF1=1;F2=R1*COSX
      NN(3)=2
      MM(3)=3
!     ????????????? M=0;NF1=2;F3=R2
      NN(4)=3
      MM(4)=4
!     ????????????? M=2;NF1=3;F4=R3*SIN2X
      NN(5)=3
      MM(5)=5
!     ????????????? M=2;NF1=3;F5=R3*COS2X
      NN(6)=6
      MM(6)=3
!     ????????????? M=0;NF2=6;F6=R6
      NN(7)=5
      MM(7)=6
!     ????????????? M=3;NF2=5;F7=R5*SIN3X
      NN(8)=5
      MM(8)=7
!     ????????????? M=3;NF2=5;F8=R5*COS3X
      NN(9)=4
	MM(9)=1
!     ????????????? M=1;NF2=4;F9=R4*SINX
      NN(10)=4
      MM(10)=2
!     ????????????? M=1;NF2=4;F9=R4*COSX
      MN(1)=1
!     ????????????? M=1;NF1=1
      MN(2)=0
!     ????????????? M=0;NF1=2
      MN(3)=2
!     ????????????? M=2;NF1=3
      MN(4)=1
!     ????????????? M=1;NF2=4
      MN(5)=3
!     ????????????? M=3;NF2=5
      MN(6)=0
!     ????????????? M=0;NF2=6
      RETURN
      END
      
	SUBROUTINE VIDGEO(T,Y,N9,N1)
      DIMENSION Y(N9),SK(20,8)
      COMMON/ MEN/ MM(10),NN(10)
      COMMON/ RIS/ TPV(7,8),RPV(6,9)
      COMMON/FGEO/ F0,F1,F2,F3,PI
      DOUBLE PRECISION F0,F1,F2,F3,PI,H,R,D,D1,S
	  DOUBLE PRECISION FS, FGP, FGPP, FG
      COMMON/GEO/ H,R,D,D1
      COMMON/RAZM/ N7,N2,N3,N4
      COMMON/EP/GAV(10,10,10),ANN(10),GB0(10,6,10),GB1(10,3,10),
     1DB1(10,3,3,10),HB(3,3,3,3,10),BB1(10,10),BF2(10,10),BF3(3),
     2GF3(10,10,10),DF4(10,10,10,10),ALX1(10),ALY1(10),ALZ1(10),
     3ALX2(10),ALY2(10),BEX2(10,10),BEY2(10,10),GAX3(10,10,10),
     4GAY3(10,10,10),GAZ3(10,10,10),DEX4(10,10,10,10),DEY4(10,10,10,10),
     5DEZ4(10,10,10,10),DES(3,3,3,10),ALS(10),BES(10,10),DE4(3,3,3,10),
     6BEX3(10,10),BEY3(10,10),BEZ3(10,10)
      G1=PI*F0**2
      G2=PI*F0*F1
      G3=PI/3.*(F1**2+F0*F2)
      S1=0.
      S2=0.
      DO 10 I=1,N2
   10 S1=S1+Y(I)**2*ANN(I)
      DO 11 I=1,N3
      DO 11 J=1,N3
      DO 11 K=1,N3
   11 S2=S2+Y(I)*Y(J)*Y(K)*GAV(I,J,K)
      S0=-(S1*G2+S2*G3)/PI/G1
	      
      DO 1 I=1,9
      J=2*I-1
      M=J+1
      DO 1 L=1,8    
       S=S0
      DO 2 K=1,N1
      KI=NN(K)
      KJ=MM(K)
    2 S=S+Y(K)*TPV(KJ,L)*RPV(KI,I)
!      U=0.125*(I-1)*FG(S*H)/F0
       U=0.125*(I-1)*FG(S*H/F0)
	  IF(I.EQ.1) U=0.01*FG(S*H/F0)
! ALFA=1; U=r	; r=ALFA*FG(z) ; z=H*BETA
!     S=BETA; SK(J,L)=r; SK(M,L)=z
      SK(J,L)=U
!    1 SK(M,L)=S/F0
    1 SK(M,L)=H*S/F0
      DO 3 I=1,8 
      SK(19,I)=H
    3 SK(20,I)=45*(I-1)
!      PRINT 4,SK
!	write (777,4) SK
!      write (500,5),T
!      write (500,4),SK
!      
!	write (122,15),T
!      write (122,5),S0
!	write (122,5),G3

!     90	      
	 I=3      
      DO 14 L=1,9
	K=2*L-1
	M=K+1
!      write (111,5) SK(K,I)
!      write (222,5) SK(M,I)
   14 continue
      
!      write (111,15),T
!	write (222,15),T

!     270
      I=7
      DO 16 L=1,9
	K=2*L-1
	M=K+1
!      write (711,5) -SK(K,I)
!      write (722,5) SK(M,I)
   16 continue
!      write (711,15),T
!	write (722,15),T

!     0	      
	 I=1      
      DO 24 L=1,9
	K=2*L-1
	M=K+1
!      write (1011,5) SK(K,I)
!	IF(K.EQ.1)  write (700,5) SK(M,I)
!	IF(K.EQ.9)  write (707,5) SK(M,I)
!      write (1022,5) SK(M,I)
   24 continue
!      write (1011,15),T
!	write (1022,15),T
!      write (1077,5),T
!     180
      I=5
      DO 26 L=1,9
	K=2*L-1
	M=K+1
 !     write (1071,5) -SK(K,I)
!      write (1072,5) SK(M,I)
   26 continue
!      write (1071,15),T
!	write (1072,15),T
!    5 FORMAT(3X,F10.4)
!    4 FORMAT(3X,20F10.4,'  Displacement KSI   Asimut')
!   15 FORMAT(3X,F10.4,'=T')
      RETURN
      END

      !     ?????? ????????? ??????????
	DOUBLE PRECISION FUNCTION FG (X)
      DOUBLE PRECISION X,H,R,D,D1,F0,D2,Z1,Hreal,R0,ae,be
      COMMON/GEO/ H,R,D,D1,D2,Z1,Hreal,R0

      REAL CLAAE, CLABE, CLAOR, CLAA, CLATK, CLAH
      COMMON /CLA/ CLAAE, CLABE, CLAOR, CLAA, CLATK, CLAH
!     Y=x**2      
!	 D2=DSQRT(X+H)
!	 F0=D2
!     Y=1/2*x**2
!      F0=dsqrt(2.D0)*D2 
!     Y=2*x**2      
!      F0=D2/dsqrt(2.D0)
!     ?????
!      F0=R*(X+H)
!     cylindr
!      F0=R    
!
!     IF(H.Gt.1.) go to 11    
!   11	 IF(X.Gt.Z1) F0=X+H  
!      F0=(1.d0-R0)*(X+H)+R0
      ae=1.d0
	be=1.d0

      ae = CLAAE
      be = CLABE

      F0=ae/be*sqrt(be**2-(X+H-be)**2)
      FG=F0
      RETURN
      END

!     ?????? ???????????
      DOUBLE PRECISION FUNCTION FGP(X)
      DOUBLE PRECISION X,H,R,D,D1,F0P,D2,Z1,Hreal,R0,ae,be
      COMMON/GEO/ H,R,D,D1,D2,Z1,Hreal,R0

      REAL CLAAE, CLABE, CLAOR, CLAA, CLATK, CLAH
      COMMON /CLA/ CLAAE, CLABE, CLAOR, CLAA, CLATK, CLAH

!     Y=x**2 
!       F0P=0.5/DSQRT(X+H)    
!     Y=1/2*x**2
!	  F0P=dsqrt(2.D0)*0.5/DSQRT(X+H) 
!     Y=2*x**2      
!      F0P=0.5/(dsqrt(2.D0)*DSQRT(X+H))
!     ?????
!      F0P=R
!    cylindr
!
!      F0P=0.
!         IF(H.Gt.1.) go to 11  
!  11	 IF (X.Gt.z1) F0P=1.
!      F0P=(1.d0-R0)
      ae=1.d0
	be=1.d0

      ae = CLAAE
      be = CLABE

      F0P=-(ae/be)*(X+H-be)/sqrt(be**2-(X+H-be)**2)
      FGP=F0P
      RETURN
      END
 
      DOUBLE PRECISION FUNCTION FGPP(X)
      DOUBLE PRECISION X,H,R,D,D1,D2,z1,Hreal,r0
      COMMON/GEO/ H,R,D,D1,D2,Z1,Hreal,r0
      common/ellips/ae,be
	DOUBLE PRECISION F0,ae,be,ce

      REAL CLAAE, CLABE, CLAOR, CLAA, CLATK, CLAH
      COMMON /CLA/ CLAAE, CLABE, CLAOR, CLAA, CLATK, CLAH

       D2=DSQRT(X+H)
!     Y=x**2
!      F0=-0.25/DSQRT(X+H)/(X+H)
!     Y=1/2*x**2
!      F0=-dsqrt(2.D0)*0.25/D2**3
!     Y=2*x**2	
!       F0=-0.25/(dsqrt(2.D0)*D2**3)
!   	 IF(X.Gt.Z1) F0=0.
!     ?????
      ae=1.d0
	be=1.d0

      ae = CLAAE
      be = CLABE

      ce=be*be-(X-be+H)**2
      F0=-ae/be*((-be+H+X)**2)/ce**(1.5)+1./sqrt(ce)
      FGPP=F0
      RETURN
      END

      DOUBLE PRECISION FUNCTION FGP3(X)
      DOUBLE PRECISION X,H,R,D,D1,D2,z1,Hreal,r0
      COMMON/GEO/ H,R,D,D1,D2,Z1,Hreal,r0
      DOUBLE PRECISION F0,ae,be,ce
	common/ellips/ae,be

      REAL CLAAE, CLABE, CLAOR, CLAA, CLATK, CLAH
      COMMON /CLA/ CLAAE, CLABE, CLAOR, CLAA, CLATK, CLAH

       D2=DSQRT(X+H)
!     Y=x**2
!	F0=0.375/DSQRT(X+H)/(X+H)/(X+H)
!     Y=1/2*x**2
!      F0=dsqrt(2.D0)*0.375/D2**5
!     Y=2*x**2
!       F0=0.375/(dsqrt(2.D0)*D2**5)
!   	 IF(X.Gt.z1) F0=0. 
!     ?????
      ae=1.d0
	be=1.d0

      ae = CLAAE
      be = CLABE

      ce=be*be-(X-be+H)**2
      F0=-ae/be*(3.*(-be+H+X)**3/ce**2.5+3.*(-be+H+X)/ce**1.5)          
      FGP3=F0
      RETURN
      END

 


C     SOME DIFFEFENCIALS FROM GARMONIC POLINOMS FOR GEO-BLOCK
      DOUBLE PRECISION FUNCTION FS(I,J,AL,BE,K,NF)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/GEO/ H,U,D,D1
      COMMON/FORMA/ PSF(25,6),PS0(25),PSZ(6),AF0,AF1
      COMMON/MEN/ MM(10),NN(10),MN(6)
!     FS(I,J,AL,BE,K,NF) - I - ????? ??????????? ?? Z;J - ????? ??????????? ?? R;
!     k - ????? ?? ???????; NF - ???????? ?????
      Z=BE*H
      R=AL*FG(Z)
      E=Z-PSZ(K)
!     PSZ(K) - ???????? ?? ???????????? ?????????? ??????
!     E - ?????? ?? ?????
!     NF - ???-?? ??????? ? ?????????? ????(?????????? ?????? ????????????)
          
      S=0.D0
      DO 1 IJ=1,NF
      W=0.D0
      M=MN(K)
!     ? - ???????? ?????
      L=IJ+M-1
!     ???????? ???????? ??? ????????????? ????????? W
      IF(M.EQ.0) L=L+1
!     ??? ??????? ?????? ???????? ??????
      IF(I.EQ.0.AND.J.EQ.0) W=WF(M,L,R,E)
      IF(M.EQ.0) W=W-PS0(L)
!     ???????? ??????
      IF(I.EQ.0.AND.J.EQ.1) W=WZ(M,L,R,E)
      IF(I.EQ.0.AND.J.EQ.2) W=WZZ(M,L,R,E)
      IF(I.EQ.0.AND.J.EQ.3) W=WZZZ(M,L,R,E)
C      IF(I.EQ.0.AND.J.EQ.4) W=WZ4 (M,L,R,E)
      IF(I.EQ.1.AND.J.EQ.0) W=WR  (M,L,R,E)
      IF(I.EQ.1.AND.J.EQ.1) W=WZR (M,L,R,E)
      IF(I.EQ.1.AND.J.EQ.2) W=WZZR(M,L,R,E)
C      IF(I.EQ.1.AND.J.EQ.3) W=WRZ3(M,L,R,E)
      IF(I.EQ.2.AND.J.EQ.0) W=WRR (M,L,R,E)
      IF(I.EQ.2.AND.J.EQ.1) W=WZRR(M,L,R,E)
C      IF(I.EQ.2.AND.J.EQ.2) W=WR2Z2(M,L,R,E)
C      IF(I.EQ.3.AND.J.EQ.0) W=WRRR (M,L,R,E)
C      IF(I.EQ.3.AND.J.EQ.1) W=WR3Z (M,L,R,E)
    1 S=S+W*PSF(IJ,K)
!     PSF(IJ,K) - ??????, ??????? ???????????? ?? ??????? ?????? ?? ?.???????? ? ??????. ???????,
!     ??????????? ? VFORMG() ? ?????????? ???? ?(??? i=j=0) ? ??? ???????????
      FS=S
      RETURN
      END

C    CALCULATION OF FORMS IN NONCYLINDRIC TANK
      SUBROUTINE VFORMG(H2,W2,M,NF1,NF2,IPR)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/CHAS/ CHA(6)
      COMMON/FORMA/ PSF(25,6),PS0(25),PSZ(6),AF0,AF1
      DIMENSION PN(110,6),PN1(110,6),AN(3),EM(110,3)
      COMMON/UM/ WW(96),GG(106),F0(106),F1(106),F2(106),F3(106),FE(106)
     1,yrad(106)
      COMMON/GEO/ H,W,V,D1
      COMMON/BC/ VF(6,25,106)
      COMMON/EP/A(22,22),B(22,22),C(22),D(22,22),F(22,22),
     1E(22),Q(22),P(22,22),QQ(22,22),QR(22,22),QA(22,22),QB(22,22),
     2X(22,22),Y(22,22),DL(14,12)
!	open(777,file='res.xls')
	open(888,file='resm.xls')
!      open(889,file='prob.xls')
!      open(999,file='vel.xls')
!	 open(890,file='time.xls')
!	 open(891,file='a1.xls')
!	open(812,file='ry.xls')
!	open(811,file='rx.xls')
!	open(813,file='rz.xls')
!	open(500,file='psi.xls')
!	open(555,file='obr.xls')
!	open(556,file='yrad.xls')
!
!      open(901,file='y1.xls')
!      open(902,file='y2.xls')
!      open(903,file='y3.xls')
!	open(491,file='v1.xls')
!	open(492,file='v2.xls')
!	open(493,file='v3.xls')
!	open(494,file='v4.xls')
!	open(495,file='v5.xls')
!	open(496,file='v6.xls')
!	open(497,file='v7.xls')
!	open(498,file='v8.xls')
!	open(499,file='v9.xls')
!	open(333,file='en.xls')
!      open(111,file='rf0right.xls')
!	open(222,file='psf0right.xls')
!	open(711,file='rf0left.xls')
!	open(722,file='psf0left.xls')
!	open(700,file='psf0.xls')
!	open(707,file='psfw.xls')
!	open(1077,file='time_psf.xls')
!
!	open(1011,file='rf0right-180.xls')
!	open(1022,file='psf0right-180.xls')
!      open(1071,file='rf0left-180.xls')
!	open(1072,file='psf0left-180.xls')
      H=H2
!	print 51,H
!   51 format('na vkhode h=', F12.5)
!      pause
      W=W2
      Z1=-PSZ(NF1)
      N=22
C   Z0 - initial displacement (???)
      Z0=0.0D0
	Z0=-Z0
!	Z1=0.
      H=H-Z1
	print 52,H
   52 format('posle Z1  h=', F12.5)
!      pause

      H1=0.2D0
	R=1.d0
      HE=0.2
	HE=0.
	print 53,HE
   53 format('HE=', F12.5)
!      pause

      HM=H
	print 54,H
   54 format('HM=', F12.5)
!      pause

C        Initial depth (real one increader for PSZ(i) for every mode
      DO 1 I=1,N
      C(I)=0.
      E(I)=0.
      Q(I)=0.
      DO 1 J=1,N
      A(I,J)=0.
      B(I,J)=0.
      D(I,J)=0.
      P(I,J)=0.
      QQ(I,J)=0.
    1 QR(I,J)=0.
      DO 2 K=1,106
      GG(K)=0.D0
      DO 2 I=1,6
      DO 2 J=1,N
    2 VF(I,J,K)=0.D0
      H=HM
      HH=H+HE
      PRINT 202,M,M,M,M,M,M,M
!	WRITE (777,202), M,M,M,M,M
  202 FORMAT(10X,' M=', 7I4)
      WRITE (888,202), M
      
  615 FORMAT(10X,' NF1=', 7I4)
      WRITE (888,615), NF1

      
  617 FORMAT(10X,' NF2=', 7I4)
      WRITE (888,617), nf2
 
	IV=0

      MU=0

!     Z1=-HE


      print 55,HH,Z0,Z1
   55 format('na pervom vkhode v VSGEO HH= Z0= Z1=', 3F12.5)
!      pause

      CALL VSGEO(Z0,Z1,HH,A,B,N,M,IV)

	 
      CALL VNROOT(N,B,A,C,D)
      HH=HM
      H=HM
	print 56,H
   56 format('na vtorom vkhode H=', F12.5)
!      pause

      CALL VSGEO(Z0,Z1,H,A,B,N,M,IV)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
C       for the initial height 
      DO 80 I=1,N
      DO 80 J=1,N
      QA(I,J)=A(I,J)
   80 QB(I,J)=B(I,J)
      CALL VNROOT(N,QB,QA,C,P)
  120 CONTINUE
      MU=MU+1
      IF(IPR.EQ.0) GO TO 805
!      PRINT 10
!	WRITE(777,10)
!   10 FORMAT(4X,'Classic variant')
      DO 20 I=1,N
      IF(C(I)-1.D-4) 24,24,25
   25 AT=1.D0/C(I)
      E(I)=AT
      C(I)=DSQRT(AT)
      GO TO 26
   24 C(I)=0.
      E(I)=0.
   26 CONTINUE
   20 CONTINUE



      PRINT 11
   11 FORMAT(3X,'Squares of frequencies')
      PRINT 12,E
   12 FORMAT(3X,10F12.6)
       
!	WRITE (777,11)
!      WRITE (777,12),E

      PRINT 13
!	WRITE (777,13)
   13 FORMAT(3X,'Eigen frequencies')
      PRINT 12,C
!	WRITE (777,12),C 
  805 CONTINUE
      J=96
      DO 61 I=1,3
      S=0.D0
      DO 62 K=1,N
   62 S=S+VF(5,K,J)*P(K,I)
   61 AN(I)=S
      DO 30 J=1,106
      DO 30 I=1,3
      TS=0.D0
      DO 35 K=1,N
      TS=TS+VF(6,K,J)*P(K,I)
   35 CONTINUE
      EM(J,I)=TS/AN(I)
   30 CONTINUE
      DO 31 J=1,106
      DO 31 I=1,3
      T=0.D0
      S=0.D0
      S1=0.D0
	DO 32 K=1,N
      T=T+VF(5,K,J)*P(K,I)
      S=S+VF(2,K,J)*P(K,I)
      S1=S1+VF(4,K,J)*P(K,I)
   32 CONTINUE
      PN(J,I+3)=T/AN(I)
	PN1(J,I)=S1/AN(I)
   31 PN(J,I)=S /AN(I)
      IF(IPR.EQ.0) GO TO 806
      PRINT 33
   33 FORMAT(2X,'epure of displacements')

!  WRITE(889,33)
      PN(110,1)=1000.
      DO 79 I=1,6
   79 PN(110,I)=9999.9999
      DO 88 J=1,3
      DO 88 I=1,3
   88 PN(I+106,J)=AN(J)
      PRINT 12,PN
      PRINT 337
  337 FORMAT(2X,'epure of displacements bottom')
      PRINT 12,PN1

!	WRITE(888,40)
      PRINT 40
!	WRITE(777,40)
   40 FORMAT(/4X,'modified variant')
  806 CONTINUE
      DO 303 I=1,3
      J=I+3
      XX=0.D0
      YY=0.D0
      DO 301 K=1,96
      ER=0.5D0*(GG(K)+1.D0)
      XX=XX+WW(K)*PN(K,J)*EM(K,I)*ER
      YY=YY+WW(K)*EM(K,I)*EM(K,I)*ER
  301 CONTINUE
      ALAM=XX/YY
      PRINT 302,ALAM
!	write (777,302), ALAM

 1655 format(10X,'ENERGY',F10.6)
      
      ENERGY=XX+YY

!      write (777,1655), ENERGY
	IF(Z1.EQ.0) GO TO 306
	IF(mu.EQ.1) GO TO 306
!  	WRITE (889,202), M
!      WRITE (889,616), mu
!      write (889,655),pn(96,I),pn(77,I)
!  655 format('????? h(96)= y(77)= ', 2F12.8)
!      write (889,302), ALAM
  616 FORMAT(10X,' mu=', 7I4)
  306    CONTINUE
 	IF(I.NE.3) GO TO 307 
	IF(Z1.NE.0) GO TO 307 
	IF(mu.EQ.2) GO TO 307 
  307   CONTINUE

!      write (888,302), ALAM
!	WRITE (889,616), mu
	
 1212 FORMAT(3X,f12.4)
!     write (889,302), ALAM
 
      
	
  302 FORMAT(3X,'Pseudofrequency ALAM = ',F10.6)
      IF(I.EQ.1) CHA(NF1)=ALAM
      IF(I.EQ.2.AND.NF2.NE.0) CHA(NF2)=ALAM
  303 CONTINUE
      do 660 k=1,96
!	write(555,1212),F0(k)
!	write(556,1212),yrad(k)
  660 continue	    
!	write(889,12),pn(96,1)
	IF(MU.EQ.2) GO TO 200
      DO 70 I=1,N
      DO 70 J=1,N
      S=0.D0
      T=0.D0
      DO 71 K=1,N
      S=S+D(K,I)*A(K,J)
      T=T+D(K,I)*B(K,J)
   71 CONTINUE
      F(I,J)=S
   70 QQ(I,J)=T
      DO 72 I=1,N
      DO 72 J=1,N
      S=0.
      T=0.
      DO 73 K=1,N
      S=S+D(K,J)*F(I,K)
   73 T=T+D(K,J)*QQ(I,K)
      A(I,J)=S
      B(I,J)=T
      X(I,J)=S
   72 Y(I,J)=T
      CALL VNROOT(N,Y,X,C,F)
      IF(MU.EQ.1) GO TO 84
      DO 85 I=1,N
      DO 85 J=1,N
   85 D(I,J)=P(I,J)
   84 CONTINUE
      DO 74 I=1,N
      DO 74 J=1,N
      S=0.
      DO 75 K=1,N
   75 S=S+D(I,K)*F(K,J)
   74 P(I,J)=S
      DO 76 I=1,N
      DO 76 J=1,N
   76 D(I,J)=F(I,J)
      GO TO 120
  200 CONTINUE
      N2=NF2
      IF(N2.EQ.0) GO TO 401
      S1=0.
      S2=0.
      DO 402 K=1,96
      ER=0.5D0*(GG(K)+1.D0)
      S1=S1+WW(K)*PN(K,4)*PN(K,5) *ER
      S2=S2+WW(K)*PN(K,4)*PN(K,4) *ER
  402 CONTINUE
      AF3=S1/S2*AN(2)/AN(1)
      IF(M.EQ.0) AF0=AF3
      IF(M.EQ.1) AF1=AF3
  401 CONTINUE
      DO 403 I=1,N
      PSF(I,NF1)=P(I,1)/AN(1)
      IF(NF2.EQ.0) GO TO 404
      PSF(I,NF2)=(P(I,2)-AF3*P(I,1))/(AN(2)- AF3*AN(1))
  404 CONTINUE
  403 CONTINUE
      H=H+Z1
	print 57,H
   57 format('na vykhode h=', F12.5)
!      pause

      RETURN
      END

C  SOME INTEGRALS FOR LINEAR VALUE PROBLEM IN GEO-VARIANT
      SUBROUTINE VSGEO(Z0,Z1,HH,AA,BB,N,M,IV)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/FORMA/ PSF(25,6),PS0(25),PSZ(6),AF0,AF1
      DIMENSION AA(N,N),BB(N,N)
      COMMON/YY/ TY(5,50)
      COMMON/GAL/ TE(4,5,25)
      COMMON/GEO/ H,R,D,D1
      COMMON/WG/ W(48),G(48)
      COMMON/UM/ WW(96),GG(106),F0(106),F1(106),F2(106),F3(106),FE(106),
     1yrad(106)
      COMMON/BC/ VK(6,25,106)
      H=HH
      M0=M+1
      QL=0.
      IF(M.EQ.0) QL=1.
      AU=FG(0.D0)
      DO 1 I=1,48
      GG(I+48)=G(I)
      WW(I+48)=W(I)*0.5D0
      GG(I)=-G(49-I)
    1 WW(I)=W(49-I)*0.5D0
      DO 2 I=1,10
!      A=0.03D0*I*2.D0
      A=0.02D0*I*2.D0
      GG(I+96)=A/H+1.D0
!      GG(I+96)=A/H+1.2D0
    2 CONTINUE
      DO 3 I=1,106
      X=0.5D0*(GG(I)-1.D0)
      Y=H*X
      yrad(i)=Y 
      F0(I)=FG(Y)
      F1(I)=FGP(Y)
      F2(I)=FGPP(Y)
      F3(I)=FGP3(Y)
      FE(I)=1.D0/DSQRT(1.D0+F1(I)**2)
    3 CONTINUE
      NV=N+IV
C        On the tank wall
      DO 4 I=1,106
      Z=0.5D0*(GG(I)-1.D0)*H +Z0
      E=F0(I)
      K=INDX(NV,M)
      CE=WF(M,K,E,Z)
      DO 41 L=1,NV
      L1=INDX(L,M)+1
   41 TE(1,1,L)=TY(M+1,L1)
      DO 42 L=2,NV
      K=L
      K1=INDX(L,M)
      TE(2,1,K)=(K1*TE(1,1,K)-(K1-M)*Z*TE(1,1,L-1))/E
   42 TE(1,2,K)=(K1-M)*TE(1,1,L-1)
      K=1
      K1=INDX(K,M)
      IF(M.EQ.0) K1=0
      TE(2,1,1)=K1*TE(1,1,1)/E
      TE(1,2,1)=QL
      DO 4 J=1,NV
      W00=TE(1,1,J)
      W10=TE(2,1,J)
      W01=TE(1,2,J)
      VK(1,J,I)=W00
      VK(2,J,I)=(W10-F1(I)*W01)*FE(I)
    4 CONTINUE
C      on the free surface 
      Z=Z0
      AW=FG(Z1)
      DO 5 I=1,96
      E=AU*(1.D0+GG(I))*0.5D0
      E1=AW*(1.D0+GG(I))*0.5D0
      K=INDX(NV,M)
      CE=WF(M,K,E,Z)
      DO 51 L=1,NV
      L1=INDX(L,M)+1
   51 TE(1,1,L)=TY(M+1,L1)
      CE=WF(M,K,E1,Z1)
      DO 52 L=1,NV
      L1=INDX(L,M)+1
   52 TE(2,1,L)=TY(M+1,L1)
      DO 53 L=2,NV
      K1=INDX(L,M)
      TE(1,2,L)=(K1-M)*TE(1,1,L-1)
   53 TE(2,2,L)=(K1-M)*TE(2,1,L-1)
      K=1
      K1=INDX(K,M)
      TE(1,2,1)=QL
      TE(2,2,K)=QL
      DO 5 J=1,NV
      W00=TE(1,1,J)
      U00=TE(2,1,J)
      U01=TE(2,2,J)
      W01=TE(1,2,J)
      VK(3,J,I)=W00
      VK(5,J,I)=U01
      VK(6,J,I)=U00
    5 VK(4,J,I)=W01
      IF(M.NE.0) GO TO 10
      PI=3.141592653589793D0
      X=0.5D0
      DO 11 I=1,N
      S=0.
      X=0.0
      DO 12 K=1,96
      E=0.5D0*(GG(K)+1.D0)
      X=X+WW(K)*E
   12 S=S+WW(K)*VK(3,I,K)*E
      PS0(I)=S/X
      DO 13 J=1,96
      VK(1,I,J)=VK(1,I,J)-S/X
      VK(6,I,J)=VK(6,I,J)-S/X
   13 VK(3,I,J)=VK(3,I,J)-S/X
   11 CONTINUE
   10 CONTINUE
C      the stage of integration 
      DO 6 I=1,N
      DO 6 J=1,N
      X=0.D0
      Y=0.D0
      Z=0.D0
      DO 7 K=1,96
      E=0.5D0*(GG(K)+1.D0)
      OT=1.D0
      E1=DSQRT(OT+F1(K)**2)
      X=X+WW(K)*VK(4,I,K)*VK(3,J,K)*E
      Y=Y+WW(K)*VK(2,I,K)*VK(1,J,K)*E1*F0(K)
      Z=Z+WW(K)*VK(3,I,K)*VK(3,J,K)*E
    7 CONTINUE
      AA(I,J)=X*AU**2+Y*H
    6 BB(I,J)=Z*AU**2
C         on the bottom
      TD=FG(-H)
      Z=Z0-H
      IF(TD.LE.0.01D0) RETURN
      DO 9 I=1,96
      E=TD*(1.D0+GG(I))*0.5D0
      K=INDX(NV,M)
      CE=WF(M,K,E,Z)
      DO 91 L=1,NV
      L1=INDX(L,M)+1
   91 TE(1,1,L)=TY(M+1,L1)
      DO 93 L=2,NV
      K1=INDX(L,M)
   93 TE(1,2,L)=(K1-M)*TE(1,1,L-1)
      K=1
      K1=INDX(K,M)
      TE(1,2,1)=QL
      DO 9 J=1,NV
      W00=TE(1,1,J)
      W01=TE(1,2,J)
      VK(3,J,I)=W00
    9 VK(4,J,I)=W01
      IF(M.NE.0) GO TO 20
      DO 19 I=1,N
      DO 19 J=1,96
   19 VK(3,I,J)=VK(3,I,J)-PS0(I)
   20 CONTINUE
C        integration stage 
      DO 16 I=1,N
      DO 16 J=1,N
      X=0.D0
      DO 17 K=1,96
      E=0.5D0*(GG(K)+1.D0)
   17 X=X+WW(K)*VK(4,I,K)*VK(3,J,K)*E
      AA(I,J)=AA(I,J)-X*TD**2
   16 CONTINUE
      RETURN
      END

      SUBROUTINE MatrM(A,B,N)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(22,22),B(22,22),DA(22),DB(22)
      DO 1 I=1,N
      DA(I)=0.
	DB(I)=0.
    1 Continue
      DO 9 I=1,N
      DO 8 J=1,N
	IF(dabs(DB(I)).LE.dabs(A(I,J))) DB(I)=dabs(A(I,J))
	DA(I)=DB(I)
    8 CONTINUE
    9 CONTINUE
	DO 2 I=1,N
      DO 5 J=1,N
      A(I,J)=A(I,J)/(DA(I))
	B(I,J)=B(I,J)/(DA(I))
 
    5 CONTINUE
    2 CONTINUE
      RETURN
      END

      SUBROUTINE Matr(A,B,N)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(22,22),B(22,22),DA(22)
	DO 1 I=1,N
      DA(I)=0.
    1 Continue

      DO 9 I=1,N
      DO 8 J=1,N
      DA(I)=DA(I)+A(I,J)*A(I,J)
    8 CONTINUE
    9 CONTINUE

      DO 4 I=1,N
      DA(I)=DSQRT((DA(I)))
      IF(DA(I).EQ.0.)   DA(I)=1.
    4 CONTINUE

	DO 3 I=1,N
      DO 5 J=1,N
      A(I,J)=A(I,J)/(DA(I))
	B(I,J)=B(I,J)/(DA(I))
    5 CONTINUE
    3 CONTINUE
      RETURN
      END

C   REPRESENTATION OF SOME BLOCKS FOR CALCULATION OF INTEGRALS         
      SUBROUTINE VLGEO(N1,N2,N3,NF)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/GAL/ TE(4,5,25)
      COMMON/GEO/ H,R,D,D1
      COMMON/BC/ PS(2,5,6,96),PKS(6,96),ALL(96),PAS(6,96),
     1AA01(6,96),AA11(6,96),AA02(6,96),AA12(6,96),AA22(6,96),AA21(6,96),
     2AA03(6,96),AA13(6,96),AA23(6,96),AA14(6,96),AA24(6,96),AA34(6,96)
      COMMON/UM/ WW(96),GG(106),E0(106),E1(106),E2(106),E3(106),EE(106)
     1,yrad(106)
      COMMON/FGEO/ U0,U1,U2,U3,PI
      COMMON/FORMA/ PSF(25,6),PS0(25),PSZ(6),AF0,AF1
      COMMON/MEN/ MM(10),NN(10),MN(6)
      COMMON/YY/ Y(5,50)
      A=0.D0
      U0=FG(A)
      U1=FGP(A)
      U2=FGPP(A)
      U3=FGP3(A)
      F0=U0
      F1=U1
      F2=U2
      F3=U3
      DO 1 J=1,96
      X=0.5D0*(1.D0+GG(J))
      ALL(J)=X
      DO 1 I=1,6
      X=ALL(J)*F0
      A=-PSZ(I)
      M=MN(I)
      QL=0.
      IF(M.EQ.0) QL=1.
      M1=M+1
      NE=INDX(NF,M)
      AB=WF(M,NE,X,A)
      DO 12 K=1,NF
      K1=INDX(K,M)+1
   12 TE(1,1,K)=Y(M1,K1)
      DO 13 K=2,NF
      K1=INDX(K,M)
      TE(2,1,K)=(K1*TE(1,1,K)-(K1-M)*A*TE(1,1,K-1))/X
   13 TE(1,2,K)=(K1-M)*TE(1,1,K-1)
      K=1
      K1=INDX(K,M)
      IF(M.EQ.0) K1=0
      TE(2,1,1)=K1*TE(1,1,1)/X
      TE(1,2,1)=QL
      DO 14 K=2,NF
      K1=INDX(K,M)
      TE(3,1,K)=(-K1*TE(1,1,K)+(K1-M)*A*TE(1,1,K-1))/X**2+
     1(K1*TE(2,1,K)-(K1-M)*A*TE(2,1,K-1))/X
      TE(2,2,K)=(K1-M)*TE(2,1,K-1)
   14 TE(1,3,K)=(K1-M)*TE(1,2,K-1)
      K=1
      K1=INDX(K,M)
      IF(M.EQ.0) K1=0
      TE(3,1,K)=-K1*TE(1,1,K)/X**2+K1*TE(2,1,K)/X
      TE(2,2,K)=0.
      TE(1,3,K)=0.
      DO 15 K=2,NF
      K1=INDX(K,M)
      TE(4,1,K)=(2.*K1*TE(1,1,K)-2.*(K1-M)*A*TE(1,1,K-1))/
     1X**3-2.*(K1*TE(2,1,K)-(K1-M)*A*TE(2,1,K-1))/X**2
     2+(K1*TE(3,1,K)-(K1-M)*A*TE(3,1,K-1))/X
      TE(3,2,K)=(K1-M)*TE(3,1,K-1)
      TE(2,3,K)=(K1-M)*TE(2,2,K-1)
   15 TE(1,4,K)=(K1-M)*TE(1,3,K-1)
      K=1
      K1=INDX(K,M)
      IF(M.EQ.0) K1=0
      TE(4,1,K)=2.*K1*TE(1,1,K)/X**3-2.*K1*TE(2,1,K)/X**2+K1*TE(3,1,K)/X
      TE(3,2,K)=0.
      TE(2,3,K)=0.
      TE(1,4,K)=0.
      DO 16 K=2,NF
      K1=INDX(K,M)
      TE(4,2,K)=(K1-M)*TE(4,1,K-1)
      TE(3,3,K)=(K1-M)*TE(3,2,K-1)
      TE(2,4,K)=(K1-M)*TE(2,3,K-1)
   16 TE(1,5,K)=(K1-M)*TE(1,4,K-1)
      TE(4,2,1)=0.
      TE(3,3,1)=0.
      TE(2,4,1)=0.
      TE(1,5,1)=0.
      IF(M.NE.0) GO TO 50
      DO 51 K=1,NF
   51 TE(1,1,K)=TE(1,1,K)-PS0(K)
   50 CONTINUE
      A00=0.
      A10=0.
      A20=0.
      A30=0.
      A01=0.
      A11=0.
      A21=0.
      A31=0.
      A02=0.
      A12=0.
      A22=0.
      A03=0.
      A13=0.
      A04=0.
      DO 17 L=1,NF
      AV=PSF(L,I)
      A00=A00+TE(1,1,L)*AV
      A10=A10+TE(2,1,L)*AV
      A20=A20+TE(3,1,L)*AV
      A30=A30+TE(4,1,L)*AV
      A01=A01+TE(1,2,L)*AV
      A11=A11+TE(2,2,L)*AV
      A21=A21+TE(3,2,L)*AV
      A31=A31+TE(4,2,L)*AV
      A02=A02+TE(1,3,L)*AV
      A12=A12+TE(2,3,L)*AV
      A22=A22+TE(3,3,L)*AV
      A03=A03+TE(1,4,L)*AV
      A13=A13+TE(2,4,L)*AV
   17 A04=A04+TE(1,5,L)*AV
      X=ALL(J)
      PS(1,1,I,J)=A00
      PS(2,1,I,J)=F0*A10
      PS(1,2,I,J)=X*H*F1*A10+H*A01
      PS(2,2,I,J)=X*H*F1*F0*A20+H*F0*A11+H*F1*A10
      PS(1,3,I,J)=(X*H*F1)**2*A20+X*H**2*F2*A10+
     12.D0*X*H**2*F1*A11+H**2*A02
      PS(2,3,I,J)=(X*H*F1)**2*F0*A30+2.D0*X*(H*F1)**2*A20+
     1H**2*F2*A10+X*H**2*F2*F0*A20+2.D0*H**2*F1*A11+
     22.D0*X*H**2*F1*F0*A21+H**2*F0*A12
      PS(1,4,I,J)=3.D0*X**2*H**3*F1*F2*A20+X*H**3*F3*A10+
     13.D0*X*H**3*F2*A11+(X*H*F1)**3 *A30+
     23.D0*X*H**3*F1*(X*F1*A21+A12)+H**3*A03
      PS(2,4,I,J)=3.D0*X**2*H**3*F1*F2*A21+X*H**3*F3*A11+
     13.D0*X*H**3*F2*A12+(X*H*F1)**3 *A31+
     23.D0*X*H**3*F1*(X*F1*A22+A13)+H**3*A04
      PS(1,5,I,J)=X*H*F1*A11+H*A02
      PS(2,5,I,J)=(X*H*F1)**2*A21+X*H**2*F2*A11+
     12.D0*X*H**2*F1*A12+H**2*A03
      PKS(I,J)=A01
      PAS(I,J)=A11*F0
    1 CONTINUE
      O1=U1**2
      DO 3 I=1,6
      DO 3 J=1,96
      A00=PS(1,1,I,J)
      A01=PS(1,2,I,J)
      A11=PS(2,2,I,J)
      A10=PS(2,1,I,J)
      A12=PS(2,3,I,J)
      A02=PS(1,3,I,J)
      A03=PS(1,4,I,J)
      AZ0=PKS(I,J)
      AZ1=PS(1,5,I,J)
      AZ2=PS(2,5,I,J)
      AZ3=PS(2,4,I,J)
      AA01(I,J)=A10/U0**2
      AA11(I,J)=(A11-2.*H/U0*U1*A10)/H/U0**2
      AA21(I,J)=0.5/H**2*(A12-4.*H*U1/U0*A11+
     1(6.*H**2/U0**2*O1-2.*H**2/U0*U2)*A10)/U0**2
      AA02(I,J) =A00/U0**2
      AA12(I,J)=(A01-2.*H*U1/U0*A00)/H/U0**2
      AA22(I,J)=0.5/H**2/U0**2*(A02-4.*H*U1/U0*A01+
     1(6.*H**2/U0**2*O1-2.*H**2/U0*U2)*A00)
      AA03(I,J)=-U1/U0*AZ0
      AA13(I,J)=(-U2/U0+(U1/U0)**2)*AZ0-U1/U0*AZ1/H
      AA23(I,J)=0.5/H**2/U0*(H**2*(-U3+U2*U1/U0+2.*U1*U2/U0
     1-2.*U1**3/U0**2)*AZ0+(-U2+U1**2/U0)*H*AZ1-U1*AZ2)
      AA14(I,J)=AZ1/H
      AA24(I,J)=AZ2*0.5/H**2
      AA34(I,J)=AZ3/H**3/6.
    3 CONTINUE
      RETURN
      END

C     FINAL CALCULATION OF COEFFICIENTS BEFOR CALCULATION
C    SOME COMBINATIONS FROM COEFFICIENTS
      SUBROUTINE VMGEO(N1,N2,N3,NF)
      COMMON/BC/ PS(2,5,6,96),PKS(6,96),ALL(96),PAS(6,96),
     1AA01(6,96),AA11(6,96),AA02(6,96),AA12(6,96),AA22(6,96),AA21(6,96),
     2AA03(6,96),AA13(6,96),AA23(6,96),AA14(6,96),AA24(6,96),AA34(6,96)
      COMMON/FGEO/ F0,F1,F2,F3,PI
      COMMON/GEO/ H,R,D,D1
      COMMON/UM/ WW(96),GG(106),E0(106),E1(106),E2(106),E3(106),EE(106)
     1,yrad(106)
      DOUBLE PRECISION PS,PKS,ALL,PAS,F0,F1,F2,F3,PI,S,
     1WW,GG,E0,E1,E2,E3,EE,S1,S2,S3,S4,S5,S6,
     2H,R,D,D1,AA01,AA11,AA21,AA02,AA12,AA22,
     3AA03,AA13,AA23,AA14,AA24,AA34
      COMMON/MEN/ MM(10),NN(10),MN(6)
      COMMON/TP8/ TANP(9),TAPP(9,9)
      COMMON/TP9/ TAL(7),TANN(7),TGA(7,7,7),TGA2(7,7,7),
     1TDE(7,7,7,7),TDE2(7,7,7,7),THH(3,3,3,3,7),THH2(3,3,3,3,7),
     2TALS(7),TALPC(7),TALC(7),TALPS(7),TBES(7,7),TBEPS(7,7),
     3TBEC(7,7),TBEPC(7,7),TGAS(7,7,7),TGAPS(7,7,7),TGAC(7,7,7),
     4TGAPC(7,7,7),TDEC(7,7,7,7),TDES(7,7,7,7),TDE44(7,7,7,7),
     5TDEPC(7,7,7,7),TDEPS(7,7,7,7)
      COMMON/EP/GAV(10,10,10),ANN(10),GB0(10,6,10),GB1(10,3,10),
     1DB1(10,3,3,10),HB(3,3,3,3,10),BB1(10,10),BF2(10,10),BF3(3),
     2GF3(10,10,10),DF4(10,10,10,10),ALX1(10),ALY1(10),ALZ1(10),
     3ALX2(10),ALY2(10),BEX2(10,10),BEY2(10,10),GAX3(10,10,10),
     4GAY3(10,10,10),GAZ3(10,10,10),DEX4(10,10,10,10),DEY4(10,10,10,10),
     5DEZ4(10,10,10,10),DES(3,3,3,10),ALS(10),BES(10,10),DE4(3,3,3,10),
     6BEX3(10,10),BEY3(10,10),BEZ3(10,10)
      PI=3.141592653589793D0
      UP=1.E-5
      CALL VUGEL(7,7,7,7)
      DO 1 I=1,N1
      NI=NN(I)
      MI=MM(I)
      DO 1 J=1,I
      NJ=NN(J)
      MJ=MM(J)
      DO 1 K=1,J
      NK=NN(K)
      MK=MM(K)
      S=0.D0
      SA=TGA(MI,MJ,MK)
      IF(SA.EQ.0.) GO TO 201
      DO 101 IG=1,96
  101 S=S+WW(IG)*PKS(NI,IG)*PKS(NJ,IG)*PKS(NK,IG)*ALL(IG)
  201 CONTINUE
      U=S*SA
      IF(ABS(U).LE.UP) U=0.
      GAV(I,J,K)=U
      GAV(I,K,J)=U
      GAV(K,I,J)=U
      GAV(K,J,I)=U
      GAV(J,I,K)=U
    1 GAV(J,K,I)=U
      DO 2 I=1,N1
      S=0.D0
      NI=NN(I)
      MI=MM(I)
      SA=TANN(MI)
      DO 102 IG=1,96
  102 S=S+WW(IG)*PKS(NI,IG)**2*ALL(IG)
      U=S*SA
      IF(ABS(U).LE.UP) U=0.
    2 ANN(I)=U
      DO 3 I=1,N1
      MI=MM(I)
      NI=NN(I)
      DO 3 J=1,N2
      NJ=NN(J)
      MJ=MM(J)
      DO 3 K=1,N1
      NK=NN(K)
      MK=MM(K)
      S1=0.D0
      S2=0.D0
      SA=TGA (MI,MJ,MK)
      SB=TGA2(MI,MJ,MK)
      IF(SA.EQ.0.) GO TO 202
      DO 103 IG=1,96
      S1=S1+WW(IG)*(AA01(NI,IG)*PAS(NJ,IG)+ALL(IG)*
     1AA03(NI,IG)*PAS(NJ,IG)-AA14(NI,IG)*PKS(NJ,IG))*PKS(NK,IG)*ALL(IG)
  103 CONTINUE
  202 CONTINUE
      IF(SB.EQ.0.) GO TO 203
      DO 104 IG=1,96
      S2=S2+WW(IG)*AA02(NI,IG)*PKS(NJ,IG)*PKS(NK,IG)/ALL(IG)
  104 CONTINUE
  203 CONTINUE
      U=S1*SA+S2*SB
      IF(ABS(U).LE.UP) U=0.
    3 GB0(I,J,K)=U
      DO 4 I=1,N1
      NI=NN(I)
      MI=MM(I)
      DO 4 J=1,N3
      NJ=NN(J)
      MJ=MM(J)
      DO 4 K=1,N1
      NK=NN(K)
      MK=MM(K)
      S1=0.D0
      S2=0.D0
      SA=TGA (MI,MJ,MK)
      SB=TGA2(MI,MJ,MK)
      IF(SA.EQ.0.) GO TO 205
      DO 105 IG=1,96
      S1=S1+WW(IG)*(AA11(NI,IG)*PAS(NJ,IG)+ALL(IG)*PAS(NJ,IG)
     1*AA13(NI,IG)-2.D0*AA24(NI,IG)*PKS(NJ,IG))*ALL(IG)*PKS(NK,IG)
  105 CONTINUE
  205 CONTINUE
      IF(SB.EQ.0.) GO TO 206
      DO 106 IG=1,96
      S2=S2+WW(IG)*AA12(NI,IG)*PKS(NJ,IG)*PKS(NK,IG)/ALL(IG)
  106 CONTINUE
  206 CONTINUE
      U=S1*SA+S2*SB
      IF(ABS(U).LE.UP) U=0.
    4 GB1(I,J,K)=U
      DO 5 I=1,N1
      NI=NN(I)
      MI=MM(I)
      DO 5 J=1,N3
      NJ=NN(J)
      MJ=MM(J)
      DO 5 K=1,N3
      NK=NN(K)
      MK=MM(K)
      DO 5 L=1,N1
      NL=NN(L)
      ML=MM(L)
      S1=0.D0
      S2=0.D0
      SA=TDE (MI,MJ,MK,ML)
      SB=TDE2(MI,MJ,MK,ML)
      IF(SA.EQ.0.) GO TO 207
      DO 107 IG=1,96
      S1=S1+WW(IG)*(AA11(NI,IG)*PAS(NJ,IG)+ALL(IG)*AA13(NI,IG)*PAS(NJ,IG
     1)-AA24(NI,IG)*PKS(NJ,IG))*ALL(IG)*PKS(NK,IG)*PKS(NL,IG)
  107 CONTINUE
  207 CONTINUE
      IF(SB.EQ.0.) GO TO 208
      DO 108 IG=1,96
      S2=S2+WW(IG)*AA12(NI,IG)*PKS(NJ,IG)*PKS(NK,IG)*PKS(NL,IG)/ALL(IG)
  108 CONTINUE
  208 CONTINUE
      U=S1*SA+S2*SB
      IF(ABS(U).LE.UP) U=0.
    5 DB1(I,J,K,L)=U
      DO 6 I=1,N3
      NI=NN(I)
      MI=MM(I)
      DO 6 J=1,N3
      NJ=NN(J)
      MJ=MM(J)
      DO 6 K=1,N3
      NK=NN(K)
      MK=MM(K)
      DO 6 L=1,K
      NL=NN(L)
      ML=MM(L)
      DO 6 M=1,N1
      MP=MM(M)
      NP=NN(M)
      S1=0.D0
      S2=0.D0
      SA=THH (MI,MJ,MK,ML,MP)
      SB=THH2(MI,MJ,MK,ML,MP)
      IF(SA.EQ.0.) GO TO 209
      DO 109 IG=1,96
      S5=ALL(IG)
      S1=S1+WW(IG)*S5*PKS(NK,IG)*PKS(NL,IG)*PKS(NP,IG)*
     1(AA21(NI,IG)*PAS(NJ,IG)+ALL(IG)*AA23(NI,IG)*PAS(NJ,IG)-
     2AA34(NI,IG)*PKS(NJ,IG))
  109 CONTINUE
  209 CONTINUE
      IF(SB.EQ.0.) GO TO 210
      DO 110 IG=1,96
      S2=S2+WW(IG)*AA22(NI,IG)/ALL(IG)*PKS(NJ,IG)*
     1PKS(NK,IG)*PKS(NL,IG)*PKS(NP,IG)
  110 CONTINUE
  210 CONTINUE
      U=S1*SA+S2*SB
      IF(ABS(U).LE.UP) U=0.
      HB(I,J,K,L,M)=U
    6 HB(I,J,L,K,M)=U
      DO 7 I=1,N1
      NI=NN(I)
      MI=MM(I)
      DO 7 J=1,N1
      NJ=NN(J)
      MJ=MM(J)
      S=0.D0
      SA=TANN(MI)*DEL(MI,MJ)
      IF(SA.EQ.0.) GO TO 117
      DO 217 IG=1,96
  217 S=S+WW(IG)*ALL(IG)*AA14(NI,IG)*PKS(NJ,IG)
  117 CONTINUE
      U=SA*S
      IF(ABS(U).LE.UP) U=0.
    7 BB1(I,J)=U
      DO 8 I=1,N1
      NI=NN(I)
      MI=MM(I)
      DO 8 J=1,I
      NJ=NN(J)
      MJ=MM(J)
      S =0.D0
      S1=0.D0
      S2=0.D0
      SA=TANN(MI)*DEL(MI,MJ)
      IF(SA.EQ.0.) GO TO 118
      DO 218 IG=1,96
      S3=0.5D0*(1.D0+GG(IG))
      S1=S1+WW(IG)*PKS(NI,IG)*PS(1,1,NJ,IG)*S3
      S2=S2+WW(IG)*PKS(NJ,IG)*PS(1,1,NI,IG)*S3
  218 CONTINUE
  118 CONTINUE
      S1=0.5*(S1+S2)
      U=SA*S1*F0**2
      IF(ABS(U).LE.UP) U=0.
      BF2(J,I)=U
    8 BF2(I,J)=U
      DO 9 I=1,N3
      NI=NN(I)
      MI=MM(I)
      S1=0.D0
      S2=0.D0
      SA=TANN(MI)
      SB=TANP(MI)
      DO 119 IG=1,96
  119 S1=S1+WW(IG)*ALL(IG)*(PS(2,1,NI,IG)**2+
     1PKS(NI,IG)**2*F0**2)
      IF(SB.EQ.0.) GO TO 220
      DO 120 IG=1,96
  120 S2=S2+WW(IG)*PS(1,1,NI,IG)**2/ALL(IG)
  220 CONTINUE
      U=S1*SA+S2*SB
      IF(ABS(U).LE.UP) U=0.
    9 BF3(I)=U
      DO 10 I=1,N1
      NI=NN(I)
      MI=MM(I)
      DO 10 J=1,I
      NJ=NN(J)
      MJ=MM(J)
      DO 10 K=1,N1
      NK=NN(K)
      MK=MM(K)
      S1=0.D0
      S2=0.D0
      SA=TGA (MI,MJ,MK)
      SB=TGA2(MI,MJ,MK)
      IF(SA.EQ.0.) GO TO 230
      DO 130 IG=1,96
      S1=S1+WW(IG)*ALL(IG)*(PS(2,1,NI,IG)*PS(2,1,NJ,IG)+
     1F0**2*PKS(NI,IG)*PKS(NJ,IG))*PKS(NK,IG)
  130 CONTINUE
  230 CONTINUE
      IF(SB.EQ.0.) GO TO 231
      DO 131 IG=1,96
  131 S2=S2+WW(IG)*PS(1,1,NI,IG)*PS(1,1,NJ,IG)*PKS(NK,IG)/ALL(IG)
  231 CONTINUE
      U=S1*SA+S2*SB
      IF(ABS(U).LE.UP) U=0.
      GF3(J,I,K)=U
   10 GF3(I,J,K)=U
      DO 12 I=1,N1
      NI=NN(I)
      MI=MM(I)
      DO 12 J=1,I
      NJ=NN(J)
      MJ=MM(J)
      DO 12 K=1,N1
      NK=NN(K)
      MK=MM(K)
      DO 12 L=1,K
      NL=NN(L)
      ML=MM(L)
      S1=0.D0
      S2=0.D0
      SA=TDE (MI,MJ,MK,ML)
      SB=TDE2(MI,MJ,MK,ML)
      IF(SA.EQ.0.) GO TO 232
      DO 132 IG=1,96
      S1=S1+WW(IG)*ALL(IG)*(PS(2,2,NI,IG)*PS(2,1,NJ,IG)
     1+PS(2,1,NI,IG)*PS(2,2,NJ,IG)+2.D0*H*F0*F1*PKS(NI,IG)*
     2PKS(NJ,IG)+F0**2*PS(1,5,NI,IG)*PKS(NJ,IG)+
     3F0**2*PKS(NI,IG)*PS(1,5,NJ,IG))*PKS(NK,IG)*PKS(NL,IG)
  132 CONTINUE
  232 CONTINUE
      IF(SB.EQ.0.) GO TO 233
      DO 133 IG=1,96
      S2=S2+WW(IG)*PKS(NK,IG)*PKS(NL,IG)/ALL(IG)*
     1(PS(1,2,NI,IG)*PS(1,1,NJ,IG)+PS(1,1,NI,IG)*PS(1,2,NJ,IG))
  133 CONTINUE
  233 CONTINUE
      U=(S1*SA+S2*SB)/H*0.5D0
      IF(ABS(U).LE.UP) U=0.
      DF4(I,J,K,L)=U
      DF4(J,I,K,L)=U
      DF4(I,J,L,K)=U
   12 DF4(J,I,L,K)=U
      RETURN
      END

C     COEFFICIENTS FOR GEO
      SUBROUTINE VNGEO(N1,N2,N3)
      COMMON/EP/GAV(10,10,10),ANN(10),GB0(10,6,10),GB1(10,3,10),
     1DB1(10,3,3,10),HB(3,3,3,3,10),BB1(10,10),BF2(10,10),BF3(3),
     2GF3(10,10,10),DF4(10,10,10,10),ALX1(10),ALY1(10),ALZ1(10),
     3ALX2(10),ALY2(10),BEX2(10,10),BEY2(10,10),GAX3(10,10,10),
     4GAY3(10,10,10),GAZ3(10,10,10),DEX4(10,10,10,10),DEY4(10,10,10,10),
     5DEZ4(10,10,10,10),DES(3,3,3,10),ALS(10),BES(10,10),DE4(3,3,3,10),
     6BEX3(10,10),BEY3(10,10),BEZ3(10,10)
      COMMON/FGEO/ F0,F1,F2,F3,PI
      DOUBLE PRECISION F0,F1,F2,F3,PI
      COMMON/BC/ GAC(6,6,10),DEC(3,3,3,10),HHC(3,3,3,3,10),
     1VV1(10,10),VV2(10,10,10),VV3(10,10,10,10),UU1(3,10),UU2(3,10,10),
     2UU3(3,10,10,10),UU4(3,10,10,10,10),WW1(10),WW2(10),WW3(6,6,10),
     3WW4(3,3,3,10),VZ2(6,6,10),VZ3(3,3,3,10),UZ2(3,10,10),
     4UZ3(3,6,6,10),UZ4(3,3,3,3,10)
      G1=PI*F0**2
      G2=PI*F0*F1
      G3=PI/3.*(F1**2+F0*F2)
      G4=PI/12.*(3.*F1*F2+F0*F3)
      DO 1 I=1,N2
      DO 1 J=1,N2
      DO 1 K=1,N1
    1 GAC(I,J,K)=GB0(I,J,K)/ANN(K)
      DO 2 I=1,N3
      DO 2 J=1,N3
      DO 2 K=1,N3
      DO 2 IP=1,N1
      S=0.
      DO 3 M=1,N1
      SA=GAC(I,J,M)
      IF(SA.EQ.0.) GO TO 4
      S=S+SA*GB0(M,K,IP)
    4 CONTINUE
    3 CONTINUE
    2 DEC(I,J,K,IP)=(S+DB1(I,J,K,IP)+G2/G1*BB1(I,IP)*
     1DEL(J,K)*ANN(J)/PI)/ANN(IP)
      DO 5 I=1,N3
      DO 5 J=1,N3
      DO 5 K=1,N3
      DO 5 L=1,N3
      DO 5 IP=1,N1
      S=0.
      U=0.
      DO 6 M=1,N1
      SA=GB0(M,L,IP)
      SB=GAC(I,J,M)
      IF(SA.EQ.0.) GO TO 7
      S=S+SA*DEC(I,J,K,M)
    7 CONTINUE
      IF(SB.EQ.0.) GO TO 8
      U=U+SB*(DB1(M,L,K,IP)+G2/G1/PI*BB1(M,IP)*ANN(K)*DEL(K,L))
    8 CONTINUE
    6 CONTINUE
    5 HHC(I,J,K,L,IP)=(S+U-G2/G1*GB1(I,J,IP)/PI*ANN(K)*DEL(K,L)+
     1HB(I,J,K,L,IP)+G3/G1/PI*BB1(I,IP)*GAV(J,K,L))/ANN(IP)
      DO 9 I=1,N1
      DO 9 J=1,I
      VV1(I,J)=BF2(I,J)
    9 VV1(J,I)=BF2(I,J)
      DO 10 I=1,N1
      DO 10 J=1,N1
      DO 10 K=1,N1
      S=0.
      U=0.
      DO 11 M=1,N1
      SA=BF2(M,J)
      IF(I.GT.N2.OR.K.GT.N2) GO TO 12
      IF(SA.EQ.0.) GO TO 12
      S=S+SA*GAC(I,K,M)
   12 CONTINUE
   11 CONTINUE
   10 VV2(I,J,K)=GF3(I,J,K)+2.*S
      DO 13 I=1,N1
      DO 13 J=1,N1
      DO 13 K=1,N1
      DO 13 L=1,N1
      S=0.
      U=0.
      V=0.
      DO 14 M=1,N1
      IF(I.GT.N2.OR.L.GT.N2) GO TO 16
      SA=GAC(I,L,M)
      IF(SA.EQ.0.) GO TO 15
      S=S+SA*GF3(M,J,K)
      IF(J.GT.N2.OR.K.GT.N2) GO TO 15
      V=V+SA*GAC(J,K,M)*BF2(M,M)
   15 CONTINUE
      IF(I.GT.N3.OR.K.GT.N3) GO TO 16
      IF(L.GT.N3) GO TO 16
      SA=BF2(M,J)
      IF(SA.EQ.0.) GO TO 17
      U=U+SA*DEC(I,K,L,M)
   17 CONTINUE
   16 CONTINUE
   14 CONTINUE
      T=0.
      IF(I.GT.N3) GO TO 101
      IF(K.GT.N2.OR.L.GT.N2) GO TO 101
      T=G2/G1/PI*BF3(I)*DEL(I,J)*ANN(K)*DEL(K,L)
      T=-T
  101 CONTINUE
   13 VV3(I,J,K,L)=DF4(I,J,K,L)+2.*(S+U)+T+V
      DO 21 I=1,N1
      UU1(1,I)=ALX1(I)
      UU1(2,I)=ALY1(I)
   21 UU1(3,I)=ALZ1(I)
      DO 22 I=1,N1
      DO 22 J=1,N1
      S1=0.
      S2=0.
      S3=0.
      IF(I.GT.N2.OR.J.GT.N2) GO TO 23
      DO 24 M=1,N1
      SA=GAC(I,J,M)
      IF(SA.EQ.0.) GO TO 224
      S1=S1+SA*ALX1(M)
      S2=S2+SA*ALY1(M)
      S3=S3+SA*ALZ1(M)
  224 CONTINUE
   24 CONTINUE
   23 CONTINUE
      UU2(1,I,J)=BEX2(I,J)+S1
      UU2(2,I,J)=BEY2(I,J)+S2
   22 UU2(3,I,J)=S3+F0**2*ANN(I)*DEL(I,J)
      DO 25 I=1,N1
      DO 25 J=1,N1
      DO 25 K=1,N1
      S1=0.
      S2=0.
      S3=0.
      DO 26 M=1,N1
      SA=0.
      SB=0.
      IF(I.GT.N2.OR.J.GT.N2) GO TO 28
      SB=GAC(I,J,M)
      IF(I.GT.N3) GO TO 28
      IF(J.GT.N3) GO TO 28
      IF(K.GT.N3) GO TO 28
      SA=DEC(I,J,K,M)
   28 CONTINUE
      S1=S1+SA*ALX1(M)+SB*BEX2(M,K)
      S2=S2+SA*ALY1(M)+SB*BEY2(M,K)
   26 S3=S3+SA*ALZ1(M)+SB*F0**2*ANN(K)*DEL(M,K)
      U=0.
      IF(J.LE.N2) U=-G2/G1*ANN(J)/PI*DEL(J,K)
      UU3(1,I,J,K)=GAX3(I,J,K)+S1+U*ALX2(I)
      UU3(2,I,J,K)=GAY3(I,J,K)+S2+U*ALY2(I)
   25 UU3(3,I,J,K)=GAZ3(I,J,K)+S3
      DO 30 I=1,N1
      DO 30 J=1,N1
      DO 30 K=1,N1
      DO 30 L=1,N1
      S1=0.
      S2=0.
      S3=0.
      DO 31 M=1,N1
      SA=0.
      SB=0.
      SC=0.
      SD=0.
      IF(I.GT.N2.OR.L.GT.N2) GO TO 32
      SD=GAC(I,L,M)
      IF(K.GT.N2.OR.J.GT.N2) GO TO 32
      SB=GAC(I,J,M)*DEL(K,L)*ANN(K)/PI*G2/G1
      IF(I.GT.N3.OR.J.GT.N3) GO TO 32
      IF(K.GT.N3) GO TO 32
      IF(L.GT.N3) GO TO 32
      SA=HHC(I,J,K,L,M)
   32 CONTINUE
      IF(I.GT.N3.OR.J.GT.N3) GO TO132
      IF(K.GT.N3) GO TO132
      SC=DEC(I,J,K,M)
  132 CONTINUE
      S1=S1+SA*ALX1(M)-SB*ALX2(M)+SC*BEX2(M,L)+SD*GAX3(M,J,K)
      S2=S2+SA*ALY1(M)-SB*ALY2(M)+SC*BEY2(M,L)+SD*GAY3(M,J,K)
   31 S3=S3+SA*ALZ1(M)+SD*GAZ3(M,J,K)+SC*F0**2*ANN(M)*DEL(M,L)
      SF=0.
      SE=0.
      IF(L.LE.N3.AND.J.LE.N3.AND.K.LE.N3) SE=GAV(J,K,L)*G3/G1/PI
      IF(K.LE.N2.AND.L.LE.N2) SF=-G2/G1*DEL(K,L)*ANN(K)/PI
      UU4(1,I,J,K,L)=S1-SE*ALX2(I)+BEX3(I,J)*SF+DEX4(I,J,K,L)
      UU4(2,I,J,K,L)=S2-SE*ALY2(I)+BEY3(I,J)*SF+DEY4(I,J,K,L)
   30 UU4(3,I,J,K,L)=S3+BEZ3(I,J)*SF+DEZ4(I,J,K,L)
      DO 51 I=1,N1
      DO 51 J=1,I
      DO 51 K=1,N1
      S1        =0.5*(VV2(I,J,K)+VV2(J,I,K))
      VV2(I,J,K)=S1
      VV2(J,I,K)=S1
      DO 51 L=1,K
      S2          =0.25*(VV3(I,J,K,L)+VV3(J,I,K,L)+
     1VV3(I,J,L,K)+VV3(J,I,L,K))
      VV3(I,J,K,L)=S2
      VV3(J,I,K,L)=S2
      VV3(I,J,L,K)=S2
      VV3(J,I,L,K)=S2
   51 CONTINUE
      DO 52 M=1,3
      DO 52 I=1,N1
      DO 52 J=1,N1
      DO 52 K=1,J
      S1=0.5*(UU3(M,I,J,K)+UU3(M,I,K,J))
      UU3(M,I,J,K)=S1
      UU3(M,I,K,J)=S1
      DO 52 L=1,K
      S2=(UU4(M,I,J,K,L)+UU4(M,I,J,L,K)+
     1UU4(M,I,K,J,L)+UU4(M,I,K,L,J)+UU4(M,I,L,J,K)+UU4(M,I,L,K,J))/6.
      UU4(M,I,J,K,L)=S2
      UU4(M,I,J,L,K)=S2
      UU4(M,I,K,J,L)=S2
      UU4(M,I,K,L,J)=S2
      UU4(M,I,L,J,K)=S2
      UU4(M,I,L,K,J)=S2
   52 CONTINUE
      E1=0.5*F0**2
      E1=F0**2
      E2=2./3.*F1*F0
      E2=4./3.*F1*F0
      E3=0.25*(F2*F0+F1**2)
      E3=0. 5*(F2*F0+F1**2)
      DO 40 I=1,N1
      WW1(I)=0.
   40 WW2(I)=E1*ANN(I)
      DO 41 I=1,N2
      DO 41 J=1,N2
      DO 41 K=1,N1
   41 WW3(I,J,K)=E2*GAV(I,J,K)
      DO 42 I=1,N3
      DO 42 J=1,N3
      DO 42 K=1,N3
      DO 42 L=1,N1
   42 WW4(I,J,K,L)=(E1*(G2/G1)**2-3.*G2*E2/G1)*0.5/PI*
     1ANN(I)*ANN(L)*(DEL(I,J)*DEL(K,L)+DEL(I,K)*DEL(J,L))+E3*
     2DE4(I,J,K,L)
      DO 45 I=1,N2
      DO 45 J=1,N2
      DO 45 K=1,N1
   45 VZ2(I,J,K)=0.5*VV2(I,J,K)-VV2(I,K,J)
      DO 46 I=1,N3
      DO 46 J=1,N3
      DO 46 K=1,N3
      DO 46 L=1,N1
   46 VZ3(I,J,K,L)=VV3(I,J,K,L)-2.*VV3(I,L,J,K)
      DO 47 I=1,3
      DO 47 J=1,N1
      DO 47 K=1,N1
   47 UZ2(I,J,K)=UU2(I,J,K)-UU2(I,K,J)
      DO 48 M=1,3
      DO 48 I=1,N2
      DO 48 J=1,N2
      DO 48 K=1,N1
   48 UZ3(M,I,J,K)=2.*(UU3(M,I,J,K)-UU3(M,K,I,J))
      DO 49 M=1,3
      DO 49 I=1,N3
      DO 49 J=1,N3
      DO 49 K=1,N3
      DO 49 L=1,N1
   49 UZ4(M,I,J,K,L)=3.*(UU4(M,I,J,K,L)-UU4(M,L,I,J,K))
      RETURN
      END

C     GAUSS INTEGRAL 1-N DIMENTION
      DOUBLE  PRECISION  FUNCTION  GI(A,B,F)
      DOUBLE PRECISION A,B,F
      COMMON /WG/ W(48),G(48)
      DOUBLE PRECISION A1,A2,C1,S,S1,G,W
      S=0.0D0
      A1=(B-A)/2.0 D0
      A2=(A+B)/2.0 D0
      DO 1 I=1,48
      C1=G(I)
      S=S+W(I)*(F(A1*C1+A2)+F(A2-A1*C1))*A1
    1 CONTINUE
      GI=S
      RETURN
      END

C    CALCULATION OF INTEGRALS ON R DIRECTION
      SUBROUTINE VPGEO(N1,N2,N3,NFF)
      COMMON/BC/ PS(2,5,6,96),PKS(6,96),ALL(96),PAS(6,96),
     1AA01(6,96),AA11(6,96),AA02(6,96),AA12(6,96),AA22(6,96),AA21(6,96),
     2AA03(6,96),AA13(6,96),AA23(6,96),AA14(6,96),AA24(6,96),AA34(6,96)
      COMMON/FGEO/ F0,F1,F2,F3,PI
      COMMON/GEO/ H,R,D,D1
      COMMON/UM/ WW(96),GG(106),E0(106),E1(106),E2(106),E3(106),EE(106)
     1,yrad(106) 
	DOUBLE PRECISION PS,PKS,ALL,PAS,F0,F1,F2,F3,PI,S,
     1WW,GG,E0,E1,E2,E3,EE,S1,S2,S3,S4,S5,S6,
     2H,R,D,D1,AA01,AA11,AA21,AA02,AA12,AA22,
     3AA03,AA13,AA23,AA14,AA24,AA34
      COMMON/MEN/ MM(10),NN(10),MN(6)
      COMMON/TP8/ TANP(9),TAPP(9,9)
      COMMON/TP9/ TAL(7),TANN(7),TGA(7,7,7),TGA2(7,7,7),
     1TDE(7,7,7,7),TDE2(7,7,7,7),THH(3,3,3,3,7),THH2(3,3,3,3,7),
     2TALS(7),TALPC(7),TALC(7),TALPS(7),TBES(7,7),TBEPS(7,7),
     3TBEC(7,7),TBEPC(7,7),TGAS(7,7,7),TGAPS(7,7,7),TGAC(7,7,7),
     4TGAPC(7,7,7),TDEC(7,7,7,7),TDES(7,7,7,7),TDE44(7,7,7,7),
     5TDEPC(7,7,7,7),TDEPS(7,7,7,7)
      COMMON/EP/GAV(10,10,10),ANN(10),GB0(10,6,10),GB1(10,3,10),
     1DB1(10,3,3,10),HB(3,3,3,3,10),BB1(10,10),BF2(10,10),BF3(3),
     2GF3(10,10,10),DF4(10,10,10,10),ALX1(10),ALY1(10),ALZ1(10),
     3ALX2(10),ALY2(10),BEX2(10,10),BEY2(10,10),GAX3(10,10,10),
     4GAY3(10,10,10),GAZ3(10,10,10),DEX4(10,10,10,10),DEY4(10,10,10,10),
     5DEZ4(10,10,10,10),DES(3,3,3,10),ALS(10),BES(10,10),DE4(3,3,3,10),
     6BEX3(10,10),BEY3(10,10),BEZ3(10,10)
      PI=3.141592653589793D0
      UP=1.E-5
      DO 1 I=1,N1
      S1=0.
      S2=0.
      NI=NN(I)
      MI=MM(I)
      SA=TALC(MI)
      SB=TALS(MI)
      IF(SA.EQ.0..AND.SB.EQ.0.) GO TO 101
      DO 201 IG=1,96
      S4=ALL(IG)
  201 S1=S1+WW(IG)*S4**2*PKS(NI,IG)
  101 CONTINUE
      U=F0**3*S1*SA
      IF(ABS(U).LE.UP) U=0.
      ALX1(I)=U
      U=F0**3*S1*SB
      IF(ABS(U).LE.UP) U=0.
      ALY1(I)=U
      ALZ1(I)=0.
    1 CONTINUE
      DO 3 I=1,N1
      S1=0.D0
      S2=0.D0
      S3=0.D0
      NI=NN(I)
      MI=MM(I)
      SA=TALC(MI)
      SB=TALS(MI)
      SC=TALPC(MI)
      SD=TALPS(MI)
      IF(SA.EQ.0..AND.SB.EQ.0.) GO TO 103
      DO 204 IG=1,96
  204 S1=S1+WW(IG)*ALL(IG)*PS(2,1,NI,IG)
  103 CONTINUE
      IF(SC.EQ.0..AND.SD.EQ.0.) GO TO 104
      DO 205 IG=1,96
  205 S2=S2+WW(IG)*PS(1,1,NI,IG)
  104 CONTINUE
      U=F0*(S1*SA-S2*SD)
      IF(ABS(U).LE.UP) U=0.
      ALX2(I)=U
      U=F0*(S1*SB+S2*SC)
      IF(ABS(U).LE.UP) U=0.
    3 ALY2(I)=U
      DO 7 I=1,N1
      NI=NN(I)
      MI=MM(I)
      DO 7 J=1,N1
      NJ=NN(J)
      MJ=MM(J)
      S1=0.
      S2=0.
      S3=0.
      SA=TBEC(MI,MJ)
      SB=TBES(MI,MJ)
      SC=TBEPS(MI,MJ)
      SD=TBEPC(MI,MJ)
      IF(SA.EQ.0..AND.SB.EQ.0.) GO TO 107
      DO 207 IG=1,96
      S4=ALL(IG)
  207 S1=S1+WW(IG)*S4*PKS(NJ,IG)*PS(2,1,NI,IG)
  107 CONTINUE
      IF(SC.EQ.0..AND.SD.EQ.0.) GO TO 108
      DO 208 IG=1,96
  208 S2=S2+WW(IG)*PS(1,1,NI,IG)*PKS(NJ,IG)
  108 CONTINUE
      U=F0*(S1*SA-S2*SC)
      IF(ABS(U).LE.UP) U=0.
      BEX2(I,J)=U
      U=F0*(S1*SB+S2*SD)
      IF(ABS(U).LE.UP) U=0.
    7 BEY2(I,J)=U
      DO 9 I=1,N1
      NI=NN(I)
      MI=MM(I)
      DO 9 J=1,N1
      NJ=NN(J)
      MJ=MM(J)
      S1=0.D0
      S2=0.D0
      S3=0.D0
      SA=TBEC(MI,MJ)
      SB=TBES(MI,MJ)
      SC=TBEPS(MI,MJ)
      SD=TBEPC(MI,MJ)
      IF(SA.EQ.0..AND.SB.EQ.0.) GO TO 109
      DO 209 IG=1,96
      S1=S1+WW(IG)*ALL(IG)*(F1*PS(2,1,NI,IG)+F0/H*
     1PS(2,2,NI,IG))*PKS(NJ,IG)
  209 CONTINUE
  109 CONTINUE
      IF(SC.EQ.0..AND.SD.EQ.0.) GO TO 110
      DO 210 IG=1,96
  210 S2=S2+WW(IG)*(F1*PS(1,1,NI,IG)+F0*PS(1,2,NI,IG)/H)*PKS(NJ,IG)
  110 CONTINUE
      SE=DEL(MI,MJ)*TANN(MI)
      IF(SE.EQ.0.) GO TO 111
      DO 211 IG=1,96
  211 S3=S3+WW(IG)*(F1*PKS(NI,IG)+F0/H*PS(1,5,NI,IG)*0.5)*PKS(NJ,IG)
  111 CONTINUE
      U=S1*SA-S2*SC
      IF(ABS(U).LE.UP) U=0.
      BEX3(I,J)=U
      U=S1*SB+S2*SD
      IF(ABS(U).LE.UP) U=0.
      BEY3(I,J)=U
      U=2.D0*F0*S3*SE
      IF(ABS(U).LE.UP) U=0.
    9 BEZ3(I,J)=U
      DO 13 I=1,N1
      NI=NN(I)
      MI=MM(I)
      DO 13 J=1,N1
      NJ=NN(J)
      MJ=MM(J)
      DO 13 K=1,J
      NK=NN(K)
      MK=MM(K)
      S1=0.
      S2=0.
      S3=0.
      SA=TGA C(MI,MJ,MK)
      SB=TGA S(MI,MJ,MK)
      SC=TGAPS(MI,MJ,MK)
      SD=TGAPC(MI,MJ,MK)
      SE=TGA  (MI,MJ,MK)
      IF(SA.EQ.0..AND.SB.EQ.0.) GO TO 113
      DO 213 IG=1,96
  213 S1=S1+WW(IG)*(F1*PS(2,1,NI,IG)+F0*PS(2,2,NI,IG)/H)*
     1ALL(IG)*PKS(NJ,IG)*PKS(NK,IG)
  113 CONTINUE
      IF(SC.EQ.0..AND.SD.EQ.0.) GO TO 114
      DO 214 IG=1,96
  214 S2=S2+WW(IG)*(F1*PS(1,1,NI,IG)+F0/H*PS(1,2,NI,IG))
     1*PKS(NJ,IG)*PKS(NK,IG)
  114 CONTINUE
      IF(SE.EQ.0.) GO TO 115
      DO 215 IG=1,96
  215 S3=S3+WW(IG)*(2.D0*F1*PKS(NI,IG)+F0*PS(1,5,NI,IG)/H)
     1*PKS(NJ,IG)*PKS(NK,IG)*ALL(IG)
  115 CONTINUE
      U=0.5D0*(S1*SA-S2*SC)
      IF(ABS(U).LE.UP) U=0.
      GAX3(I,J,K)=U
      GAX3(I,K,J)=U
      U=0.5D0*(S1*SB+S2*SD)
      IF(ABS(U).LE.UP) U=0.
      GAY3(I,J,K)=U
      GAY3(I,K,J)=U
      U=0.5D0*F0*S3*SE
      IF(ABS(U).LE.UP) U=0.
      GAZ3(I,K,J)=U
   13 GAZ3(I,J,K)=U
      DO 17 I=1,N1
      NI=NN(I)
      MI=MM(I)
      DO 17 J=1,N1
      NJ=NN(J)
      MJ=MM(J)
      DO 17 K=1,J
      NK=NN(K)
      MK=MM(K)
      DO 17 L=1,N1
      NL=NN(L)
      ML=MM(L)
      S1=0.
      S2=0.
      S3=0.
      SA=TDEC (MI,MJ,MK,ML)
      SB=TDES (MI,MJ,MK,ML)
      SC=TDEPS(MI,MJ,MK,ML)
      SD=TDEPC(MI,MJ,MK,ML)
      SE=TDE  (MI,MJ,MK,ML)
      IF(SA.EQ.0..AND.SB.EQ.0.) GO TO 117
      DO 217  IG=1,96
  217 S1=S1+WW(IG)*ALL(IG)*(F2*PS(2,1,NI,IG)+
     12.D0/H*F1*PS(2,2,NI,IG)+F0/H/H*PS(2,3,NI,IG))
     2*PKS(NJ,IG)*PKS(NK,IG)*PKS(NL,IG)
  117 CONTINUE
      IF(SC.EQ.0..AND.SD.EQ.0.) GO TO 118
      DO 218 IG=1,96
  218 S2=S2+WW(IG)*(F2*PS(1,1,NI,IG)+2.D0/H*F1*
     1PS(1,2,NI,IG)+F0/H/H*PS(1,3,NI,IG))
     2*PKS(NJ,IG)*PKS(NK,IG)*PKS(NL,IG)
  118 CONTINUE
      IF(SE.EQ.0.) GO TO 119
      DO 219 IG=1,96
  219 S3=S3+WW(IG)*(4.D0*F1*F0/H*PS(1,5,NI,IG)+
     12.D0*(F1**2+F2*F0)*PKS(NI,IG)+(F0/H)**2*
     2PS(2,5,NI,IG))*ALL(IG)*PKS(NJ,IG)*PKS(NK,IG)*PKS(NL,IG)
  119 CONTINUE
      U=(S1*SA-S2*SC)/6.
      IF(ABS(U).LE.UP) U=0.
      DEX4(I,J,K,L)=U
      DEX4(I,K,J,L)=U
      U=(S1*SB+S2*SD)/6.D0
      IF(ABS(U).LE.UP) U=0.
      DEY4(I,J,K,L)=U
      DEY4(I,K,J,L)=U
      U=S3*SE/6.D0
      IF(ABS(U).LE.UP) U=0.
      DEZ4(I,J,K,L)=U
   17 DEZ4(I,K,J,L)=U
      DO 20 I=1,N3
      NI=NN(I)
      MI=MM(I)
      DO 20 J=1,N3
      NJ=NN(J)
      MJ=MM(J)
      DO 20 K=1,J
      NK=NN(K)
      MK=MM(K)
      DO 20 L=1,N1
      NL=NN(L)
      ML=MM(L)
      S1=0.
      SA=TDE(MI,MJ,MK,ML)
      IF(SA.EQ.0.) GO TO 120
      DO 220 IG=1,96
  220 S1=S1+WW(IG)*ALL(IG)*PKS(NI,IG)*PKS(NJ,IG)
     1*PKS(NK,IG)*PKS(NL,IG)
  120 CONTINUE
      U=S1*SA
      IF(ABS(U).LE.UP) U=0.
      DE4(I,K,J,L)=U
   20 DE4(I,J,K,L)=U
      RETURN
      END

C    
      SUBROUTINE VRGEO(XX,RR,Z,N9)
      DIMENSION Z(N9),RR(N9)
      COMMON/BC/ GAC(6,6,10),DEC(3,3,3,10),HHC(3,3,3,3,10),
     1VV1(10,10),VV2(10,10,10),VV3(10,10,10,10),UU1(3,10),UU2(3,10,10),
     2UU3(3,10,10,10),UU4(3,10,10,10,10),WW1(10),WW2(10),WW3(6,6,10),
     3WW4(3,3,3,10),VZ2(6,6,10),VZ3(3,3,3,10),UZ2(3,10,10),
     4UZ3(3,6,6,10),UZ4(3,3,3,3,10)
      COMMON/RAZM/ N1,N2,N3,N4
      COMMON/FIZP/ RMS,ROG,G,SR,G0,RKG
      COMMON/RS/ RS(6)
      COMMON/MUP/ MU(10)
      COMMON/AQ/ L13(13),U13(13),P1(13,13),P2(13,13),P3(13),P4(13)
	COMMON /EY/ EPSY,C,EPSYT
      IF(XX.NE.0.) GO TO 101
      DO 1 I=1,6
    1 RS(I)=0.
  101 CONTINUE
      DO 2 I=1,13
      L13(I)=1.
      U13(I)=1.
      P3(I)=0.
      P4(I)=0.
      DO 2 J=1,13
      P1(I,J)=DEL(I,J)
    2 P2(I,J)=0.
    	EPSY=RR(12)
      EPSYT=RR(25)
      DO 5 I=1,N1
      DO 5 IR=1,N1
    5 P1(IR,I)=VV1(I,IR)
      DO 6 IR=1,N1
      DO 6 I=1,N2
      S=0.
      DO 7 J=1,N2
      SA=RR(J)
      IF(SA.EQ.0.) GO TO 117
      S=S+SA*VV2(I,IR,J)
  117 CONTINUE
    7 CONTINUE
    6 P1(IR,I)=P1(IR,I)+S
      DO 8 IR=1,N1
      DO 8 I=1,N3
      S=0.
      DO 9 J=1,N3
      DO 9 K=1,N3
      SA=RR(J)*RR(K)
      IF(SA.EQ.0.) GO TO 119
      S=S+SA*VV3(I,IR,J,K)
  119 CONTINUE
    9 CONTINUE
    8 P1(IR,I)=P1(IR,I)+S
      DO 100 M=1,3
      IM=M+N1
      DO 10 IR=1,N1
   10 P1(IR,IM)=P1(IR,IM)+UU1(M,IR)
      DO 11 IR=1,N1
      S=0.
      DO 12 I=1,N1
      SA=RR(I)
      IF(SA.EQ.0.) GO TO 112
      S=S+SA*UU2(M,IR,I)
  112 CONTINUE
   12 CONTINUE
   11 P1(IR,IM)=P1(IR,IM)+S
      DO 13 IR=1,N1
      S=0.
      DO 14 I=1,N2
      DO 14 J=1,N2
      SA=RR(I)*RR(J)
      IF(SA.EQ.0.) GO TO 114
      S=S+SA*UU3(M,IR,I,J)
  114 CONTINUE
   14 CONTINUE
   13 P1(IR,IM)=P1(IR,IM)+S
      DO 15 IR=1,N1
      S=0.
      DO 16 I=1,N3
      DO 16 J=1,N3
      DO 16 K=1,N3
      SA=RR(I)*RR(J)*RR(K)
      IF(SA.EQ.0.) GO TO 116
      S=S+SA*UU4(M,IR,I,J,K)
  116 CONTINUE
   16 CONTINUE
   15 P1(IR,IM)=P1(IR,IM)+S
  100 CONTINUE
      DO 17 IR=1,N1
      S=0.
      DO 18 I=1,N2
      NI=N4+I
      DO 18 J=1,N2
      NJ=N4+J
      SA=RR(NI)*RR(NJ)
      IF(SA.EQ.0.) GO TO 118
      S=S+SA*VZ2(I,J,IR)
  118 CONTINUE
   18 CONTINUE
   17 P3(IR)=P3(IR)+S
      DO 19 IR=1,N1
      S=0.
      DO 20 I=1,N3
      NI=N4+I
      DO 20 J=1,N3
      NJ=N4+J
      DO 20 K=1,N3
      SA=RR(NI)*RR(NJ)*RR(K)
      IF(SA.EQ.0.) GO TO 120
      S=S+SA*VZ3(I,J,K,IR)
  120 CONTINUE
   20 CONTINUE
   19 P3(IR)=P3(IR)+S
      DO 21 IR=1,N1
      S=0.
      DO 22 M=1,3
      IM=M+N1+N4
      DO 22 I=1,N1
      NI=I+N4
      SA=RR(IM)*RR(NI)
      IF(SA.EQ.0.) GO TO 122
      S=S+SA*UZ2(M,I,IR)
  122 CONTINUE
   22 CONTINUE
   21 P3(IR)=P3(IR)+S
      DO 23 IR=1,N1
      S=0.
      DO 24 M=1,3
      IM=M+N1+N4
      DO 24 I=1,N2
      NI=I+N4
      DO 24 J=1,N2
      SA=RR(IM)*RR(NI)*RR(J)
      IF(SA.EQ.0.) GO TO 124
      S=S+SA*UZ3(M,I,J,IR)
  124 CONTINUE
   24 CONTINUE
   23 P3(IR)=P3(IR)+S
      DO 25 IR=1,N1
      S=0.
      DO 26 M=1,3
      IM=M+N1+N4
      DO 26 I=1,N3
      NI=I+N4
      DO 26 J=1,N3
      DO 26 K=1,N3
      SA=RR(IM)*RR(NI)*RR(J)*RR(K)
      IF(SA.EQ.0.) GO TO 126
      S=S+SA*UZ4( M,I,J,K,IR)
  126 CONTINUE
   26 CONTINUE
   25 P3(IR)=P3(IR)+S
      DO 27 IR=1,N1
      S=0.5*G*WW1(IR)
   27 P3(IR)=P3(IR)-S
      DO 28 IR=1,N1
      S=RR(IR)*WW2(IR)*G
   28 P3(IR)=P3(IR)-S
      DO 30 IR=1,N1
      S=0.
      DO 31 I=1,N2
      DO 31 J=1,N2
      SA=RR(I)*RR(J)
      IF(SA.EQ.0.) GO TO 131
      S=S+SA*WW3(I,J,IR)
  131 CONTINUE
   31 CONTINUE
      S=S*1.5*G
   30 P3(IR)=P3(IR)-S
      DO 32 IR=1,N1
      S=0.
      DO 33 I=1,N3
      DO 33 J=1,N3
      DO 33 K=1,N3
      SA=RR(I)*RR(J)*RR(K)
      IF(SA.EQ.0.) GO TO 133
      S=S+SA*WW4(I,J,K,IR)
  133 CONTINUE
   33 CONTINUE
      S=S*2.*G
   32 P3(IR)=P3(IR)-S
      DO 40 M=1,3
      IM=M+N1
      DO 34 I=1,N1
   34 P1(IM,I)=P1(IM,I)+ROG/RMS*UU1(M,I)
      DO 35 I=1,N2
      S=0.
      DO 36 J=1,N2
      SA=RR(J)
      IF(SA.EQ.0.) GO TO 136
      S=S+SA*UU2(M,I,J)
  136 CONTINUE
   36 CONTINUE
   35 P1(IM,I)=P1(IM,I)+ROG/RMS*S
      DO 37 I=1,N3
      S=0.
      DO 38 J=1,N3
      DO 38 K=1,N3
      SA=RR(J)*RR(K)
      IF(SA.EQ.0.) GO TO 138
      S=S+SA*UU3(M,I,J,K)
  138 CONTINUE
   38 CONTINUE
   37 P1(IM,I)=P1(IM,I)+ROG/RMS*S
      P1(IM,IM)=1.
      S1=XSILA(XX)
      S2=YSILA(XX)
      S3=ZSILA(XX)-G
      P3(N1+1)=S1
      RS(1)=S1
      P3(N1+2)=S2
      RS(2)=S2
      P3(N1+3)=S3
      RS(3)=S3
      S=0.
      DO 41 I=1,N2
      NI=I+N4
      DO 41 J=1,N2
      NJ=J+N4
      SA=RR(NI)*RR(NJ)
      IF(SA.EQ.0.) GO TO 141
      S=S+SA*UU2(M,I,J)
  141 CONTINUE
   41 P3(IM)=P3(IM)-ROG/RMS*S
      S=0.
      DO 42 I=1,N3
      NI=I+N4
      DO 42 J=1,N3
      NJ=J+N4
      DO 42 K=1,N3
      SA=RR(NI)*RR(NJ)*RR(K)
      IF(SA.EQ.0.) GO TO 142
      S=S+SA*UU3(M,I,J,K)
  142 CONTINUE
   42 P3(IM)=P3(IM)-ROG/RMS*S*2.
   40 CONTINUE
      DO 80 I=1,N1
      P3(I)=P3(I)/VV1(I,I)
      DO 80 J=1,N4
   80 P1(I,J)=P1(I,J)/VV1(I,I)
      DO 50 I=1,N4
      P4(I)=P3(I)
      DO 50 J=1,N4
      P2(I,J)=P1(I,J)
   50 CONTINUE
      DO 51 I=1, 3
      IF(MU(I).NE.0) GO TO 52
      NI=I+N1
      P4(NI)=0.
      DO 53 J=1,N1
      P1(NI,J)=0.
   53 P1(J,NI)=0.
      P1(NI,NI)=1.
   52 CONTINUE
   51 CONTINUE
      IF(XX.EQ.0.) PRINT 212,P1,P3
      IF(XX.EQ.0.1) PRINT 212,P1,P3
  212 FORMAT(5X,13F7.4)
      JA=13
	CALL VLINAR(P1,P4,JA,N4,L13,U13)
	DO 59 I=1,N4
      NI=I+N4
      Z(I)=RR(NI)
   59 Z(NI)=P4(I)
      DO 60 I=1,3
      S=0.
      NI=N1+I
      DO 61 J=1,N4
      IF(NI.EQ.J) GO TO 62
      S=S+P2(NI,J)*P3(J)
   62 CONTINUE
   61 CONTINUE
   60 RS(I+3)=P3(NI)-S-RS(I)
      IF(XX.EQ.0.) PRINT 65,RS
   65 FORMAT(3X,6F15.7)
      RETURN
      END

      SUBROUTINE VRRKKS(X,Y1,HH,N9,VFF1,YY,RR,Z)
      DIMENSION Y1(N9),YY(N9),RR(N9),Z(N9),A(5)
      A(1)=HH/2
      A(2)=HH/2
      A(5)=HH/2
      A(3)=HH
      A(4)=HH
      DO 1 I=1,N9
      RR(I)=Y1(I)
    1 YY(I)=Y1(I)
      XX=X
      DO 2 J=1,4
      CALL VFF1(XX,RR,Z,N9)
      XX=X+A(J)
      DO 3 I=1,N9
      YY(I)=YY(I)+A(J+1)*Z(I)/3
      RR(I)=Y1(I)+A(J)*Z(I)
    3 CONTINUE
    2 CONTINUE
      DO 4 I=1,N9
    4 Y1(I)=YY(I)

      RETURN
      END

      DOUBLE PRECISION FUNCTION WF(M,K,R,Z)
      DOUBLE PRECISION R,Z,X,Y,S,T,V
      COMMON/YY/ Y(5,50)
      IF(K-M) 1,2,2
    1 WF=0.D0
      RETURN
    2 M1=M+1
      K1=K+1
      K11=K-1
      Y(1,1)=1.D0
      IF(M.NE.0) Y(M1,M1)=R**M
      Y(M1,M1+1)=Z*R**M
      KM=K-M
      IF(KM.LT.2) GO TO 7
      DO 4 I1=M1,K11
      I=I1-1
      Y(M1,I1+2)=((2*I+3)*Z*Y(M1,I1+1)-
     1(I-M+1)*(R**2+Z**2)*Y(M1,I1))/(I+M+2)
    4 CONTINUE
    7 V=Y(M1,K1)
      WF=V
      RETURN
      END

C     POINTS AND WEITS FOR GAUSS INTEGRALS
C     IT COULD BE TRANSFORMED TO DAT FILE
      SUBROUTINE WGL
      DOUBLE PRECISION W,G
      COMMON/WG/ W(48),G(48)
      G(1)=0.0162767448496030
      G(2)=0.0488129851360497
      G(3)=0.0812974954644256
      G(4)=0.1136958501106659
      G(5)=0.1459737146548969
      G(6)=0.1780968823676186
      G(7)=0.2100313104605672
      G(8)=0.2417431561638400
      G(9)=0.2731988125910491
      G(10)=0.3043649443544964
      G(11)=0.3352085228926254
      G(12)=0.3656968614723136
      G(13)=0.3957976498289086
      G(14)=0.4254789884073005
      G(15)=0.4547094221677430
      G(16)=0.4834579739205964
      G(17)=0.5116941771546677
      G(18)=0.5393881083243574
      G(19)=0.5665104185613972
      G(20)=0.5930323647775721
      G(21)=0.6189258401254686
      G(22)=0.6441634037849671
      G(23)=0.6687183100439162
      G(24)=0.6925645366421716
      G(25)=0.7156768123489676
      G(26)=0.7380306437444001
      G(27)=0.7596023411766475
      G(28)=0.7803690438674332
      G(29)=0.8003087441391308
      G(30)=0.8194003107379317
      G(31)=0.8376235112281871
      G(32)=0.8549590334346015
      G(33)=0.8713885059092965
      G(34)=0.8868945174024204
      G(35)=0.9014606353158523
      G(36)=0.9150714231208981
      G(37)=0.9277124567223087
      G(38)=0.9393703397527552
      G(39)=0.9500327177844376
      G(40)=0.9596882914487425
      G(41)=0.9683268284632642
      G(42)=0.9759391745851365
      G(43)=0.9825172635630147
      G(44)=0.9880541263296238
      G(45)=0.9925439003237626
      G(46)=0.9959818429872093
      G(47)=0.9983643758631817
      G(48)=0.9996895038822308
      W(1)=0.0325506144923632
      W(2)=0.0325161187138688
      W(3)=0.0324471637140643
      W(4)=0.0323438225685759
      W(5)=0.0322062047940303
      W(6)=0.0320344562319927
      W(7)=0.0318287588944110
      W(8)=0.0315893307707272
      W(9)=0.0313164255968614
      W(10)=0.0310103325863138
      W(11)=0.0306713761236691
      W(12)=0.0302999154208276
      W(13)=0.0298963441363284
      W(14)=0.0294610899581679
      W(15)=0.0289946141505552
      W(16)=0.0284974110650854
      W(17)=0.0279700076168483
      W(18)=0.0274129627260292
      W(19)=0.0268268667255918
      W(20)=0.0262123407356724
      W(21)=0.0255700360053494
      W(22)=0.0249006332224836
      W(23)=0.0242048417923647
      W(24)=0.0234833990859262
      W(25)=0.0227370696583294
      W(26)=0.0219666444387443
      W(27)=0.0211729398921913
      W(28)=0.0203567971543333
      W(29)=0.0195190811401450
      W(30)=0.0186606796274115
      W(31)=0.0177825023160453
      W(32)=0.0168854798642452
      W(33)=0.0159705629025623
      W(34)=0.0150387210269949
      W(35)=0.0140909417723149
      W(36)=0.0131282295669616
      W(37)=0.0121516046710883
      W(38)=0.0111621020998385
      W(39)=0.0101607705350084
      W(40)=0.0091486712307834
      W(41)=0.0081268769256988
      W(42)=0.0070964707911539
      W(43)=0.0060585455042360
      W(44)=0.0050142027429275
      W(45)=0.0039645543384447
      W(46)=0.0029107318179349
      W(47)=0.0018539607889469
      W(48)=0.0007967920655520
      RETURN
      END

      DOUBLE PRECISION FUNCTION WRR(M,K,R,Z)
      DOUBLE PRECISION R,Z,V,X,WR,WF
      IF(K-M  ) 1,2,2
    1 WRR=0.D0
      RETURN
    2 WRR=(-K*WF(M,K,R,Z)+(K-M)*Z*WF(M,K-1,R,Z)) /
     /R**2+(K*WR(M,K,R,Z)-(K-M)*Z*WR(M,K-1,R,Z))/R
      RETURN
      END

      DOUBLE PRECISION FUNCTION WZ(M,K,R,Z)
      DOUBLE PRECISION R,Z,V,W,X,Y,WF
      IF(K-M) 1,1,2
    1 WZ=0.D0
      RETURN
    2 WZ=(K-M)*WF(M,K-1,R,Z)
      RETURN
      END

      DOUBLE PRECISION FUNCTION WZR(M,K,R,Z)
      DOUBLE PRECISION R,Z,V,X,WR
      IF(K-M) 1,1,2
    1 WZR=0.D0
      RETURN
    2 WZR=(K-M)*WR(M,K-1,R,Z)
      RETURN
      END

      DOUBLE PRECISION FUNCTION WZRR(M,K,R,Z)
      DOUBLE PRECISION R,Z,V,X,WRR
      IF(K-M) 1,1,2
    1 WZRR=0.D0
      RETURN
    2 WZRR=(K-M)*WRR(M,K-1,R,Z)
      RETURN
      END

      DOUBLE PRECISION FUNCTION WZZ(M,K,R,Z)
      DOUBLE PRECISION R,Z,V,X,WZ
      IF(K-M) 1,2,2
    1 WZZ=0.D0
      RETURN
    2 WZZ=(K-M)*WZ(M,K-1,R,Z)
      RETURN
      END

      DOUBLE PRECISION FUNCTION WZZR(M,K,R,Z)
      DOUBLE PRECISION R,Z,V,X,WZR
      IF(K-M) 1,2,2
    1 WZZR=0.D0
      RETURN
    2 WZZR=(K-M)*WZR(M,K-1,R,Z)
      RETURN
      END

      DOUBLE PRECISION FUNCTION WZZZ(M,K,R,Z)
      DOUBLE PRECISION R,Z,V,X,WZZ
      IF(K-M-2) 1,1,2
    1 WZZZ=0.D0
      RETURN
    2 WZZZ=(K-M)*WZZ(M,K-1,R,Z)
      RETURN
      END

C     SOLVE, DECOMP AND LINAR FOR DOUBLE PRECISION
C     PLACED TOGETHER
      SUBROUTINE VOLDE(NDIM,N,A,B,IPVT)
      IMPLICIT REAL*4 (A-H,O-Z)
      INTEGER NDIM,N,IPVT(N)
      DIMENSION
     1     A(NDIM,N),B(N)
C    B=CTAHOBiTCq BEKTOPOM   PE{EHiq
      INTEGERR KB,KM1,NM1,KP1,I,J,K,M
      IF(N.EQ.1) GO TO 50
      NM1=N-1
      DO 20 K=1,NM1
      KP1=K+1
      M=IPVT(K)
      T=B(M)
      B(M)=B(K)
      B(K)=T
      DO 10 I=KP1,N
      B(I)=B(I)+A(I,K)*T
   10 CONTINUE
   20 CONTINUE
      DO 40 KB=1,NM1
      KM1=N-KB
      K=KM1+1
      B(K)=B(K)/A(K,K)
      T=-B(K)
      DO 30 I=1,KM1
      B(I)=B(I)+A(I,K)*T
   30 CONTINUE
   40 CONTINUE
   50 B(1)=B(1)/A(1,1)
      RETURN
      END

      SUBROUTINE VLINAR (A,B,NDIM,N,IPVT,WORK)
      IMPLICIT REAL*4 (A-H,O-Z)
      COMMON /COND/ COND
      DIMENSION A(NDIM,NDIM),B(NDIM)
      DIMENSION IPVT(N),WORK(N)
C     Matrix A and column B unichtozhayutsya
      COND=1.
      CALL VECOMD(NDIM,N,A,COND,IPVT,WORK)
      CONDP1=COND+1.
      IF(CONDP1.EQ.COND) PRINT 4
    4 FORMAT(3X,'Plokhaya obuslovlennost A' )
      IF(CONDP1.EQ.COND) STOP
      CALL VOLDE(NDIM,N,A,B,IPVT)
      IF (COND.GT.100 .)   PRINT 1001, COND
 1001 FORMAT(10X,'COND=', E15.5)
      RETURN
      END

      SUBROUTINE VECOMD(NDIM,N,A,COND,IPVT,WORK)
      IMPLICIT REAL*4 (A-H,O-Z)
      INTEGER NDIM,N
      DIMENSION A(NDIM,N),WORK(N)
      INTEGER IPVT(N)
C    NDIM=zAqBlEHHAq PAzMEPHOCTx MACCiBA A(NDIM,NDIM)
C   N=pOPqdOK MATPicy liHEjHOj CiCTEMy
C    A- MATPicA KOTOPAq budET PAzlOvEHA
C     dAlEE B A - PEzulxTAT
C   COND= OcEHKA ObuClOBlEHHOCTi MATPicy, fAKTi~ECKi COND pOKAzyBAET
C    BO CKOlxKO pAz izMEHEHiq B A i B MOguT ByzBATx izMEHEHiq B PE{EHii
C   ECli COND=10**32, TO OSTANOW
C    OpPEdEliTElx DET(A)=IPVT(N)*A(1,1)*...*A(N,N)
      INTEGER NM1,I,J,K,KP1,KB,KM1,M
      IPVT(N)=1
      E0=0.D0
      E1=1.D0
      IF(N.EQ.1) GO TO 80
      NM1=N-1
      ANORM=0.
      DO 10 J=1,N
      T=0.
      DO 5 I=1,N
      T=T+ABS(A(I,J))
    5 CONTINUE
      IF(T.GT.ANORM) ANORM=T
   10 CONTINUE
      DO 35 K=1,NM1
      KP1=K+1
      M=K
      DO 15 I=KP1,N
      IF(ABS(A(I,K)).GT.ABS(A(M,K))) M=I
   15 CONTINUE
      IPVT(K)=M
      IF(M.NE.K) IPVT(N)=-IPVT(N)
      T=A(M,K)
      A(M,K)=A(K,K)
      A(K,K)=T
      IF(T.EQ.E0) GO TO 351
      DO 20 I=KP1,N
      A(I,K)=-A(I,K)/T
   20 CONTINUE
      DO 30 J=KP1,N
      T=A(M,J)
      A(M,J)=A(K,J)
      A(K,J)=T
      IF(T.EQ.E0) GO TO 301
      DO 25 I=KP1,N
      A(I,J)=A(I,J)+A(I,K)*T
   25 CONTINUE
  301 CONTINUE
   30 CONTINUE
  351 CONTINUE
   35 CONTINUE
      DO 50 K=1,N
      T=0.
      IF(K.EQ.1) GO TO 45
      KM1=K-1
      DO 40 I=1,KM1
      T=T+A(I,K)*WORK(I)
   40 CONTINUE
   45 EK=1.0
      IF(T.LT.E0) EK=-1.
      IF(A(K,K).EQ.E0) GO TO 90
      WORK(K)=-(EK+T )/A(K,K)
   50 CONTINUE
      DO 60 KB=1,NM1
      K=N-KB
      T=0.
      KP1=K+1
      DO 55 I=KP1,N
      T=T+A(I,K)*WORK(K)
   55 CONTINUE
      WORK(K)=T
      M=IPVT(K)
      IF(M.EQ.K) GO TO 601
      T=WORK(M)
      WORK(M)=WORK(K)
      WORK(K)=T
  601 CONTINUE
   60 CONTINUE
      YNORM=0.
      DO 65 I=1,N
      YNORM=YNORM+ABS(WORK(I))
   65 CONTINUE
      CALL VOLDE(NDIM,N,A,WORK,IPVT)
      ZNORM=0.
      DO 70 I=1,N
      ZNORM=ZNORM+ABS(WORK(I))
   70 CONTINUE
      COND=ANORM*ZNORM/YNORM
      IF(COND.LT.E1) COND=1.
      RETURN
   80 COND=1.
      IF(A(1,1).NE.E0) RETURN
   90 COND=1.0E+32
      RETURN
      END

C   EIGEN FROM VILENKIN
      SUBROUTINE VIGEN(A,R,N,MV)
      DIMENSION A(1),R(1)
C   ObPAzOBAHiE EdiHi~HOj MATPicy
C     DOHBLE PRECISION A,R,ANORM,ANRMX,THR,X,Y,SINX,
C     SINX2,COSX,COSX2,SINCS,RANGE
      DOUBLE PRECISION A,R,ANORM,ANRMX,THR,X,Y,SINX,
     *SINX2,COSX,COSX2,SINCS,RANGE
    5 RANGE=1.0D-19
      IF(MV-1)10,25,10
   10 IQ=-N
      DO 22 J=1,N
      IQ=IQ+N
      DO 22 I=1,N
      IJ=IQ+I
      R(IJ)=0.0
      IF(I-J) 20,15,20
   15 R(IJ)=1.0
   20 CONTINUE
   22 CONTINUE
C   By~iClEHiE HA~AlxHOj i KOHE~HOj HOPM
   25 ANORM=0.0
      DO 36 I=1,N
      DO 36 J=1,N
      IF(I-J) 30,35,30
   30 IA=I+(J*J-J)/2
      ANORM=ANORM+A(IA)*A(IA)
   35 CONTINUE
   36 CONTINUE
      IF(ANORM) 165,165,40
   40 ANORM=DSQRT(ANORM)*1.41421356237309D0
      ANRMX=ANORM*RANGE/FLOAT(N)
C   ObPOzOBAHiE iHdiKATOPOB i By~iClEHiE pOPOgA THR
      IND=0
      THR=ANORM
   45 THR=THR/FLOAT(N)
   50 L=1
   55 M=L+1
C   By~iClEHiE SIN U COS
   60 MQ=(M*M-M)/2
      LQ=(L*L-L)/2
      LM=L+MQ
   62 IF(DABS(A(LM))-THR) 130,65,65
   65 IND=1
      LL=L+LQ
      MM=M+MQ
      X=0.5*(A(LL)-A(MM))
   68 Y=-A(LM)/DSQRT(A(LM)*A(LM)+X*X)
      IF(X) 70,75,75
   70 Y=-Y
   75 SINX=Y/DSQRT(2.0*(1.0+(DSQRT(1.0-Y*Y))))
      SINX2=SINX*SINX
   78 COSX=DSQRT(1.0-SINX2)
      COSX2=COSX*COSX
      SINCS=SINX*COSX
C   pEPECTAHOBKA L i M CTOlbcOB
      ILQ=N*(L-1)
      IMQ=N*(M-1)
      DO 125 I=1,N
      IQ=(I*I-I)/2
      IF(I-L) 80,115,80
   80 IF(I-M) 85,115,90
   85 IM=I+MQ
      GO TO 95
   90 IM=M+IQ
   95 IF(I-L) 100,105,105
  100 IL=I+LQ
      GO TO 110
  105 IL=L+IQ
  110 X=A(IL)*COSX-A(IM)*SINX
      A(IM)=A(IL)*SINX+A(IM)*COSX
      A(IL)=X
  115 IF(MV-1) 120,125,120
  120 ILR=ILQ+I
      IMR=IMQ+I
      X=R(ILR)*COSX-R(IMR)*SINX
      R(IMR)=R(ILR)*SINX+R(IMR)*COSX
      R(ILR)=X
  125 CONTINUE
      X=2.0*A(LM)*SINCS
      Y=A(LL)*COSX2+A(MM)*SINX2-X
      X=A(LL)*SINX2+A(MM)*COSX2+X
      A(LM)=(A(LL)-A(MM))*SINCS+A(LM)*(COSX2-SINX2)
      A(LL)=Y
      A(MM)=X
C   pPOBEPKA i zABEP{EHiE
C   qBlqETCq li M pOClEdHiM CTOlbcOM
  130 IF(M-N) 135,140,135
  135 M=M+1
      GO TO 60
C   qBlqETCq li L BTOPyM C KOHcA CTOlbcOM
  140 IF(L-(N-1)) 145,150,145
  145 L=L+1
      GO TO 55
  150 IF(IND-1) 160,155,160
  155 IND=0
      GO TO 50
C   CPABHEHiE pOPOgOBOgO zHA~EHiq C KOHE~HOj HOPMOj
  160 IF(THR-ANRMX) 165,165,45
C  COPTiPOBKA CObCTBEHHyX zHA~EHij i BEKTOPOB
  165 IQ=-N
      DO 186 I=1,N
      IQ=IQ+N
      LL=I+(I*I-I)/2
      JQ=N*(I-2)
      DO 186 J=I,N
      JQ=JQ+N
      MM=J+(J*J-J)/2
      IF(A(LL)-A(MM)) 170,185,185
  170 X=A(LL)
      A(LL)=A(MM)
      A(MM)=X
      IF(MV-1) 175,185,175
  175 DO 180 K=1,N
      ILR=IQ+K
      IMR=JQ+K
      X=R(ILR)
      R(ILR)=R(IMR)
  180 R(IMR)=X
  185 CONTINUE
  186 CONTINUE
      RETURN
      END

C    NROOT FROM VILENKIN
      SUBROUTINE VNROOT(M,A,B,XL,X)
      DIMENSION A(1),B(1),XL(1),X(1)
      DOUBLE PRECISION A,B,XL,X,SUMV
      K=1
      DO 100 J=2,M
      L=M*(J-1)
      DO 100 I=1,J
      L=L+1
      K=K+1
  100 B(K)=B(L)
      MV=0
      CALL VIGEN(B,X,M,MV)
      L=0
      DO 110 J=1,M
      L=L+J
  110 XL(J)=1./DSQRT(DABS(B(L)))
      K=0
      DO 115 J=1,M
      DO 115 I=1,M
      K=K+1
  115 B(K)=X(K)*XL(J)
      DO 120 I=1,M
      N2=0
      DO 120 J=1,M
      N1=M*(I-1)
      L=M*(J-1)+I
      X(L)=0.0
      DO 120 K=1,M
      N1=N1+1
      N2=N2+1
  120 X(L)=X(L)+B(N1)*A(N2)
      L=0
      DO 130 J=1,M
      DO 130 I=1,J
      N1=I-M
      N2=M*(J-1)
      L=L+1
      A(L)=0.0
      DO 130 K=1,M
      N1=N1+M
      N2=N2+1
  130 A(L)=A(L)+X(N1)*B(N2)
      CALL VIGEN(A,X,M,MV)
      L=0
      DO 140 I=1,M
      L=L+I
  140 XL(I)=A(L)
      DO 150 I=1,M
      N2=0
      DO 150 J=1,M
      N1=I-M
      L=M*(J-1)+I
      A(L)=0.0
      DO 150 K=1,M
      N1=N1+M
      N2=N2+1
  150 A(L)=A(L)+B(N1)*X(N2)
      L=0
      K=0
      DO 180 J=1,M
      SUMV=0.0
      DO 170 I=1,M
      L=L+1
  170 SUMV=SUMV+A(L)*A(L)
  175 SUMV=DSQRT(SUMV)
      DO 180 I=1,M
      K=K+1
  180 X(K)=A(K)/SUMV
      RETURN
      END

C    CALCULATION OF INTEGRALS FOR ANGULAR DIRECTION IN GEO-VARIANT
      SUBROUTINE VUGEL(K1,K2,K3,K4 )
      COMMON/WG/ W1(48),G1(48)
       COMMON/UH0/ TP(9,96),TM(9,96),TPP(9,96),TPM(9,96)
      COMMON/WGA/ W(96),G(96)
      DOUBLE PRECISION TP,TM,TPP,TPM,W,G,WP,WM,PI,
     1S1,S2,S3,S4,S5,S6,SA,SB,SC,SD,SE,SF,SG,SH,
     2SI,SJ,SK,SL,SW,SX,SY,SS,TPS,TPSP,W1,G1
      COMMON/TP8/   TANP(9),TAPP(9,9)
      COMMON/TP9/ TAL(7),TANN(7),TGA(7,7,7),TGA2(7,7,7),
     1TDE(7,7,7,7),TDE2(7,7,7,7),THH(3,3,3,3,7),THH2(3,3,3,3,7),
     2TALS(7),TALPC(7),TALC(7),TALPS(7),TBES(7,7),TBEPS(7,7),
     3TBEC(7,7),TBEPC(7,7),TGAS(7,7,7),TGAPS(7,7,7),TGAC(7,7,7),
     4TGAPC(7,7,7),TDEC(7,7,7,7),TDES(7,7,7,7),TDE44(7,7,7,7),
     5TDEPC(7,7,7,7),TDEPS(7,7,7,7)
      N1=K1
      N2=K2
      N3=K3
      N1I=K4
      PI=3.141592653589793
      SS=0.0001
      DO 100 I=1,48
      W(I)=W1( 49-I)*0.5
      W(97-I)=W1(49-I)*0.5
      G(I)=-G1(49-I)
      G(97-I)=G1(49-I)
  100 CONTINUE
      DO 1 J=1,96
      WP=PI*(1.+G(J))*0.5
      WM=WP+PI
      DO 1 I=1,N1
      TP(I,J)=TPS(I,WP)
      TM(I,J)=TPS(I,WM)
      TPP(I,J)=TPSP(I,WP)
      TPM(I,J)=TPSP(I,WM)
    1 CONTINUE
      DO 2 I=1,N1
      S1=0.
      S2=0.
      S3=0.
      S4=0.
      S5=0.
      S6=0.
      SX=0.
      DO 21 J=1,96
      SA=TP(I,J)
      SB=TM(I,J)
      SC=TPP(I,J)
      SD=TPM(I,J)
      SE=TP(1,J)
      SF=TM(1,J)
      SG=TP(2,J)
      SH=TM(2,J)
      SW=W(J)
      S1=S1+SW*(SA+SB)
      S2=S2+SW*(SA**2+SB**2)
      S3=S3+SW*(SA*SE+SB*SF)
      S4=S4+SW*(SC*SE+SD*SF)
      S5=S5+SW*(SA*SG+SB*SH)
      S6=S6+SW*(SC*SG+SD*SH)
      SX=SX+SW*(SC**2+SD**2)
   21 CONTINUE
      IF(DABS(S1).LT.SS) S1=0.
      IF(DABS(S2).LT.SS) S2=0.
      IF(DABS(S3).LT.SS) S3=0.
      IF(DABS(S4).LT.SS) S4=0.
      IF(DABS(S5).LT.SS) S5=0.
      IF(DABS(S6).LT.SS) S6=0.
      IF(DABS(SX).LT.SS) SX=0.
      TAL(I)=S1*PI
      TANN(I)=S2*PI
      TALS(I)=S3*PI
      TALPS(I)=S4*PI
      TALC(I)=S5*PI
      TALPC(I)=S6*PI
      TANP(I)=SX*PI
    2 CONTINUE
      DO 3 I=1,N1
      DO 3 J=1,N1
      S1=0.
      S2=0.
      S3=0.
      S4=0.
      S5=0.
      DO 31 K=1,96
      SW=W(K)
      SA=TP(J,K)
      SB=TM(J,K)
      SC=TP(I,K)
      SD=TM(I,K)
      SE=TPP(I,K)
      SF=TPM(I,K)
      SG=TP(1,K)
      SH=TM(1,K)
      SI=TP(2,K)
      SJ=TM(2,K)
      S1=SW*(SC*SA*SG+SD*SB*SH)+S1
      S2=SW*(SE*SA*SG+SF*SB*SH)+S2
      S3=S3+SW*(SC*SA*SI+SD*SB*SJ)
      S4=S4+SW*(SE*SA*SI+SF*SB*SJ)
      S5=S5+SW*(SE*SA   +SF*SB)
   31 CONTINUE
      IF(DABS(S1).LT.SS) S1=0.
      IF(DABS(S2).LT.SS) S2=0.
      IF(DABS(S3).LT.SS) S3=0.
      IF(DABS(S4).LT.SS) S4=0.
      IF(DABS(S5).LT.SS) S5=0.
      TBES(I,J)=S1*PI
      TBEPS(I,J)=S2*PI
      TBEC(I,J)=S3*PI
      TBEPC(I,J)=S4*PI
      TAPP(I,J)=S5*PI
    3 CONTINUE
      DO 4 I=1,N1
      DO 4 J=1,N1
      DO 4 K=1,N1
      S1=0.
      S2=0.
      DO 41 L=1,96
      S1=S1+W(L)*(TP(I,L)*TP(J,L)*TP(K,L)+TM(I,L)*TM(J,L)*TM(K,L))
      S2=S2+W(L)*(TPP(I,L)*TPP(J,L)*TP(K,L)+TPM(I,L)*TPM(J,L)*TM(K,L))
   41 CONTINUE
      IF(DABS(S1).LT.SS) S1=0.
      IF(DABS(S2).LT.SS) S2=0.
      TGA(I,J,K)=S1*PI
      TGA2(I,J,K)=S2*PI
    4 CONTINUE
      DO 5 I=1,N1
      DO 5 J=1,N2
      DO 5 K=1,N1
      S1=0.
      S2=0.
      S3=0.
      S4=0.
      DO 51 L=1,96
      SA=TP(I,L)
      SB=TM(I,L)
      SC=TPP(I,L)
      SD=TPM(I,L)
      SE=TP(J,L)
      SF=TM(J,L)
      SG=TP(K,L)
      SH=TM(K,L)
      SI=TP(1,L)
      SJ=TM(1,L)
      SK=TP(2,L)
      SL=TM(2,L)
      SW=W(L)
      S1=S1+SW*(SA*SE*SG*SI+SB*SF*SH*SJ)
      S2=S2+SW*(SC*SE*SG*SI+SD*SF*SH*SJ)
      S3=S3+SW*(SA*SE*SG*SK+SB*SF*SH*SL)
      S4=S4+SW*(SC*SE*SG*SK+SD*SF*SH*SL)
   51 CONTINUE
      IF(DABS(S1).LT.SS) S1=0.
      IF(DABS(S2).LT.SS) S2=0.
      IF(DABS(S3).LT.SS) S3=0.
      IF(DABS(S4).LT.SS) S4=0.
      TGAS(I,J,K)=S1*PI
      TGAPS(I,J,K)=S2*PI
      TGAC(I,J,K)=S3*PI
      TGAPC(I,J,K)=S4*PI
    5 CONTINUE
      DO 6 I=1,N1I
      DO 6 J=1,N3
      DO 6 K=1,N3
      DO 6 L=1,N1
      S1=0.
      S2=0.
      S4=0.
      S3=0.
      DO 61 M=1,96
      SA=TP(I,M)
      SB=TM(I,M)
      SC=TPP(I,M)
      SD=TPM(I,M)
      SE=TP(J,M)
      SF=TM(J,M)
      SG=TP(K,M)
      SH=TM(K,M)
      SI=TP(1,M)
      SJ=TM(1,M)
      SK=TP(2,M)
      SL=TM(2,M)
      SW=W(M)
      SX=TP(L,M)
      SY=TM(L,M)
      S1=S1+SW*(SA*SE*SG*SX*SK+SB*SF*SH*SY*SL)
      S2=S2+SW*(SA*SE*SG*SX*SI+SB*SF*SH*SY*SJ)
      S3=S3+SW*(SC*SE*SG*SX*SK+SD*SF*SH*SY*SL)
      S4=S4+SW*(SC*SE*SG*SX*SI+SD*SF*SH*SY*SJ)
   61 CONTINUE
      IF(DABS(S1).LT.SS) S1=0.
      IF(DABS(S2).LT.SS) S2=0.
      IF(DABS(S3).LT.SS) S3=0.
      IF(DABS(S4).LT.SS) S4=0.
      TDEC(I,J,K,L)=S1*PI
      TDES(I,J,K,L)=S2*PI
      TDEPC(I,J,K,L)=S3*PI
      TDEPS(I,J,K,L)=S4*PI
    6 CONTINUE
      DO 7 J=1,N1I
      DO 7 I=1,N1
      DO 7 K=1,N3
      DO 7 L=1,N1
      S1=0.
      S2=0.
      DO 71 M=1,96
      S1=S1+W(M)*(TP(I,M)*TP(J,M)*TP(K,M)*TP(L,M)+
     1TM(I,M)*TM(J,M)*TM(K,M)*TM(L,M))
      S2=S2+W(M)*(TPP(I,M)*TPP(J,M)*TP(K,M)*TP(L,M)+
     1TPM(I,M)*TPM(J,M)*TM(K,M)*TM(L,M))
   71 CONTINUE
      IF(DABS(S1).LT.SS) S1=0.
      IF(DABS(S2).LT.SS) S2=0.
      TDE(I,J,K,L)=S1*PI
      TDE2(I,J,K,L)=S2*PI
    7 CONTINUE
      DO 8 I=1,3
      DO 8 J=1,3
      DO 8 K=1,3
      DO 8 L=1,3
      DO 8 M=1,N1
      S1=0.
      S2=0.
      DO 81 N=1,96
      SA=TP(K,N)*TP(L,N)*TP(M,N)
      SB=TM(K,N)*TM(L,N)*TM(M,N)
      S1=S1+W(N)*(TP(I,N)*TP(J,N)*SA+TM(I,N)*TM(J,N)*SB)
      S2=S2+W(N)*(TPP(I,N)*TPP(J,N)*SA+TPM(I,N)*TPM(J,N)*SB)
   81 CONTINUE
      IF(DABS(S1).LT.SS) S1=0.
      IF(DABS(S2).LT.SS) S2=0.
      THH(I,J,K,L,M)=S1*PI
      THH2(I,J,K,L,M)=S2*PI
    8 CONTINUE
      DO 9 I=1,N3
      DO 9 J=1,N3
      DO 9 K=1,N3
      DO 9 L=1,N3
      S2=0.
      DO 91 M=1,96
      SA=TPP(I,M)*TPP(J,M)
      SB=TPM(I,M)*TPM(J,M)
      S2=S2+W(M)*(SA*TPP(K,M)*TPP(L,M)+SB*TPM(K,M)*TPM(L,M))
   91 CONTINUE
      IF(DABS(S2).LT.SS) S2=0.
    9 TDE44(I,J,K,L)=S2
      RETURN
      END

C     DIFFERENTIAL R**3 FROM GARMONIC POLINOMS
      DOUBLE PRECISION FUNCTION WRRR(M,K,R,Z)
      DOUBLE PRECISION R,Z,V,X,WF,WR,WRR
      IF(K-M) 1,2,2
    1 WRRR=0.D0
      RETURN
    2 WRRR=(2.*K*WF(M,K,R,Z)-2.*(K-M)*Z*
     1WF(M,K-1,R,Z))/R**3-2.*(K*WR(M,K,R,Z)
     2-(K-M)*Z*WR(M,K-1,R,Z))/R**2+
     3(K*WRR(M,K,R,Z)-(K-M)*Z*WRR(M,K-1,R,Z))/R
      RETURN
      END

      DOUBLE PRECISION FUNCTION WR(M,K,R,Z)
      DOUBLE PRECISION R,Z,V,W,X,Y,WF
      IF(K-M) 1,2,2
    1 WR=0.D0
      RETURN
    2 W=K*WF(M,K,R,Z)-(K-M)*Z*WF(M,K-1,R,Z)
      WR=W/R
      RETURN
      END

      FUNCTION DEL(I,K)
      IF(I.EQ.K) GO TO 2
      DEL=0.0
      GO TO 3
    2 DEL=1.0
    3 RETURN
       END

	SUBROUTINE VENERGY(TT,RR,N9,N1)
      DIMENSION RR(N9)
      COMMON/BC/ GAC(6,6,10),DEC(3,3,3,10),HHC(3,3,3,3,10),
     1VV1(10,10),VV2(10,10,10),VV3(10,10,10,10),UU1(3,10),UU2(3,10,10),
     2UU3(3,10,10,10),UU4(3,10,10,10,10),WW1(10),WW2(10),WW3(6,6,10),
     3WW4(3,3,3,10),VZ2(6,6,10),VZ3(3,3,3,10),UZ2(3,10,10),
     4UZ3(3,6,6,10),UZ4(3,3,3,3,10)
      COMMON/RAZM/ N7,N2,N3,N4
      COMMON/FIZP/ RMS,ROG,G,SR,G0,RKG
      COMMON/MUP/ MU(10)
      EE=0.
	S=0.
	SA=0.
      DO 217 I=1,N1
	NI=N4+I
      DO 217 J=1,N1
      NJ=N4+J
      SA=RR(NI)*RR(NJ)
      IF(SA.EQ.0.) GO TO 2118
      S=S+SA*VV1(I,J)
 2118 CONTINUE
  217 CONTINUE
      EE=S
	S=0.
      DO 219 I=1,N2
	NI=N4+I
      DO 219 J=1,N2
      NJ=N4+J
      DO 219 K=1,N2
      SA=RR(NI)*RR(NJ)*RR(K)
      IF(SA.EQ.0.) GO TO 2120
      S=S+SA*VV2(I,J,K)
 2120 CONTINUE
  219 CONTINUE
      EE=EE+S
	S=0.
      DO 7219 I=1,N3
	NI=N4+I
      DO 7219 J=1,N3
      NJ=N4+J
      DO 7219 K=1,N3
	DO 7219 L=1,N3
      SA=RR(NI)*RR(NJ)*RR(K)*RR(L)
      IF(SA.EQ.0.) GO TO 7120
      S=S+SA*VV3(I,J,K,L)
 7120 CONTINUE
 7219 CONTINUE
      EE=EE+S
      EE=0.5*ROG*EE
	S=0.
!	DO 227 I=1,N1
!  227 S=S+0.5*ROG*RR(I)*WW1(I)*G  
      DO 228 I=1,N1
!      DO 228 J=1,N1
      J=I
      SA=RR(I)*RR(J)
      IF(SA.EQ.0.) GO TO 2131
      S=S+SA*WW2(I)*0.5*ROG*G
 2131 CONTINUE
  228 CONTINUE
      EE=EE+S
	S=0.
      PI=3.141592653589793D0
      DO 229 I=1,N2
      DO 229 J=1,N2
      DO 229 K=1,N2
      SA=RR(I)*RR(J)*RR(K)
      IF(SA.EQ.0.) GO TO 2132
      S=S+SA*WW3(I,J,K)*0.5*ROG*G
 2132 CONTINUE
  229 CONTINUE
      EE=EE+S
	S=0.
      DO 7229 I=1,N3
      DO 7229 J=1,N3
      DO 7229 K=1,N3
	DO 7229 L=1,N3
      SA=RR(I)*RR(J)*RR(K)*RR(L)
      IF(SA.EQ.0.) GO TO 7132
      S=S+SA*WW4(I,J,K,L)*0.5*ROG*G
 7132 CONTINUE
 7229 CONTINUE
      EE=EE+S
	RES=EE
!		WRITE (333,265),RES
!      IF(TT.EQ.0.) PRINT 265,RES
!  265 FORMAT(3X,10F15.8)
      RETURN
      END