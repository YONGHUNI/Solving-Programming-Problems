IMPLICIT NONE
INTEGER*8,ALLOCATABLE::A(:,:),RES(:,:)
INTEGER*8::I,J,K,L,M


READ*,I,J

K = I

ALLOCATE(A(I,I),RES(I,I))

DO L=1,I
    READ*,A(L,:)
ENDDO



CALL POWER(A,I,J,RES)

DO I=1,K

    M=0

    DO L=1,(K+(K-1))

        IF(MOD(L,2)==0)THEN
    
            WRITE(*,'(A)',ADVANCE='NO') ' '
            M=M+1
    
        ELSE
    
            WRITE(*,'(I0)',ADVANCE='NO') RES(I,L-M)
    
        ENDIF
    
    ENDDO

    PRINT*,''
    
ENDDO



CONTAINS

RECURSIVE SUBROUTINE POWER(A,I,B,RES)

IMPLICIT NONE
INTEGER*8, INTENT(INOUT) :: A(:,:),I,B,RES(:,:)
INTEGER*8::TMP(I,I),TMP2
   

IF(B==1)THEN

    RES = MOD(A,1000)

ELSE
    
    TMP2 = B/2
    
    CALL POWER(A,I,TMP2,TMP)
    
    IF(MOD(B,2)==0)THEN
    
        RES = MOD(MATMUL(TMP,TMP),1000)
    
    ELSE
    
        RES = MOD(MATMUL(MOD(MATMUL(TMP,TMP),1000),A),1000)
    
    END IF

END IF


END SUBROUTINE POWER
END