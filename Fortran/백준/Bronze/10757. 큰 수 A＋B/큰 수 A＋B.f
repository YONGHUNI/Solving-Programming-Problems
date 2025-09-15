PROGRAM LGEADD
    
    IMPLICIT NONE
    CHARACTER(20099)::S
    CHARACTER(10099)::A,RA,B,RB,RESR,RES
    INTEGER::I
    INTEGER::LH,LL,AL,BL,FLG
    INTEGER::TA,TB,TR,UP

    READ'(A)',S
    
    CALL SPLIT(S,A,B)
    
    
    AL=LEN_TRIM(A)
    BL=LEN_TRIM(B)
    
    IF(AL>=BL)THEN
        
        LH=AL
        LL=BL
        FLG=0
        
    ELSE
        
        LH=BL
        LL=AL
        FLG=1
        
    END IF
    
    
    CALL REV(A,RA)
    CALL REV(B,RB)
    
    UP=0
    DO I=1,LH
        
        
        IF(I<=LL) THEN
            
            READ(RA(I:I),*) TA
            READ(RB(I:I),*) TB
            
            TR=TA+TB+UP
            
            UP=0
            
            IF(TR>9) THEN
                
                WRITE(RESR(I:I),'(I1)') TR-10
                UP=1
                
            ELSE
                
                WRITE(RESR(I:I),'(I1)') TR
                
            END IF
            
        ELSE
            
            IF(FLG==0) THEN
                
                READ(RA(I:I),*) TA
                TR=TA+UP
                UP=0
                
                IF(TR>9) THEN
                    
                    WRITE(RESR(I:I),'(I1)') TR-10
                    UP=1
                    
                ELSE
                    
                    WRITE(RESR(I:I),'(I1)') TR
                    
                END IF
                
            ELSE IF(FLG==1) THEN
                
                READ(RB(I:I),*) TB
                TR=TB+UP
                UP=0
                
                IF(TR>9) THEN
                    
                    WRITE(RESR(I:I),'(I1)') TR-10
                    UP=1
                    
                ELSE
                    
                    WRITE(RESR(I:I),'(I1)') TR
                    
                END IF
                
            END IF
            
        END IF
        
    ENDDO
    

    IF(UP==1)THEN
        RESR(LH+1:LH+1)="1"
        CALL REV(RESR,RES)
        
        PRINT'(A)',RES(10099-LH:10099)
        
    ELSE
        
        CALL REV(RESR,RES)
        
        PRINT'(A)',RES(10099-LH+1:10099)
    
    END IF
    




CONTAINS

    SUBROUTINE SPLIT(S,A,B)
        
        IMPLICIT NONE
        CHARACTER(20099), INTENT(IN)::S
        CHARACTER(10099), INTENT(OUT)::A,B
        INTEGER::I
        CHARACTER(1)::D=' '
        
        
        I = SCAN(S,D)
        
        A=S(1:I-1)
        B=S(I+1:LEN_TRIM(S))
        
        
    END SUBROUTINE SPLIT
    
    
    SUBROUTINE REV(A,B)
        
        IMPLICIT NONE
        CHARACTER(10099), INTENT(INOUT)::A,B
        CHARACTER(1)::TEMP
        INTEGER::I,L
        
        L=LEN_TRIM(A)
        B=A
        
        DO I=0,L/2-1
            
            TEMP=B(I+1:I+1)
            B(I+1:I+1)=B(L-I:L-I)
            B(L-I:L-I)=TEMP
            
        ENDDO
        
    END SUBROUTINE REV



END PROGRAM LGEADD