
        .MODEL  TINY
        .CODE
        ORG     100H  
START:
		JMP 	MAIN
         
RESIDENT_DATA: 
                           
FILENAME          DB "PRTSCR.TXT",0

FILE_ERROR_STRING DB "File error", 0Dh, 0Ah, '$' 

FILEBUF           DB 80*25    DUP(?)     	
	
PROC PRTSCR FAR
        CLI 
        PUSHA
        PUSH    DS
        PUSH    ES
        MOV     AX, CS
        MOV     DS, AX
        MOV     ES, AX     
        MOV     AH, 3CH
        LEA     DX, FILENAME
        XOR     CX, CX
        INT     21H
        JC      FILE_ERROR
        PUSH    AX
        PUSH    DS
        MOV     AX, 0B800H
        MOV     DS, AX
        MOV     CX, 25
        LEA     DI, FILEBUF
        XOR     SI, SI
LOOP1:    
        PUSH    CX
        MOV     CX, 80
LOOP2:    
        MOVSB
        INC     SI
        LOOP    LOOP2
        POP     CX
        LOOP    LOOP1
        POP     DS
        MOV     AH, 40H
        POP     BX
        MOV     CX, 80*25
        LEA     DX, FILEBUF
        INT     21H
        JC      FILE_ERROR
        MOV     AH, 3EH
        INT     21H
        JC      FILE_ERROR
        JMP 	PRTSCR_END
FILE_ERROR:
        MOV     AH, 09H
        LEA     DX, FILE_ERROR_STRING
        INT     21H 
PRTSCR_END:
        POP 	ES
        POP 	DS
        POPA
        STI
        DB      0EAH
        OLD_HANDLER       DD 0
        IRET
     
ENDP PRTSCR       

MAIN:   

        MOV     AH, 35H
        MOV     AL, 05H
        INT     21H   
        
        MOV     WORD PTR OLD_HANDLER,   BX  
        MOV     WORD PTR OLD_HANDLER+2, ES
        
        MOV     AH, 25H
        MOV     AL, 05H
        LEA     DX, PRTSCR
        INT     21H
        
        MOV 	AH, 31H
        MOV 	DX, (MAIN-START+10FH)/16
        INT     21H
END MAIN     