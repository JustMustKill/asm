.model small

.stack 100h 

.data
    fileName db 30 dup(0) 
    outFileName db 'outfile.txt',0h 
    fileOpened db 'file opened', 0Dh, 0Ah, '$'
    fileid dw 0
    outFileid dw 0 
    n db 0
    buffer db 100 dup(0) 
    numOfCurrentLine db 1
    symbol db 0 
    argsSize db ?
    args db 120 dup('$') 
    number db 5
    emptyArgs db 'no cmd args', '$'
    error db 'error', '$'
    endedStr db 'ended', '$'
.code

outStr macro str
    mov ah, 09h
    mov dx, offset str
    int 21h    
endm    
  
readSymbol proc 
    pusha
    mov dx, offset symbol
    mov bx, fileid
    mov cx, 1
    mov ah, 3Fh
    int 21h
    cmp ax, 0
    je clearCall
    cmp symbol, 0Dh
    je lineEnded
    popa
    ret
clearCall:
    call clear 
lineEnded:
    inc numOfCurrentLine   
    popa  
    ret
readSymbol endp      

findlLineBeg proc
findLineBegLoop:   
    call readSymbol
    call writeInFile
    cmp symbol, 0Ah
    je findLineBegEnd
findLineBegEnd: 
    ret         
findlLineBeg endp 
    

skipLineProc proc
skipLineProcBegin:
    call readSymbol
    cmp symbol, 0Dh
    je skipLineProcLineEnded
    jmp skipLineProcBegin
skipLineProcLineEnded:
    call readSymbol     
    call writeInFile
skipWordProcEnded:      
    ret
skipLineProc endp

writeInFile proc
    pusha
    mov ah, 40h
    mov cx, 1
    mov bx, outFileid
    mov dx, offset symbol
    int 21h 
    popa 
    ret
writeInFile endp

processingArgs proc
    xor ax, ax
    xor bx, bx
    mov bl, 10
    xor cx, cx 
    mov si, offset args
processingArgsNum: 
    lodsb 
    cmp al, ' '
    je processingArgsNumEnd
    cmp al, '0'
    jb processingArgsError
    cmp al, '9'
    ja processingArgsNum
    sub al, '0'
    xchg ax, cx
    mul bl     
    add ax, cx
    xchg ax, cx
    jmp processingArgsNum
processingArgsNumEnd:
    mov n, cl
    mov di, offset filename
processingArgsFilename:    
    cmp [si], 0Dh
    je processingEnded
    movsb
    jmp processingArgsFilename    
processingArgsError:
    outStr error
    ret
processingEnded:
    ret               
processingArgs endp    

clear proc
clearM:    
    mov ah, 3Eh
    mov bx, fileid
    int 21h
    mov ah, 3Eh
    mov bx, outFileid
    int 21h
    mov ah, 41h
    mov dx, offset fileName
    int 21h
    mov ah, 56h
    mov dx, offset outFileName
    mov di, offset filename
    int 21h  
    jmp ended
clear endp    

start:
    mov ax, @data
    mov es, ax    
    xor cx, cx
	mov cl, ds:[80h]			
	mov argsSize, cl 		
	mov si, 82h
	mov di, offset args 
	rep movsb
	mov ds, ax
	call processingArgs    
    mov ax, 3D00h
    mov dx, offset fileName
    int 21h
    mov fileid, ax
    jnc opened 
    jmp ended
continue:      
    mov ah, 5Bh
    xor cx, cx
    mov cx, 7
    mov dx, offset outFileName
    int 21h
    mov outFileid, ax
    cmp n, 1
    je clearM
mainLoop:       
    call findlLineBeg
    mov bl, n
    cmp bl, numOfCurrentLine
    je skipLine
    jmp mainLoop    
skipLine:
    call skipLineProc
    mov numOfCurrentLine, 1
    jmp mainLoop
opened:
    outStr fileOpened
    jmp continue 
emptyArgsM:
    outStr emptyArgs
    jmp ended                              
ended:
    outStr endedStr
    mov ah, 4Ch
    int 21h
end start
