.model      small
.stack      100h

.data
    iterationsMsg     db "Iterations count: ", '$'
    fileMsg           db "Filename: ", '$'
    applicationError  db "Application start error!", '$'
    stringMsg         db "String number: ", '$'
    negativeExit      db "Enter correct number!", '$'
    allocatingError   db "Allocating error!", '$'
    startupError      db "Startup error!", '$'
    badFileMessage    db "Cannot open file", 0dh, 0ah, '$'
    badArguments      db "Bad cmd arguments", 0dh, 0ah, '$'
    fileError         db "Error while opening file!", '$'
    badFileName       db "Bad file name!", '$'
    
    argsSize db ?
    args db 120 dup('$')
    partSize          equ 256
    wasPreviousLetter dw 0
    realPartSize      dw 256
    descriptor        dw 0
    pointerPosition   dd 0
    path              db 256 dup('$')
    tempVariable      dw 0
    isEndl            db 0
    spacePos          dw 0
    base              dw 10
    iterations        db 0
    stringNumber      db 0
    parsingStep       dw 1
    endl              db 13, 10, '$'
    endlCounter       db 0
    
    fileName          db 256 dup(0)
    applicationName   db 256 dup(0)
    part              db partSize dup('$')
    
    env               dw 0
    dsize=$-startMessage       
.code

outStr macro str
    mov ah, 09h
    mov dx, offset str
    int 21h
    
    mov dl, 0Ah             
	mov ah, 02h           
	int 21h 
	
	mov dl, 0Dh             
	mov ah, 02h             
	int 21h     
endm 

printString proc  
    push    bp
    mov     bp, sp   
    pusha 
    
    mov     dx, [ss:bp+4+0]     
    mov     ax, 0900h
    int     21h 
    
    mov     dx, offset endl
    mov     ax, 0900h
    int     21h  
    
    popa
    pop     bp      
    ret 
endp

processingArgs proc
    xor ax, ax
    xor bx, bx
    mov bl, 10
    xor cx, cx 
    mov si, offset args
processingArgsIter: 
    lodsb 
    cmp al, ' '
    je processingArgsIterEnd
    cmp al, '0'
    jb processingArgsError
    cmp al, '9'
    ja processingArgsIter
    sub al, '0'
    xchg ax, cx
    mul bl     
    add ax, cx
    xchg ax, cx
    cmp cx, 00FFh
    ja badCMD
    jmp processingArgsIter
processingArgsIterEnd:
    mov iterations, cl 
    
    xor cx, cx
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
    cmp cx, 00FFh
    ja badCMD
    jmp processingArgsNum
processingArgsNumEnd:
    mov stringNumber, cl
    
    mov di, offset filename
processingArgsFilename:    
    cmp [si], 0Dh
    je processingEnded
    movsb
    jmp processingArgsFilename    
processingArgsError: 
    outStr badArguments
    call    exit
    ret
badCMD:
    outStr badArguments
    call    exit
    ret
processingEnded:
    ret               
processingArgs endp

badFileNameCall proc
    outStr badFileName
    call    exit
endp

exit proc
    mov     ax, 4c00h
    int     21h
endp

badRange:
    outStr negativeExit
    call    exit
ret


applicationStartError:
    outStr applicationError
    call    exit
ret


allocateMemory proc
    push    ax
    push    bx 

    mov     bx, ((csize/16)+1)+((dsize/16)+1)+32
    mov     ah, 4Ah
    int     21h 

    jc      allocateMemoryError
    jmp     allocateMemoryEnd 

    allocateMemoryError:
        outStr allocatingError
        call    exit  
      
    allocateMemoryEnd:
        pop     bx
        pop     ax
        ret
endp

loadAndRun proc
    mov     ax, 4B00h
    lea     dx, applicationName
    lea     bx, env
    int     21h
    jc applicationStartError 
    
    ret
endp


fileErrorCall:
    outStr fileError
    call    exit
ret


getApplicationName proc
    pusha
    xor     ax, ax
    
    mov     dx, offset fileName
    mov     ah, 3Dh
    mov     al, 00h
    int     21h
    mov     descriptor, ax

    mov     bx, ax
    jnc     readFilePart
    jmp     fileErrorCall;

    readFilePart:    
    
        mov     ah, 42h
        mov     cx, word ptr [offset pointerPosition]
        mov     dx, word ptr [offset pointerPosition + 2]
        mov     al, 0  
        mov     bx, descriptor
        int     21h

        mov     cx, partSize
        lea     dx, part
        mov     ah, 3Fh
        mov     bx, descriptor
        int     21h
        mov     realPartSize, ax

        call    searchApplicationName
        call    memset

        cmp     realPartSize, partSize
        jb      closeFile

        mov     bl, stringNumber
        cmp     endlCounter, bl
        je      closeFile
        
        mov     cx, word ptr [offset pointerPosition]
        mov     dx, word ptr [offset pointerPosition + 2]
        add     dx, ax
        adc     cx, 0
        mov     word ptr [offset pointerPosition], cx
        mov     word ptr [offset pointerPosition + 2], dx

        jmp     readFilePart
             
    
    closeFile:
        exitFromFile:
            mov     ah, 3Eh
            mov     bx, descriptor
            int     21h
            popa
    
    ret
endp

searchApplicationName proc
    pusha
    xor     si, si

    partParsing:
        call    checkEndl 

        mov     al, stringNumber
        cmp     endlCounter, al
        je      parseApplicationName

        cmp     isEndl, 0
        je      increment

        inc     endlCounter
        jmp     partParsingCycle

        increment:
            inc     si
        
        partParsingCycle:
            mov     isEndl, 0
            cmp     si, realPartSize
            jb      partParsing

    
    popa
    ret

    parseApplicationName:
        cmp     isEndl, 1
        jne     parseStart   
        
        call    badFileNameCall

        parseStart:
            lea     di, applicationName

            copyApplicationName:
                xor     ax, ax
                mov     al, [part + si]
                mov     [di], al

                inc     si
                inc     di

                cmp     [part + si], 0dh
                je      exitFromParsing

                cmp     [part + si], 0ah
                je      exitFromParsing

                cmp     si, realPartSize
                je      exitFromParsing

                jmp     copyApplicationName
        
    exitFromParsing:

    popa
    ret
endp

checkEndl proc
    mov     al, [part + si]
    xor     ah,ah

    cmp     al, 0dh
    je      checkNextSymbol

    cmp     al, 0ah
    jne     exitFromEndlCheck

    inc     si
    call    setIsEndl
    
    exitFromEndlCheck:
    ret
endp

checkNextSymbol:
    call    setIsEndl
    mov     bl, [part + si + 1]
    xor     bh,bh

    cmp     bl, 0ah
    jne     exitFromCheck

    inc     si

    exitFromCheck:
        inc     si
ret

setIsEndl proc
    mov     isEndl, 1
    ret
endp

memset proc
    pusha
    xor     si, si
    lea     si, part
    mov     cx, partSize

    setEndCycle:
        mov     byte ptr [si], '$'
        inc     si
        loop    setEndCycle
    
    popa
    ret
endp


badArgumentsCall:
    outStr badArguments
    call    exit
ret


start proc
    call    allocateMemory
    
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
    
    main:
        
        dec     stringNumber
        call    getApplicationName

        xor cx, cx
        mov cl, iterations
        
        startApps:
            call    loadAndRun
            loop    startApps

        call exit
endp

csize = $ - printString

end start