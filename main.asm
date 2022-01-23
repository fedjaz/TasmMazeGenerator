.model small
.stack 100h
.data
	graph db 1000 dup(0)
    used db 1000 dup(0)
    directions db 0, 1, 2, 3
    sizeX dw 20
    sizeY dw 20
    cellSize db 10
    lineWidth db 1
    playerPosX dw 0
    playerPosY dw 0
    playerSize dw 2
    playerOffset dw 3
    fileName db "D:\dist\data", 0
    char db 0
.code
.386

printNumberOld proc
	push bx
	push cx
	push dx
	
	mov bx, 10
	xor cx, cx
	xor dx, dx
	begin2qq:
		div bx
		push dx
		xor dx, dx
		inc cx
		cmp ax, 0
		jnz begin2qq
	
	begin3qq:
		pop ax
		mov dl, '0'
		add dl, al
		mov ah, 2
		int 21h
		loop begin3qq
		
	pop dx
	pop cx
	pop bx
	ret
printNumberOld endp

fillRectangle proc
    jmp fillRectangleVariables
        X1 dw 0
        X2 dw 0
        Y1 dw 0
        Y2 dw 0
        color db 0
    fillRectangleVariables:

    push ax
    push bx
    push cx
    push dx

    mov cx, X1
    mov dx, Y1
    mov ah, 0ch
    mov al, color
    mov bx, 0

    loop1_1:
        cmp dx, Y2
        jg loop1_1_end

        mov cx, x1
        loop2_1:
            cmp cx, X2
            jg loop2_1end

            int 10h

            inc cx
            jmp loop2_1

        loop2_1end:
        inc dx
        jmp loop1_1
    loop1_1_end:

    pop dx
    pop cx
    pop bx
    pop ax
    ret
fillRectangle endp

drawCell proc
    push ax
    push bx
    push cx
    push dx

    jmp drawCellVariables
        cornerY dw 0
        cornerX dw 0
    drawCellVariables:

    ;calculating coords
    mul cellSize
    push ax
    mov ax, bx
    mul cellSize
    mov bx, ax
    pop ax
    mov cornerY, ax
    mov cornerX, bx

    ;down border
    shr cx, 1
    rcr cx, 1
    jc rightborder

    ;Y1
    mov ax, cornerY
    xor bx, bx
    mov bl, cellSize
    add ax, bx
    mov bl, lineWidth
    sub ax, bx
    mov Y1, ax

    ;X1
    mov ax, cornerX
    mov X1, ax

    ;Y2
    mov ax, cornerY
    mov bl, cellSize
    add ax, bx
    mov Y2, ax 

    ;X2
    mov ax, cornerX
    mov bl, cellSize 
    add ax, bx
    mov X2, ax

    mov color, 0
    call fillRectangle


    rightborder:
    shr cx, 1
    rcr cx, 1
    jc endDrawCell

    ;Y1
    mov ax, cornerY
    mov Y1, ax

    ;X1
    mov ax, cornerX
    xor bx, bx
    mov bl, cellSize
    add ax, bx
    mov bl, lineWidth
    sub ax, bx
    mov X1, ax

    ;Y2
    mov ax, cornerY
    mov bl, cellSize
    add ax, bx
    mov Y2, ax

    ;X2
    mov ax, cornerX
    mov bl, cellSize
    add ax, bx
    mov X2, ax

    mov color, 0
    call fillRectangle

    endDrawCell:

    ;draw square
    mov ax, cornerY 
    mov bl, cellSize
    add ax, bx
    mov bl, lineWidth
    sub ax, bx
    mov Y1, ax
    mov Y2, ax
    inc Y2

    mov ax, cornerX
    mov bl, cellSize
    add ax, bx
    mov bl, lineWidth
    sub ax, bx
    mov X1, ax
    mov X2, ax
    inc X2

    call fillRectangle

    pop dx
    pop cx
    pop bx
    pop ax
    ret

drawCell endp

drawMaze proc
    push ax
    push bx

    ;drawing game background

    mov X1, 0
    mov Y1, 0
    mov X2, 320
    mov Y2, 200
    mov color, 6
    call fillRectangle
    ;drawing maze background

    mov X1, 0
    mov Y1, 0
    mov X2, 200
    mov Y2, 200
    mov color, 15
    call fillRectangle

    ;drawing start and finish
    mov X1, 0
    mov Y1, 0
    xor ax, ax
    mov al, cellSize
    mov Y2, ax
    mov X2, ax
    mov color, 2
    call fillRectangle

    mov ax, sizeX
    sub ax, 1
    mul cellSize
    mov X1, ax
    mov ax, sizeY
    sub ax, 1
    mul cellSize
    mov Y1, ax

    
    xor bx, bx
    mov bl, cellSize
    mov ax, X1
    add ax, bx
    mov X2, ax
    mov ax, Y1
    add ax, bx
    mov Y2, ax
    mov color, 4
    call fillRectangle

    ;drawing borders
    mov color, 0

    mov X1, 0
    mov Y1, 0

    mov X2, 200
    xor ax, ax
    mov al, lineWidth
    mov Y2, ax
    call fillRectangle

    mov X2, ax
    mov Y2, 200
    call fillRectangle

    

    ;drawing cells
    lea si, graph
    mov ax, 0
    drawMazeloop1:
        cmp ax, sizeY
        jge drawMazeloop1end

        mov bx, 0
        drawMazeloop2:
        cmp bx, sizeX
        jge drawMazeloop2end
        call getElem

        call drawCell

        inc bx
        jmp drawMazeloop2
        drawMazeloop2end:
        inc ax
        jmp drawMazeloop1
    drawMazeloop1end:
    pop bx
    pop ax
    ret
drawMaze endp

check proc
    push ax
    push bx
    push cx
    push dx
    push si

    jmp variables1
        y dw 0
        x dw 0
    variables1:

    mov y, ax
    mov x, bx
    mov dx, 0
    ;checking up
    up:
        mov ax, y
        mov bx, x

        cmp ax, 0
        je down

        sub ax, 1

        lea si, used
        call getElem
        cmp cx, 0
        
        jne down
        mov dx, 1

    ;checking down
    down:
        mov ax, y
        mov bx, x
        
        add ax, 1
        cmp ax, sizeY
        je left

        lea si, used
        call getElem
        cmp cx, 0
        jne left
        mov dx, 1

    ;checking left
    left:
        mov ax, y
        mov bx, x
        
        cmp bx, 0
        je right

        sub bx, 1

        lea si, used
        call getElem
        cmp cx, 0
        jne right
        mov dx, 1

    ;checking right
    right:
        mov ax, y
        mov bx, x
        
        add bx, 1
        cmp bx, sizeX
        je exit

        lea si, used
        call getElem
        cmp cx, 0
        jne exit
        mov dx, 1

    exit:
        cmp dx, 1
        jne cantMove

    canMove:
        mov ah, 0
        jmp endcheck
    cantMove:
        mov ah, 01000000b
    
    endCheck:
        sahf
        pop si
        pop dx
        pop cx
        pop bx
        pop ax

    ret
check endp

getElem proc
    push ax
    push si
    mov cx, sizeY
    mul cl
    add ax, bx
    add si, ax
    lodsb
    xor cx, cx
    mov cl, al
    pop si
    pop ax

    ret
getElem endp

setElem proc
    push ax
    push dx
    push di
    mov dx, sizeY
    mul dl
    add ax, bx
    add di, ax
    mov al, cl
    stosb
    pop di
    pop dx
    pop ax

    ret
setElem endp

rand proc
    push bx
    push dx
    db 0fh, 31h

    mov dx, ax
    mov ax, 25173

    mul dx

    add ax, 13849
    xor ah, ah
    mov bl, 4
    div bl
    mov al, ah
    xor ah, ah

    pop dx
    pop bx

    ret
rand endp 

movePlayer proc
    push ax
    push bx
    push cx
    push dx


    jmp movePlayerVariables
        lastPosX dw 0
        lastPosY dw 0
        newPosX dw 0
        newPosY dw 0
    movePlayerVariables:

    mul cellSize
    mov newPosY, ax

    mov ax, bx
    mul cellSize
    mov newPosX, ax

    mov ax, cx
    mul cellSize
    mov lastPosY, ax

    mov ax, dx
    mul cellSize
    mov lastPosX, ax

    ;drawing white square
    mov ax, playerOffset
    add ax, lastPosY
    mov Y1, ax

    mov ax, playerOffset
    add ax, lastPosX
    mov X1, ax

    mov ax, Y1
    add ax, playerSize
    mov Y2, ax

    mov ax, X1
    add ax, playerSize
    mov X2, ax

    cmp lastPosY, 0
    jne white
    cmp lastPosX, 0
    jne white
    mov color, 2
    jmp draw

    white:
    mov color, 15

    draw:
    call fillRectangle

    ;drawing player
    mov ax, playerOffset
    add ax, newPosY
    mov Y1, ax

    mov ax, playerOffset
    add ax, newPosX
    mov X1, ax

    mov ax, Y1
    add ax, playerSize
    mov Y2, ax

    mov ax, X1
    add ax, playerSize
    mov X2, ax

    mov color, 1
    call fillRectangle

    pop dx
    pop cx
    pop bx
    pop ax
    ret
movePlayer endp

play proc far

    push ax
    push bx
    push cx
    push dx

    cli
    xor ax, ax
    in al, 60h
    push ax

    mov ax, playerPosY
    mov bx, playerPosX
    lea si, graph
    call getElem
    
    pop ax

    load:
    cmp al, 38
    jne save
    call read
    jmp playEnd

    save:
    cmp al, 1fh
    jne restart
    call write
    jmp playEnd

    restart:
    cmp al, 13h
    jne moveUp
    call newGame
    jmp playEnd
    
    moveUp:
    cmp al, 48h
    jne moveDown
    rcr cx, 1
    jnc playEnd

    mov ax, playerPosY
    mov bx, playerPosX

    dec ax

    jmp move

    moveDown:
    cmp al, 50h
    jne moveLeft

    rcr cx, 2
    jnc playEnd

    mov ax, playerPosY
    mov bx, playerPosX

    inc ax

    jmp move

    moveLeft:
    cmp al, 4bh
    jne moveRight

    rcr cx, 3
    jnc playEnd

    mov ax, playerPosY
    mov bx, playerPosX
    dec bx

    jmp move

    moveRight:
    cmp al, 4dh
    jne playEnd

    rcr cx, 4
    jnc playEnd

    mov ax, playerPosY
    mov bx, playerPosX
    inc bx

    move:
    mov cx, playerPosY
    mov dx, playerPosX

    call movePlayer

    mov playerPosY, ax
    mov playerPosX, bx

    playEnd:
    mov al, 20h 
    out 20h,  al

    ;checking new cords
    mov ax, playerPosY
    mov bx, playerPosX
    inc ax
    inc bx
    cmp ax, sizeY
    jne trueEnd
    cmp bx, sizeX
    jne trueEnd
    call newGame

    trueEnd:
    sti

    pop dx
    pop cx
    pop bx
    pop ax
    iret
play endp

dfs proc
    jmp variables
        posY dw 0
        posX dw 0
    variables:   

    mov posY, ax
    mov posX, bx

    ;mixing
    mov cx, 16

    mixloop:
    push cx
    call rand
    push ax
    call rand
    mov bx, ax
    mov ax, 0
    lea si, directions
    call getElem
    mov dx, cx
    mov ax, bx
    pop bx
    push ax
    mov ax, 0
    call getElem
    mov ax, dx
    mov dx, cx
    mov cx, ax
    mov ax, 0
    lea di, directions
    call setElem
    mov cx, dx
    pop bx
    call setElem

    pop cx
    loop mixloop

    mov ax, 0
    mov bx, 0
    lea si, directions
    pushingLoop:
    cmp bx, 4
    jge pushingLoopEnd
    call getElem
    push cx

    inc bx
    jmp pushingLoop
    pushingLoopEnd:


    mov ax, posY
    mov bx, posX
    lea di, used
    mov cx, 1
    call setElem

    mov cx, 4
    bigloop:
        mov ax, posY
        mov bx, posX

        ;call check
        ;jz endbigloop

        pop dx
        push cx

        upDir:
            ;checking is up
            cmp dx, 0
            jne downDir
            
            ;checking if can move up
            cmp ax, 0
            je next

            ;checking if up direction is not used
            sub ax, 1
            lea si, used
            call getElem
            cmp cx, 1
            je next

            ;adding info that can move up from this cell
            lea si, graph
            add ax, 1
            call getElem
            add cx, 0001b
            lea di, graph
            call setElem

            ;adding info that can move down from upper cell
            sub ax, 1
            lea di, graph
            mov cx, 0010b
            call setElem

            ;calling recursivly dfs into 
            call dfs
            add ax, 1
            mov posY, ax

            ;getting out of loop
            jmp next

        downDir:
            ;checking is down
            cmp dx, 1
            jne leftDir

            ;checking if can move down
            add ax, 1
            cmp ax, sizeY
            je next
            
            ;checking if down direction is not used
            lea si, used
            call getElem
            cmp cx, 1
            je next

            ;adding info that can move down from this cell
            lea si, graph
            sub ax, 1
            call getElem
            add cx, 0010b
            lea di, graph
            call setElem

            ;adding info that can move up from down cell
            add ax, 1
            lea di, graph
            mov cx, 0001b
            call setElem

            ;calling recursivly dfs into 
            call dfs
            sub ax, 1
            mov posY, ax

            ;getting out of loop
            jmp next


        leftDir:
            ;checking is left
            cmp dx, 2
            jne rightDir

            ;checking if can move left
            cmp bx, 0
            je next

            ;checking if left direction is not used
            sub bx, 1
            lea si, used
            call getElem
            cmp cx, 1
            je next

            ;adding info that can move left from this cell
            lea si, graph
            add bx, 1
            call getElem
            add cx, 0100b
            lea di, graph
            call setElem

            ;adding info that can move right from left cell
            sub bx, 1
            lea di, graph
            mov cx, 1000b
            call setElem

            ;calling recursivly dfs into 
            call dfs
            add bx, 1
            mov posX, bx

            ;getting out of loop
            jmp next

        rightDir:
            ;checking if can move right
            add bx, 1
            cmp bx, sizeX
            je next

            ;checking if right direction is not used
            lea si, used
            call getElem
            cmp cx, 1
            je next

            ;adding info that can move right from this cell
            lea si, graph
            sub bx, 1
            call getElem
            add cx, 1000b
            lea di, graph
            call setElem

            ;adding info that can move left from right cell
            add bx, 1
            lea di, graph
            mov cx, 0100b
            call setElem

            ;calling recursivly dfs into 
            call dfs
            sub bx, 1
            mov posX, bx

            jmp next

        next:
            pop cx
            dec cx
            cmp cx, 0

            je endbigloop
            jmp bigloop

    endbigloop:
    mov ax, posY
    mov bx, posX
    ret
dfs endp

newGame proc
    mov ax, 0
    mov bx, 0
    mov cx, 0
    newGameLoop1:
        cmp ax, sizeY
        jge newGameLoop1End

        mov bx, 0
        newGameLoop2:
            cmp bx, sizeX
            jge newGameLoop2End

            lea di, graph
            call setElem

            lea di, used
            call setElem

            inc bx
            jmp newGameLoop2
        newGameLoop2End:

        inc ax
        jmp newGameLoop1
    newGameLoop1End:
    mov playerPosX, 0
    mov playerPosY, 0
    mov ax, 0
    mov bx, 0
    call dfs
    call drawMaze
    mov ax, 0
    mov bx, 0
    mov cx, 0
    mov dx, 0
    call movePlayer
    ret
newGame endp

printNumber proc
	push bx
	push cx
	push dx
	

	xor cx, cx
	xor dx, dx
	begin2:
        push bx
        mov bx, 10
		div bx
        pop bx
		push dx
		xor dx, dx
		inc cx
		cmp ax, 0
		jnz begin2
	
	begin3:
		pop ax
		push cx
        add al, '0'
        mov char, al

        mov ah, 40h
        mov cx, 1
        mov dx, offset char
		int 21h

        pop cx
		loop begin3
		
	pop dx
	pop cx
	pop bx
	ret
printNumber endp

printWhiteSpace proc
    push ax
    push cx
    push dx

    mov ah, 40h
    mov char, ' '
    mov cx, 1
    mov dx, offset char
    int 21h

    pop dx
    pop cx
    pop ax
    ret
printWhiteSpace endp

printEnter proc
    push ax
    push cx
    push dx

    mov ah, 40h
    mov char, 13
    mov cx, 1
    mov dx, offset char
    int 21h

    pop dx
    pop cx
    pop ax
    ret
printEnter endp

write proc
    push ax
    push bx
    push cx
    push dx

    ;creating file
    mov ah, 5bh
    mov dx, offset fileName
    mov cx, 0
    int 21h

    ;opening file
    mov ah, 3ch
    mov al, 1
    mov dx, offset fileName
    int 21h
    jc endWrite
    mov dx, ax

    mov ax, playerPosY
    mov bx, dx
    call printNumber

    call printWhiteSpace

    mov ax, playerPosX
    call printNumber

    call printEnter
    
    mov ax, 0
    mov bx, 0
    writeLoop1:
        cmp ax, sizeY
        jge writeLoop1End

        mov bx, 0
        writeLoop2:
            cmp bx, sizeX
            jge writeLoop2End

            lea si, graph
            call getElem

            push ax
            push bx

            mov ax, cx
            mov bx, dx
            call printNumber
            
            call printWhiteSpace
            mov dx, bx

            pop bx
            pop ax

            inc bx
            jmp writeLoop2
        writeLoop2End:

        push ax
        push bx

        mov bx, dx
        call printEnter

        pop bx
        pop ax 

        inc ax
        jmp writeLoop1
    writeLoop1End:


    endWrite:
    mov bx, dx
    mov ah, 3eh
    int 21h

    pop dx
    pop cx
    pop bx
    pop ax
    ret

write endp

readNumber proc
    push bx
    push cx
    push dx

    jmp readNumberVariables
        isStarted db 0
    readNumberVariables:
    mov isStarted, 0
    mov ax, 0
    readNumberLoop:
        push ax
        mov ah, 3fh
        mov dx, offset char
        mov cx, 1
        int 21h
        cmp ax, 0
        je readNumberEnd

        mov cl, char
        pop ax

        cmp cl, '0'
        jl notNumber
        cmp cl, '9'
        jg notNumber
        jmp isNumber
        
        notNumber:
        cmp isStarted, 1
        je readNumberEnd
        jmp readNumberLoop

        isNumber:
        mov isStarted, 1
        sub cl, '0'
        mov dx, ax
        mov ax, 10
        mul dx
        add ax, cx

        jmp readNumberLoop

    readNumberEnd:
    pop bx
    pop cx
    pop dx

    ret
readNumber endp

read proc
    push ax
    push bx
    push cx
    push dx

    mov ah, 3dh
    mov al, 0
    mov dx, offset fileName
    int 21h    

    jc readEnd


    mov dx, ax
    mov bx, ax

    call readNumber
    mov playerPosY, ax

    call readNumber
    mov playerPosX, ax

    mov ax, 0
    mov bx, 0
    readLoop1:
        cmp ax, sizeY
        jge readLoop1End

        mov bx, 0
        readLoop2:
        cmp bx, sizeX
        jge readLoop2End

        push ax
        push bx

        mov bx, dx
        call readNumber

        mov cx, ax
        mov dx, bx

        pop bx
        pop ax
        lea di, graph

        call setElem

        inc bx
        jmp readLoop2
        readLoop2End:

        inc ax
        jmp readLoop1
    readLoop1End:

    call drawMaze

    mov ax, playerPosY
    mov bx, playerPosX
    mov cx, playerPosY
    mov dx, playerPosX
    call movePlayer

    readEnd:
    pop dx
    pop cx
    pop bx
    pop ax

    ret
read endp

main:
    mov ax, @data
	mov ds, ax
	mov es, ax

    mov ah, 7
    int 21h

    mov ah, 0Fh
    int 10h
    mov ax, 0013h
    int 10h

    call newGame

    cli
    push ds
    mov ax, 2509h
    mov dx, @code
    mov ds, dx
    mov dx, offset play
    int 21h
    pop ds
    sti

    infiniteLoop:
    sti
    jmp infiniteLoop

    mov ax, 4c00h
	int 21h
end main