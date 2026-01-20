[org 0x0100]
jmp start

; --- DATA SECTION: Memory variables for game state and logic ---
start_game: db 0             
oldtmr: dd 0
end_game: db 0     
oldisr: dd 0
curr_col: dw 0
curr_row: dw 0
incC: db 0           ; Increment Column (Ball movement direction)
incR: db 0           ; Increment Row (Ball movement direction)
previous: dw 0                  
tickcount: db 0                  
left_edge: dw 3524   ; Paddle movement boundary (Screen floor left)
right_edge: dw 3660  ; Paddle movement boundary (Screen floor right)
right_: dw 0         ; Input flag: Right arrow pressed
left_: dw 0          ; Input flag: Left arrow pressed
pre_stack_pos: dw 3580 ; Previous paddle memory offset

score: dw 0
total_b: dw 24       ; Counter for total bricks remaining
cal_loc:  dw 0       ; Calculated memory offset storage
inst_men_status: db 0      
left_limit: dw 0     ; Current paddle left edge offset 
right_limit: dw 0    ; Current paddle right edge offset
mid: dw 0            ; Midpoint of the paddle
left_or_right: db 0
preBall:dw 0         ; Previous ball location (for clearing)

live: db 3           ; Total lives counter
end_of_game: dw 0
onPaddle: db 0       ; Boolean: Is the ball currently stuck to the paddle?
restart: db 0                
quit: db 0
variable: dw 0

; --- BRICK POSITION ARRAYS ---
b_start_loc: dw 810 , 828 , 846 , 864 , 882 , 900 , 918 , 936 , 1130 , 1148 , 1166 , 1184 , 1202, 1220, 1238 , 1256 , 1450 , 1468 , 1486 , 1504 , 1522 , 1540 , 1558 , 1576
b_end_loc: dw   826 , 844 , 862 , 880 , 898 , 916 , 934 , 952 , 1146 , 1164 , 1182 , 1200 , 1218, 1236, 1254 , 1272 , 1466 , 1484 , 1502 , 1520 , 1538 , 1556 , 1574 , 1592

; --- STRING DEFINITIONS: For UI/Text output ---
game_ov: db '!!!GAME OVER!!!'
instruc: db '--> PRESS (I) FOR INSTRUCTIONS'      
Score_str: db 'SCORE: '
Lives_str: db 'LIVES: '                                                                                
welcome_str: db '<===( Atari Breakout Arcade Game )===>'
option_str: db 'PLEASE SELECT OPTIONS'                                
instructions_str: db 'INSTRUCTION'
play_str: db '--> PRESS ENTER TO PLAY'
ttl_live_str: db '-->  YOUR TOTAL LIVES ARE THREE '
space_bar: db '-->  PRESS SPACE BAR TO RELEASE BALL'
total_score_str: db 'YOUR TOTAL SCORES :'
lives_remain_str: db 'LIVES REMAINING'
quit_str: db 'PRESS (Q) FOR MAIN MENU'      
restart_str: db 'PRESS (R) TO RESTART GAME' 
left_arrow: db '-->  USE RIGHT & LEFT ARROW TO MOVE BAR'

; Generates a beep sound using PC speaker via ports 43h, 42h, and 61h
sound:                         
        push ax
        push bx
        mov al,182
        out 43h,al
        mov ax, 4560          ; Frequency value
        out 42h,al
        mov al,ah
        out 42h,al
        in al,61h
        or al,00000011        ; Speaker ON
        out 61h,al
        mov bx,2
    one:
        mov cx,65535
    two:                      ; Simple delay loop
        dec cx
        jne two
        dec bx
        jne one
        in al,61h
        and AL,11111100b      ; Speaker OFF
        out 61h,al 
        pop bx
        pop ax
        ret 

; Clears screen and draws the initial UI welcome screen
start_menu:                                     
    push ax
    call clrscr
    call borders              ; Draw surrounding box
    
    mov ax , 364              ; Welcome position
    push ax
    mov ax ,welcome_str
    push ax
    mov ax , 38               ; String length
    push ax
    call printstr
    
    mov ax , 1170             ; Play text position
    push ax
    mov ax , play_str
    push ax
    mov ax , 23
    push ax
    call printstr
    
    mov ax , 1490             ; Instruction text position
    push ax
    mov ax , instruc
    push ax
    mov ax , 30
    push ax
    call printstr
    
    pop ax
    ret

; Converts a word value to decimal and prints to video memory
printnum:                     
    push bp
    mov bp, sp
    push es
    push ax
    push bx
    push cx
    push dx
    push di
    mov ax, 0xb800
    mov es, ax 
    mov ax, [bp+4]            ; Number to print
    mov bx, 10 
    mov cx, 0 
    nextdigit: mov dx, 0 
    div bx                    ; ax / 10
    add dl, 0x30              ; Convert to ASCII
    push dx 
    inc cx 
    cmp ax, 0 
    jnz nextdigit 
    mov di, [bp+6]            ; Memory offset
    nextpos: pop dx 
    mov dh, 0x07              ; Normal attribute
    mov [es:di], dx 
    add di, 2 
    loop nextpos
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    pop es
    pop bp
ret 4

; Standard string printing routine for 0xb800 segment
printstr:                               
    push bp
    mov bp, sp
    push es
    push ax
    push cx
    push si
    push di
    mov ax, 0xb800
    mov es, ax 
    mov di, [bp+8]            ; Screen offset
    mov si, [bp+6]            ; String address
    mov cx, [bp+4]            ; Length
    mov ah, 0x0F              ; Bright white attribute
    nextchar: 
    mov al, [si] 
    mov [es:di], ax 
    add di, 2 
    add si, 1 
    loop nextchar 
    pop di
    pop si
    pop cx
    pop ax
    pop es
    pop bp
    ret 6

; Prints static labels 'LIVES' and 'SCORE' at the top of the game screen
printTop:                                  
    push ax
    mov ax , 440
    push ax
    mov ax , Lives_str
    push ax
    mov ax , 7
    push ax
    call printstr
    
    mov ax , 324
    push ax
    mov ax , Score_str
    push ax
    mov ax , 7
    push ax
    call printstr
    
    pop ax
ret

; Clears the entire text mode screen using whitespaces
clrscr:                                  
    push es
    push ax
    push cx
    push di
    mov ax, 0xb800
    mov es, ax 
    xor di, di 
    mov ax, 0x0720            ; Space char with normal color
    mov cx, 2000              ; 80x25 screen
    cld 
    rep stosw 
    pop di 
    pop cx
    pop ax
    pop es
ret 

; Draws 5 distinct line segments forming the game environment box
borders:                             
    push ax
    push es
    push di
    mov ax,0xb800
    mov es,ax
    mov ah,0xB0               ; Cyan background attribute
    mov al,0x20
    mov di,482
    b1:                       ; Top horizontal line 1
         mov word[es:di],ax
         add di,2
         cmp di,636
         jne b1
    mov di,3682
    b2:                       ; Bottom horizontal line
            mov word[es:di],ax
            add di,2
            cmp di,3836
            jne b2
    mov di,2
    b3:                       ; Left vertical line
        mov word[es:di],ax
        add di,160
        cmp di,3842
        jne b3
    mov di,156
    b4:                       ; Right vertical line
        mov word[es:di],ax
        add di,160
        cmp di,3996
        jne b4
    mov di,2
    b5:                       ; Top absolute horizontal line
        mov word[es:di],ax
        add di,2
        cmp di,156
        jne b5
pop di
pop es
pop ax
ret

; Logic to detect collision index and overwrite brick space with empty space
brick_remove:                       
    push es
    push ax
    push dx
    push cx
    push si
    push bx
    push di
    mov ax,0xb800
    mov es,ax
    mov cx , 24               ; Loop count (Total bricks)
    mov si , 0      
    mov dx , [cs:cal_loc]     ; Ball current location index
check:                       
    mov ax , word[cs:b_start_loc + si] ; Get start offset of brick SI
    mov bx , word[cs:b_end_loc + si]   ; Get end offset of brick SI
    cmp dx , ax
    jb next_brick
    cmp dx , bx
    ja next_brick
    
    ; Match found: overwrite brick
    mov di , ax
    mov cx , 8
    mov ax , 0x0720
    cld
    rep stosw                 ; Draw space where brick was
    call sound
    add word[cs:score] , 10   ; Increment score
    dec word[cs:total_b]
    mov ax , 334
    push ax              
    push word[cs:score]
    call printnum             ; Refresh score on screen
    jmp end_fun
next_brick:
    add si , 2
    loop check
end_fun:
    pop di
    pop bx
    pop si
    pop cx
    pop dx
    pop ax
    pop es
ret

; Renders all 24 bricks in 3 distinct colorful rows
bricks:                    
    push es
    push cx
    push bx
    push si
    push di
    mov ax, 0xb800
    mov es, ax  
    mov di, 810               ; Starting row position
    mov si , 0
    mov bx , 0
    cld
    brickline1:               ; Draw Green Blue bricks
        cmp di , 936
        ja brickline2
            mov ah , 0x90
            mov al , 0x20
            mov cx , 8
            rep stosw
            mov cx , 1
            mov ax, 0x0720    ; Gap between bricks
            rep stosw
            add si , 2  
            jmp brickline1
    brickline2:
        mov di , 1130
    brickline2_print:         ; Draw Darker Blue bricks
        cmp di, 1256
        ja brickline3
            mov ah , 0xA0
            mov al , 0x20
            mov cx , 8
            rep stosw
            mov cx , 1
            mov ax, 0x0720 
            rep stosw
            add si , 2  
            jmp brickline2_print
    brickline3:
        mov di, 1450
    brickline2_print3:      
        cmp di, 1576
        ja endn
            mov ah , 0xD0
            mov al , 0x20
            mov cx , 8
            rep stosw
            mov cx , 1
            mov ax, 0x0720 
            rep stosw
            add si , 2  
            jmp brickline2_print3
    endn:
    pop di
    pop si
    pop bx
    pop cx
    pop es
ret

; Erases the paddle (and ball if attached) before redraw
clrpaddle:                         
    push bp
    mov bp , sp
    push es
    push ax
    push di
    push cx
    mov ax , 0xb800
    mov es , ax
    mov ax , 0x0720
    mov cx , 9                ; Width of paddle
    mov di , [bp+4]
    rep stosw
    mov di,[cs:preBall]
    mov word[es:di],ax        ; Clear old ball too
    pop cx
    pop di
    pop ax
    pop es
    pop bp
ret 2

; Draws paddle and updates boundaries; handles ball-on-paddle logic
prntpaddle:            
    push bp
    mov bp , sp
    push es
    push ax
    push di
    push cx
    mov ax , 0xb800
    mov es , ax
    mov al , 0x20
    mov ah , 01000000b         ; Red background for paddle
    mov cx , 9
    mov di , [bp+4]
    mov word[cs:left_limit] , di
    rep stosw
    sub di , 2
    mov word[cs:right_limit] , di
    mov ax , word[cs:right_limit]
    sub ax,12
    mov word[cs:mid] , ax      ; Calculate paddle center
    sub ax,160                 ; Move center offset up one row
    mov di,ax
    shr ax,1
    sub ax,1680
    mov cx,ax
    cmp byte[cs:onPaddle],1
    jne endi
        mov al,'*'             ; Print ball attached to paddle
        mov ah,0x0F
        mov word[es:di],ax
        mov [cs:preBall],di
        mov word[cs:curr_row],21
        mov word[cs:curr_col],cx
        mov word[cs:previous],di
    endi:
    pop cx
    pop di
    pop ax
    pop es
    pop bp
ret 2

; Wrapper that determines movement direction and triggers redraws
paddle:                
    push ax
    push di
    cmp word[cs:right_] , 1
    je movRight
    cmp word[cs:left_] , 1
    je movLeft
    movRight:
        mov ax, word[cs:pre_stack_pos]
        add ax , 8            ; Attempt movement right
        cmp ax , word[cs:right_edge]
        ja exit1
            mov di, word[cs:pre_stack_pos]
            push di
            call clrpaddle
            push ax
            call prntpaddle
            mov word[cs:pre_stack_pos] , ax
            jmp exit1
    movLeft:
        mov ax, word[cs:pre_stack_pos]
        sub ax , 8            ; Attempt movement left
        cmp ax , word[cs:left_edge]
        jb exit1
            mov di, word[cs:pre_stack_pos]
            push di
            call clrpaddle
            push ax
            call prntpaddle
            mov word[cs:pre_stack_pos] , ax
            jmp exit1
    exit1:
        pop di
        pop ax
ret

; Calculates video memory offset from Row/Col coordinates
; Formula: (Row * 80 + Col) * 2
cal_pos: 
    push bp
    mov bp , sp
    push ax
    mov al , 80
    mul byte[bp+4]            ; Multiply Row by screen width
    add ax , [bp+6]            ; Add Column
    shl ax ,1                 ; Multiply by 2 (2 bytes per char)
    mov word[cs:cal_loc] , ax
    pop ax
    pop bp
    ret 4

; Updates ball coordinates based on movement flags (up/down/left/right)
nextposition:
    push ax
    push bx
    push cx
    
    mov al,[cs:incC]   ; Get horizontal movement flag (0=left, 1=right, 2=straight)
    mov ah,[cs:incR]   ; Get vertical movement flag (0=up, 1=down)
    mov bx,[cs:curr_col]
    mov cx,[cs:curr_row]    

    ; Boundary check: Left wall collision
    cmp word[cs:curr_col],3
    jne nextcond4
        mov al,1       ; Force direction to Right
        jmp rowCheck3
    nextcond4:          
        ; Boundary check: Right wall collision
        cmp word[cs:curr_col],77
        jne rowCheck3
            mov al,0   ; Force direction to Left
            
    rowCheck3:
        ; Boundary check: Top wall collision
        cmp word[cs:curr_row],4
        jne nextcond5
            mov ah,1   ; Force direction to Down
            jmp printingLocation1
        nextcond5:
            ; Boundary check: Bottom boundary
            cmp word[cs:curr_row],22
            jne printingLocation1
                mov ah,0 ; Force direction to Up
    
    printingLocation1:
        ; Apply horizontal movement
        cmp al,2
        je skip_col_move ; Skip if moving straight up
        cmp al,1
        jne nextcond6
            add bx,1     ; Move Right
            jmp rowCheck4
        nextcond6:
            sub bx,1     ; Move Left
        skip_col_move:
            
        rowCheck4:
            ; Apply vertical movement
            cmp ah,1
            jne nextcond7
                add cx,1 ; Move Down
                jmp calculatelocation1
            nextcond7:
                sub cx,1 ; Move Up

    calculatelocation1:
        push bx 
        push cx 
        call cal_pos     ; Convert updated coordinates to memory offset
        
    pop cx
    pop bx
    pop ax
ret

; Logic to determine if ball is hitting left or right side of the paddle
left_right:
    push ax
    
    mov ax , word[cs:cal_loc]
    cmp ax , [cs:mid]      ; Check against paddle midpoint
    ja check_right
    
    cmp ax , [cs:left_limit]
    jb endit
    mov byte[cs:left_or_right] , 0 ; Set flag to Left
    jmp endit
    
    check_right:
    cmp ax , [cs:right_limit]
    ja endit
    mov byte[cs:left_or_right] , 1 ; Set flag to Right
    jmp endit
    
    endit:
    pop ax
ret

; Main ball handling: collisions with bricks, paddle, and screen walls
ball:
    push es
    push ax
    push bx
    push cx
    push di
    
    mov ax,0xb800
    mov es,ax
    
    ; Erase ball from previous location
    mov di,[cs:previous]
    mov word[es:di],0x0720
    
    call nextposition      ; Calculate new intended position
    mov di,[cs:cal_loc]
    mov ax,word[es:di]     ; Check attribute at target position
    
    ; Collision detection logic
    cmp ah,0x07            ; Check if space is empty
    je R
        cmp ah,0xb0        ; Check if colliding with boundary/ui
        je n
        call brick_remove  ; Collided with brick: Remove brick
        jmp n1
        n:
            call left_right ; Collided with paddle: check hit location
            cmp byte[cs:left_or_right],1
            jne n3
                mov byte[cs:incC],1 ; Bounce Right
                jmp n1
            n3:
                mov byte[cs:incC],0 ; Bounce Left
        n1:
        ; Flip vertical direction upon collision
        cmp byte[cs:incR],1
        jne r1
            mov byte[cs:incR],0
            jmp R
        r1:
            cmp byte[cs:incR],0
            jne R
                mov byte[cs:incR],1
    R:
    ; Wall collision fallback checks
    cmp word[cs:curr_col],3
    jne nextcond
        mov byte[cs:incC],1
        jmp rowCheck
    nextcond:
        cmp word[cs:curr_col],77
        jne rowCheck
            mov byte[cs:incC],0
            
    rowCheck:
        cmp word[cs:curr_row],4
        jne nextcond1
            mov byte[cs:incR],1
            jmp printingLocation
        nextcond1:

        cmp word[cs:curr_row],22
        jne printingLocation
            ; Check if paddle missed: Lose a life
            mov ax, word[cs:curr_col]
            cmp ax, word[cs:left_limit]
            jb missed_paddle_hit
            cmp ax, word[cs:right_limit]
            ja missed_paddle_hit
            
            ; Paddle Hit: Bounce ball straight up
            mov byte[cs:incC], 2       
            mov byte[cs:incR], 0       
            mov word[cs:curr_row], 21  
            jmp printingLocation
            
        missed_paddle_hit:
            ; Ball fell past paddle
            mov byte[cs:onPaddle],1 
            mov ax,word[cs:mid]

            ; Reposition ball back onto paddle center
            sub ax,160
            mov di,ax
            shr ax,1
            sub ax,1680
            mov cx,ax

            mov al,'*'
            mov ah,0x0F
            mov word[es:di],ax
            mov [cs:preBall],di
            mov word[cs:curr_row],21
            mov word[cs:curr_col],cx
            mov word[cs:previous],di
            
            sub byte[cs:live],1 ; Decrement lives
            call print_lives
            jne end1

            jmp end1

    printingLocation:
        ; Final update of coordinates before rendering ball
        cmp byte[cs:incC],1
        jne nextcond2
            add word[cs:curr_col],1
            jmp rowCheck1
        nextcond2:
            sub word[cs:curr_col],1
            
        rowCheck1:
            cmp byte[cs:incR],1
            jne nextcond3
                add word[cs:curr_row],1
                jmp calculatelocation
            nextcond3:
            sub word[cs:curr_row],1
            
        calculatelocation:
            ; Math for final coordinate offset
            mov ax,word[cs:curr_row]
            mov bx,80
            mul bx
            add ax,word[cs:curr_col]
            shl ax,1
            mov di,ax
            mov word[cs:previous],ax
        
    mov ah,0x0F
    mov al,'*'
    mov word[es:di],ax ; Render the ball
    
    end1:
    pop di
    pop cx
    pop bx
    pop ax
    pop es
ret 

; UI routine for instructions display
instruction_menu:             
    push ax
    call clrscr
    call borders
    
    mov ax , 220
    push ax
    mov ax ,instructions_str
    push ax
    mov ax , 11
    push ax
    call printstr
    
    mov ax , 1010 
    push ax
    mov ax , ttl_live_str
    push ax
    mov ax , 32
    push ax
    call printstr
    
    mov ax , 1170
    push ax
    mov ax , play_str
    push ax
    mov ax , 24
    push ax
    call printstr

    mov ax , 1330
    push ax
    mov ax , space_bar
    push ax
    mov ax , 36
    push ax
    call printstr
    
    mov ax , 1490
    push ax
    mov ax , left_arrow
    push ax
    mov ax , 39
    push ax
    call printstr

    pop ax
    ret
    
; Interrupt Service Routine for Keyboard Input
kbisr:                    
    push ax
    push es
    
    ; Reset paddle flags
    mov word[cs:right_] , 0
    mov word[cs:left_] , 0
    mov ax, 0xb800
    mov es, ax 
    
    in al, 0x60 ; Fetch scan code from keyboard hardware
    
    cmp al, 0x10  ; Check for 'Q' key
    jne check_restart_key
    mov byte[cs:quit], 1
    mov byte[cs:end_game], 1 
    jmp exit

    check_restart_key:    
    cmp al, 0x13  ; Check for 'R' key
    jne continue_checks
    mov byte[cs:restart], 1
    mov byte[cs:end_game], 1 
    jmp exit

    continue_checks:           
    cmp byte[start_game] , 0
    jne main_game
    
    cmp al , 0x1c ; Check for Enter key
    jne cmp_instruction
    mov byte[start_game] , 1
    jmp exit
    
cmp_instruction:
    cmp al , 0x17 ; Check for 'I' key
    jne near exit
    mov byte[inst_men_status] , 1
    
    cmp byte[start_game] , 1
    jne exit
main_game:
    cmp al, 0x4b ; Left Arrow
    jne nextcmp 
        mov word[cs:left_] , 1
        call paddle 
        jmp near exit 
    nextcmp: 
        cmp al, 0x4d ; Right Arrow
        jne nextcmp2  
            mov word[cs:right_] , 1
            call paddle
            jmp exit
    nextcmp2: 
        cmp al, 0xad ; A key break scan code (ignored)
        jne nextcmp3 
        jmp exit
    nextcmp3: 
        cmp al, 0xab ; D key break scan code (ignored)
        jne nextcmp4 
        jmp exit 
    nextcmp4:
        cmp al,0x39  ; Spacebar: Release ball from paddle
        jne nextcmp5    
            mov byte[cs:onPaddle],0
            mov byte[cs:incC],2     
            mov byte[cs:incR],0        
        jmp exit
    nextcmp5:
        cmp al,0xb9
        jne exitcmp
        jmp exit
        
    exitcmp: 
    
    nomatch: 
        pop es
        pop ax
        jmp far [cs:oldisr] ; Pass control back to BIOS default ISR
    exit:
        mov al, 0x20
        out 0x20, al ; Send End of Interrupt to PIC
    pop es
    pop ax 
iret 

; Interrupt Service Routine for Timer
timer: 
    ; Execute logic if ball is moving and game has started
    cmp byte[cs:onPaddle],0
    jne endof
    cmp byte[cs:start_game] , 1
    jne endof
        inc byte[cs:tickcount]
        cmp byte[cs:tickcount], 2 ; Frame skip for speed control
        jne endof
            call ball
            call borders
            mov byte[cs:tickcount],0

        endof:
        mov al, 0x20
        out 0x20, al
iret  

; Visualize remaining lives using symbols (#)
print_lives:
    push ax
    push es
    push di
    push cx
    
    mov ax , 0xb800
    mov es , ax
    mov di , 452 ; UI position for lives
    mov cx , 5
    mov ax , 0x0720
    rep stosw
    
    mov cl , byte[cs:live]
    mov ch , 0
    mov ah , 0x07
    mov al , '#'
    mov di , 452
    rep stosw
    
    pop cx
    pop di
    pop es
    pop ax
ret

; Transition logic for ending game
end_menu:          
    push ax
        call clrscr
        
    cmp byte[live] , 1
    jne check_win
    mov ax , 1990

check_win:
    cmp word[total_b] , 0
    jne no_results

no_results:
    call end_menu_printing
    
; Spinlock for end menu interaction (waiting for R or Q flags via ISR)
next_do:                                            
    cmp byte[cs:restart] , 1
    je do_restart
    cmp byte[cs:quit] , 1
    je do_quit_menu
    jmp next_do

do_quit_menu:
    pop ax
    ret
        
do_restart:
    ; Reset all game variables for new attempt
    pop ax
    mov byte[start_game] , 1
    mov word[total_b] , 24
    mov byte[live] , 3
    mov word[score] , 0
    mov byte[end_game] , 0
    ret
        
; Display final screen with Score and restart instructions
end_menu_printing:                       
    call sound

    push ax
    
    mov ax , 380  
    push ax
    mov ax ,game_ov
    push ax
    mov ax , 15
    push ax
    call printstr

    mov ax , 1010  
    push ax
    mov ax ,total_score_str
    push ax
    mov ax , 17
    push ax
    call printstr
    
    mov ax , 1048
    push ax
    push word[score]
    call printnum
        
    mov ax , 1330  
    push ax
    mov ax ,restart_str
    push ax
    mov ax , 25     
    push ax
    call printstr
    
    mov ax , 1650  
    push ax
    mov ax , quit_str
    push ax
    mov ax , 23       
    push ax
    call printstr
    call borders

    pop ax
    ret
    
; --- Program Execution Entry Point ---
start:

    xor ax, ax
    mov es, ax ; Target IVT

    ; Save original Interrupt Handlers
    mov ax, [es:9*4]
    mov [oldisr], ax 
    mov ax, [es:9*4+2]
    mov [oldisr+2], ax 

    cli ; Halt interrupts during re-vectoring
        mov word [es:9*4], kbisr  ; Hook Keyboard
        mov [es:9*4+2], cs              
        mov word [es:8*4],timer   ; Hook Timer
        mov [es:8*4+2],cs
    sti ; Resume
    
main_menu_entry:
    mov byte[start_game], 0    
    mov byte[quit], 0       
    mov byte[restart], 0
    
    call start_menu
menu_loop:
    ; Main menu spin wait
    cmp byte[inst_men_status] , 1
    je instruction
    cmp byte[start_game] , 0
    je menu_loop
    cmp byte[end_game] , 1
    je endgame

start_game_here:
    ; Initialization before gameplay begins
    mov byte[restart] , 0
    mov byte[quit] , 0
    mov byte[end_game], 0      
    call clrscr
    call printTop
    call print_lives
    mov ax , 334
    push ax
    push word[score]
    call printnum
    call borders
    call bricks
    mov byte[onPaddle],1
    call paddle

game_inner_loop:
    ; Core gameplay loop checking win/loss conditions
    cmp word[total_b] , 0
    je endgame 
    
    cmp byte[quit], 1
    je endgame
    
    cmp byte[end_game] , 1
    je endgame
    cmp byte[live] , 0
    je endgame
    jmp game_inner_loop

instruction:
    call instruction_menu
again_ins:
    cmp byte[start_game] , 1
    je start_game_here
    jne again_ins
    
endgame:
    ; Game cleanup and end display
    mov byte[start_game] , 0
    call end_menu
    call clrscr

    cmp byte[restart] , 1
    je start ; Loop back to beginning for restart

    ; Restore old vectors for system stability
    mov ax , [oldisr]
    mov bx , [oldisr+2]
    mov cx , [oldtmr]
    mov dx , [oldtmr+2]

    mov ax , 0x4c00 ; Exit to DOS
    int 0x21
