[org 0x0100]                            ; Project Members:
                                        ; Oman Shahid    ( 22L-6556 )
jmp  start                              ; Syed Aown Raza ( 22L-6547 )

Row : dw 0
Column : dw 0
Constant : dw 132
Check : dw -70
CheckBoard: dw -17
RotateCheck : dw 0
buffer: times 1584 dw 0
oldisr: dd 0
TimerOldisr: dd 0
temp : dw 0

seconds: dw 0
timerflag: dw 0

randNum: dw 0
 
Score: dw 0
pauseFlag: dw 0 
 
message: db 'You Lost The Game'
introMessage: db 'Welcome to Bunny Jump'
introMessage2: db 'Developed by Oman Shahid (22L-6556) and Aown Raza (22L-6547)'
introMessage3: db 'Important Instructions about the Bunny Game'
introMessage4: db '1) press " UP " key for jumping'
introMessage5: db '2) 100 MilliSeconds Timer Fixed on Blue Brick'
introMessage6: db '3) If Rabbit does not jump on the Brick then GAME OVER'
introMessage7: db 'Press " P " to play Bunnuy jump Game'
scoreMessage: db 'Score : '
TimerMessage: dw 'Timer : '
Name1: db 'Enter your Name: '

Life: dw 1

carrotFlag: dw 0
CarrotCol: dw 0
 
; music_length dw 18644
; music_data incbin "getthem.imf"
 
 RabbitCurrCol: dw 180
 
Bar1Col: dw 180
Bar2Col: dw 180
Bar3Col: dw 180
stopBar: dw 2
CurrBoardNo: dw 3

message1:	db 'Player Name :$'

;following is input buffer in format required by service
buffer1:		db 80 								; Byte # 0: Max length of buffer
db 0 											; Byte # 1: number of characters on return
times 80 db 0 

;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

PlayerName:

push dx
push ax

       mov ah , 0x02
	   mov dh , 0x18
	   mov dl , 0x48
	   mov bh , 0
	   int 0x10
	   
		mov dx, buffer1 						; input buffer (ds:dx pointing to input buffer)
		mov ah, 0x0A 							; DOS' service A – buffered input
		int 0x21 								; dos services call

		mov bh, 0
		mov bl, [buffer1+1] 						; read actual size in bx i.e. no of characters user entered
		mov byte [buffer1+2+bx], '$' 			; append $ at the end of user input

		

pop ax
pop dx
ret

;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
IntroClrscr:
push ax
push si
push cx

mov ax, 0xb800
mov es , ax



mov si, 0
mov cx, 5676

loopClear:

mov word[es:si] , 0x0700
add si, 2

loop loopClear

mov si , 0

mov ax, 0x89CE


mov cx, 264

L5:
mov word[es: si] , ax
add si, 2
loop L5

mov si, 10824
mov cx, 264

l12:
mov word[es: si] , ax
add si, 2
loop l12




pop cx
pop si
pop ax


ret


;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

RabbitLife:

	push bp
mov bp , sp
push ax
push bx
push cx
push dx
push si
push di
push es
push ds
mov dx, 0
mov bx, 0


mov ax, [RabbitCurrCol]
mov dx, [Bar3Col]
sub ax, 2
sub dx, 4

add ax , 4

cmp ax, dx
jl NotFound2

add dx ,34
cmp ax,dx
jg NotFound2
jmp Found


NotFound2:
mov word[Life] , 0

Found:

pop ds
pop es
pop di
pop si
pop dx
pop cx
pop bx
pop ax
pop bp
ret

;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

clrscr:
push ax
push si
push cx

mov ax, 0xb800
mov es , ax

mov si, 7128
mov cx, 2112

loopClear1:

mov word[es:si] , 0x08B0
add si, 2

loop loopClear1


pop cx
pop si
pop ax


ret
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


Intro:

push ax
push si
push cx
mov ax, 0xb800
mov es, ax

call IntroClrscr

; push 

push 0x1636        ;; push coordinates 
push 0x07
push 21
push introMessage
call PrintStr

call Getch
call IntroClrscr
push 0x1836        ;; push coordinates 
push 0x07
push 17
push Name1
call PrintStr


call PlayerName



call IntroClrscr

push 0x1626        ;; push coordinates 
push 0x07
push 60
push introMessage2
call PrintStr


call Getch

call IntroClrscr

push 0x1436        ;; push coordinates 
push 0x07
push 43 ;36
push introMessage3
call PrintStr

push 0x1536        ;; push coordinates 
push 0x07
push 31 
push introMessage4
call PrintStr

push 0x1636        ;; push coordinates 
push 0x07
push 45
push introMessage5
call PrintStr

push 0x1736        ;; push coordinates 
push 0x07
push 54
push introMessage6
call PrintStr

push 0x1836        ;; push coordinates 
push 0x07
push 36
push introMessage7
call PrintStr

call Getch

pop cx
pop si
pop ax

ret
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
strlen: push bp
		mov bp,sp
		push es
		push cx
		push di
	
		les di, [bp+4] ;* point es:di to string di=[bp+4], es = [bp+6]
		mov cx, 0xffff ; load maximum number in cx
		xor al, al ; load a zero in al
		repne scasb ; find zero in the string
		mov ax, 0xffff ; load maximum number in ax
		sub ax, cx ; find change in cx
		dec ax ; exclude null from length

		pop di
		pop cx
		pop es
		pop bp
		ret 4 
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
Outro:

push ax
push si
push cx
mov ax, 0xb800
mov es, ax

call IntroClrscr

		push 0x1636
		push 0x09
		push 17
		push message
		call PrintStr
        
		push ds
		push buffer1+2
		call strlen
		
		push 0x1648
		push 0x09
		dec ax
		push ax
		push buffer1+2
		call PrintStr
		
		push 0x1836
        push 0x09
		push 8
		push scoreMessage
		call PrintStr
        
mov ax, [Score]
push 0x1940
push ax
call printnum	

pop cx
pop si
pop ax

ret
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
swapBar:
push ax
push ds
	mov ax, word [Bar3Col]
	mov word[temp] , ax
	
	mov ax, word [Bar2Col]
	mov word[Bar3Col] , ax
	
	mov ax, word [Bar1Col]
	mov word[Bar2Col],ax
	
	mov ax, [temp]
	mov word[Bar1Col] , ax

pop ds
pop ax
ret

;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

color1: db 0x44
color2: db 0x11
color3: db 0x55
colorTemp: db 0

;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
PrintAllBoard:

push bp
mov bp , sp
push ax
push bx
push cx
push dx
push si
push di
push es
push ds

	
		  mov ax , [color1]
		  push ax  ; 44 ->  Red colour of first Board (Attribute)
		  mov word[Row] , 27 
		  mov bx, [Bar1Col]
		  mov word[Column] , bx
		  push 32
		  call Board
		 
		  mov ax , [color2]
		  push ax  ; 44 ->  Red colour of first Board (Attribute)
		  mov word[Row] , 32
		  mov bx, [Bar2Col]
		  mov word[Column] , bx
		  push 32
		  call Board
		 
		  mov ax , [color3]
		  push ax  ; 44 ->  Red colour of first Board (Attribute)
		  mov word[Row] , 37
		  mov bx, [Bar3Col]
		  mov word[Column] , bx
		  push 32
		  call Board
		  

		  mov ah, [color3]
		  mov byte[colorTemp] , ah
		  
		  mov ah, [color2]
		  mov byte[color3] ,ah
		  
		  mov ah, [color1]
		  mov byte[color2] ,ah
		  
		  mov ah, [colorTemp]
		  mov byte[color1] ,ah

pop ds
pop es
pop di
pop si
pop dx
pop cx
pop bx
pop ax
pop bp

ret
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 kbisr:		
		
			
			push ax
			push es
            push bx
			push dx
			mov ax, 0xb800
			mov es, ax					; point es to video memory

			in al, 0x60									; read a char from keyboard port

			cmp al, 0x48						; is the key u
			jne nomatch
				
          ; mov word[Row] , 34
          ; call clearRabbit		
          ; call clrscr ; no, try next comparison
			; call SaveScreen
			; call Scroll
			call restoreScreen
			call clrscr
			call swapBar
				
			call PrintAllBoard
			

		   mov word[seconds] , 0     ; when rabbit jump from bule to another brick timer set to 0
		   
		   add word[timerflag] , 1
		   cmp word[timerflag] , 3 
		   jne moveForward
		   mov word[timerflag] , 0

moveForward:			
			call RabbitLife
			; cmp word[Life] , 0
			; je Dead1
			cmp word[carrotFlag] , 2
            jne NoZero

        mov word[carrotFlag] , 0
NoZero:		
		cmp word[carrotFlag] , 0
		jne NoCarrot
		
		call GenRandNum      ; Carrot  random col generator
		mov dx,[randNum]
		mov ax, 4
		mul dx
		
		 add ax, 180
		
		 mov word[CarrotCol] , ax
		 
         mov word[Column] , ax
		  mov word[Row] , 30 ;26
         call Carrot
		 
		
NoCarrot:
			call PrintScore
			add word[carrotFlag] , 1
			
    mov word[Row] , 34
    mov dx, [RabbitCurrCol]
    mov word[Column] , dx
    call Rabbit

    nomatch:		
			
			SecondKey:
			cmp al, 0x81
			jne ThirdKey
			mov word[pauseFlag] , 1
			; jmp Dead1
			
			ThirdKey:
			cmp al , 0x15
			jne Exit
			mov word[Life] , 0
			
			; FourthKey:
			; cmp al , 0x31
			; jne Exit
			; mov word[Life] , 1
Exit:
			
			mov al, 0x20
			out 0x20, al
			pop dx
			pop bx
			pop es
			pop ax
			jmp far [cs:oldisr] ; call the original ISR
			iret
			
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

pauseMsg: db 'Do You really Want To exit (Y / N)'

;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
clearRabbit:
push ax
push bx
push cx
push dx
push si
push di
push es
    push ds

	mov ax , 132         ; Calculate Location of Rabbit to create ((132*r)+c)*2
   mul word[Row]
   shl ax , 1

   mov di , ax
   mov ax , 0xB800
   mov es ,  ax
   
   mov ax , 0x2AB0 
	mov cx, 396
	
	
	LoO1:
	
	mov word[es:di] , ax
	add di,2
	
	loop LoO1
   
   
    pop ds
    pop es
pop di
    pop si
pop dx
pop cx
pop bx
pop ax


ret
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
SaveScreen:

push ds
push si
push cx
push ax
push es
push di


mov si, 10032			; 38-43
mov cx, 792				; 6x132

mov di,  0
mov ax, buffer
mov es, ax
mov ax, 0xb800
mov ds, ax

cld
rep movsw
pop di
pop es
pop ax
pop cx
pop si
pop ds
ret

;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
restoreScreen:
push ds
push si
push cx
push ax
push es
push di

mov si, 0
mov cx, 660

mov di, 7392				; 28x132x2
mov ax, buffer
mov ds, ax
mov ax, 0xb800
mov es, ax

cld
rep movsw

pop di
pop es
pop ax
pop cx
pop si
pop ds

ret

;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

Scroll:


push ds
push di
push si
push ax
push es
push cx
mov di, 11352
mov si, 10032			;(37x132+132) x 2

mov ax, 0xb800
mov es, ax
mov ds, ax

mov cx, 1452

std
rep movsw

pop cx
pop es
pop ax
pop si
pop di
pop ds
ret
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
DelayTime: dw 0xFFFF

Delay:      
            push cx
mov cx, [DelayTime]
loop1: 
loop loop1
; mov cx, 0xFFFF
; loop2: loop loop2
pop cx
ret

;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

 weather:
    push ax
push bx
push di
push es
   
   
   
mov ax, 1003h  ; this code is used to stop bilnking
    mov bx, 0      
    int 10h
 
mov ax , 0xB800
mov es , ax
mov ah , 0x99  ; 99 -> Light Blue (Sky) , 66 -> orange , 55-> Magenta background and Foreground
mov al , '-'

weatherLoop1:
mov word[es:di] , ax
add di , 2
cmp di , 3960 ; First cell of Row 15 (15*132*2=3960)
jne weatherLoop1

RabbitScreen:
   
   mov ax , 0x2AB0
   mov di , 7128    ; (27x132x2) -> First cell of Row 27
RabbitLoop1:
    stosw
    cmp di , 11616
jne RabbitLoop1



pop es
pop di
pop bx
pop ax

ret

;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

Sun:
    push ax
push cx
push di
push es
 
   
mov ax , 0xB800
mov es , ax
mov ax , [Row]
mul word[Constant]
add ax , [Column]
mov cx , 2
mul cx
add ax , 2        ; add 2 so that printing start from next cell (Building)

mov di , ax       ; Store Starting index of Building to be built in di
mov ah , 0x9E     ; 9 -> Light blue Background and E -> Yellow ForeGround
mov al , '\'
   
    mov word[es:di] , ax
add di , 2
mov al , ' '
    mov word[es:di] , ax
add di , 2
mov al , '|'
    mov word[es:di] , ax
add di , 2
mov al , ' '
    mov word[es:di] , ax
add di , 2
mov al , '/'
    mov word[es:di] , ax

    add di , 254
mov al , '-'
    mov word[es:di] , ax
  add di , 2
mov al , '-'
    mov word[es:di] , ax
add di , 2
mov al , ' '
    mov word[es:di] , ax
add di , 2
mov al , 'o'
    mov word[es:di] , ax
add di , 2
mov al , ' '
    mov word[es:di] , ax
add di , 2
mov al , '-'
    mov word[es:di] , ax
add di , 2
mov al , '-'
    mov word[es:di] , ax

add di , 254
    mov al , '/'
    mov word[es:di] , ax
add di , 2
mov al , ' '
    mov word[es:di] , ax
add di , 2
    mov al , '|'
    mov word[es:di] , ax
add di , 2
    mov al , ' '
    mov word[es:di] , ax
add di , 2
mov al , '\'
    mov word[es:di] , ax

    pop es
pop di
pop cx
pop ax

ret
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

 Bird:
    push ax
push cx
push di
push es
   
mov ax , 0xB800
mov es , ax
mov ax , [Row]
mul word[Constant]
add ax , [Column]
mov cx , 2
mul cx
add ax , 2        ; add 2 so that printing start from next cell (Building)

mov di , ax       ; Store Starting index of Building to be built in di
mov ah , 0x90     ; 9  -> Light Blue background and 0 -> Black Foreground
mov al , '^'
mov word[es:di] , ax
add di , 2
mov al , '\'
mov word[es:di] , ax
add di , 2
mov al , '/'
mov word[es:di] , ax
add di , 2
mov al , '^'
mov word[es:di] , ax

pop es
pop di
pop cx
pop ax

ret

;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 
Road:
    push ax
push di
push es
   
mov ax , 0xB800
mov es , ax
mov di , 3696  ; (14 x 132 x 2 )+2 start from row 14
mov ah , 0x62  
mov al , 0xB0

RoadMove1:             ; Print Garden Area
    mov word[es:di] , ax  
    add di , 2
    cmp di , 4488     ;  (17*132*2=4488) -> starting cell of  17 row
jne RoadMove1

    mov di , 4488 ; (17*132*2=4488) ->  starting cell of  17 row  
mov ah , 0x88 ; 88 means Dark Gray Foreground and Black Background (Yellow = Red+Green)
mov al , '-'
RoadMove2:
    mov word[es:di] , ax  
    add di , 2
    cmp di , 5544 ; (21x132x2=5544) -> Row no 21 (First cell)  
jne RoadMove2

    mov di , 5544   ;  (21x132x2=5544) -> starts from Row no 22  
mov ah , 0x06   ; 06 means yellow foreground and black background(Yellow=Red+Green)
mov al , '='
RoadMove3:
    mov word[es:di] , ax  
    add di , 2
    cmp di , 6072 ; (23x132x2=6072) -> First cell of Row no 23
jne RoadMove3

mov ah , 0x88 ; 88 means dark gray Foreground and Background
mov al , '-'
RoadMove4:
    mov word[es:di] , ax  
    add di , 2
    cmp di , 7128   ; ( 27 x 132 x 2 ) -> First cell of Row 27
jne RoadMove4

pop es
pop di
pop ax

ret

;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


Building:
    push bp
    mov bp, sp
  push ax
push bx            
push cx
push dx
push si
push di
push es

mov ax , 0xB800
mov es , ax
mov ax , [Row]
mul word[Constant]
add ax , [Column]
mov cx , 2
mul cx
add ax , 2        ; add 2 so that printing start from next cell (Building)

mov si ,ax        ; store starting index in si which is used in BuildingFilling function

mov cx , [bp+4]   ; Store size of Building in CX
mov di , ax       ; Store Starting index of Building to be built in di
mov ah , 0x44     ; 44 means Red background and Foreground Colour
mov al , '_'
                 ; Create Box of Building
BulidingMoveRight:
mov word[es:di] , ax  
add di , 2              ; Column increment by 2
loop BulidingMoveRight

mov cx , [bp+4]        ; Store size of Building in CX
mov ah , 0x44          ; 44 means Red background and Foreground Colour
mov al , '|'
mov word[es:di] , ax  ;print last cell of the row where building starts before going to row increment
add di , 264           ; row increment 132 x 2 = 264

BulidingMoveDown:
mov word[es:di] , ax  
add di , 264           ; row increment 132 x 2 = 264
loop BulidingMoveDown

    mov cx , [bp+4]        ; Store size of Building in CX
mov ah ,0x44           ; 44 means Red background and Foreground Colour
mov al , '_'
sub di , 264            ; 1 Row decrement so that over printing is Good looking (Building)
sub di , 2              ; Column Decrement by 2 to move left
BulidingMoveLeft:
mov word[es:di] , ax  
sub di , 2              ; Column Decrement by 2 to move left
loop BulidingMoveLeft

    mov cx , [bp+4]        ; Store size of Building in CX
mov ah , 0x44          ; 44 means Red background and Foreground Colour
mov al , '|'

BulidingMoveUP:
mov word[es:di] , ax  
sub di , 264              ; Row decrement to move up 132 x 2 = 264  
loop BulidingMoveUP
   
mov word[es:di] , ax  ; print on the cell where Building starts (Left most corner where building is start)

 ; Building Box is created Now we have to fill colours in it for Good looking

BuildingColour:

    mov cx, [bp+4]   ; store Size of Building
    sub cx, 1      
    mov dx, cx       ; OuterLoop Condition size ( Rows )
mov bx , cx      ; InnerLoop Iteration condition ( Column printing on each Row)
mov ah , 0x44    ; 44 -> red background and Foreground colour
mov al , '-'


FillLoop:
    add si, 264  ; move to next row
    mov di, si
    add dx , 1

    _innerLoop:

    mov word[es:di] , ax
    add di,2

    sub dx, 1
    jnz _innerLoop
    mov dx, bx
    loop FillLoop


BuildingWindows:

    mov ax , [Row]
mul word[Constant]
add ax , [Column]
mov cx , 2
mul cx
add ax , 2        ; add 2 so that printing start from next cell (Building)
mov si , ax       ; store starting index of Building in si

    mov cx, [bp+4]   ; store Size of Building
    mov di , 0
    sub cx, 1     ; skip the border of building means 10x10 then windows area is 9x9      
    mov dx, cx
    shr dx , 1    ;windows number loop(half of the actual size -> no. of Columns of windows printing )
    mov bx , cx
    shr bx  , 1     ; divide by 2 windows row
    mov ah , 0xFF   ; FF -> White background and Foreground Colour
    mov al , '-'
    ;sub cx , 2
    shr cx , 1     ; divided by 2 ( no. of rows of windows printing )

WindowsLoop:       ;( no. of rows of windows printing )
    add si, 528  ; skip 2 row( Moves 2 row  downward )
    mov di, si
    add dx , 1

  WindowsinnerLoop:     ;( no. of Columns of windows printing )

    mov word[es:di] , ax
    add di , 4       ; skip 1 column rightward

    sub dx , 1
    jnz WindowsinnerLoop

    mov dx, bx
    loop WindowsLoop

 
pop es
pop di
pop si
pop dx
pop cx
pop bx
pop ax
pop bp

ret 2

;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

  Trees:
    push ax
push bx
push cx
push dx
push si
push di
push es

mov ax , 0xB800
mov es , ax
mov ax, [Row]
mov bx, 264
mul bx

mov si, [Column]
add si, ax
mov bx, si
mov cx, 4


mov ax, 1
TreeOuterLoop:
mov dx, 0
mov di, si

TreeInnerLoop:

mov word[es:di] , 0xA2B0   ; A2 -> A Light Green background and 2 Green Foreground , B0 is ASCII of symbol
add di,2

add dx, 1
cmp ax, dx
jne TreeInnerLoop
add ax, 2

add si, 262

loop TreeOuterLoop

mov si, bx
mov bx, 4
mov ax, 264
mul bx
add si , ax

mov word[es:si] , 0x96Ba

pop es
pop di
pop si
pop dx
pop cx
pop bx
pop ax

    ret

;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

  Truck:
   
push bp
    mov bp, sp
  push ax
push bx            
push cx
push dx
push si
push di
push es

   mov ax, 0xB800  
   mov es, ax
   mov ax, [Row]    ; calculating the starting index of printing
   mov bx, 264
   mul bx

   mov si, [Column]
   add si, ax

;---------
;---------
;---------

    mov cx, 3   ; truck rows 3 (height) -> size
    mov dx, 9   ; truck  col 9 ( width )
    mov bx, 9   ; truck  col 9 ( width )

CarOuterloop:     ; row loop -> 3
 
    mov di, si
CarInnerLoop:  ; col loop -> 9

    mov word[es:di] , 0x4456
    add di,2

    sub dx, 1
    jnz CarInnerLoop

    mov dx, bx
    add si, 264

loop CarOuterloop

; Now Front cabin ( Driver Cabin )

    mov ax, [Row]
    mov bx, 264
    mul bx

    mov si, [Column]
    add si, ax

    add si, 282  ; go to 2nd line and starting position of cabin

    mov cx, 2   ; truck Cabin rows 2 (height) -> size
    mov dx, 3   ; truck Cabin col 3 ( width )
    mov bx, 3   ; truck Cabin col 3 ( width )

CabinOuterLoop:      ; cabin loop -> 2 (height)

    mov di,si
mov ah , 0x66
    mov al , 0x56
CabinInnerLoop:  ; cabin inner loop -> 3

    mov word[es:di] ,  ax ;0x6656
    add di, 2

    sub dx, 1
    jnz CabinInnerLoop

    mov ah , 0x8B
    mov al , 0x2E
 
HeadLights:

    mov word[es:di] , ax  ; Headlight
    mov dx,bx
    add si, 264

loop CabinOuterLoop

 Wheels:              
    mov ax, [Row]      ; Wheel
    mov bx, 264
    mul bx

    mov si, [Column]
    add si, ax
    add si, 796 ; (264x3 + 4 = 796 )

    mov ah , 0x8D
    mov al , 'o'  
mov word[es:si] , ax
add si, 16
mov word[es:si] , ax

    pop es
pop di
pop si
pop dx
pop cx
pop bx
pop ax
pop bp

    ret

;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

RotatePartition1Left :
   
    push ax
push bx
push cx
push dx
push si
push di
push es
    push ds

mov ax , 132
    mul word[Row]
    add ax , word[Column]
    shl ax  , 1
   
    mov di , ax
mov si , di
add si , 2

    mov ax  , 0xB800
mov es , ax
mov ds , ax
   
    mov dx , [es:di]

mov cx , 131

cld
rep movsw
   
     mov [es:di] , dx

pop ds
     pop es
pop di
pop si
pop dx
pop cx
pop bx
pop ax


    ret
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 
 RotatePartition2Right:
    push ax
push bx
push cx
push dx
push si
push di
push es
    push ds

mov ax , 132
    mul word[Row]
    add ax , word[Column]
    shl ax  , 1
   
    mov di , ax
mov si , di
sub si , 2

     mov ax  , 0xB800
mov es , ax
mov ds , ax
   
     mov dx , [es:di]

mov cx , 131

std
rep movsw
   
     mov [es:di] , dx
pop ds
     pop es
pop di
pop si
pop dx
pop cx
pop bx
pop ax


    ret
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 RotateLeft:
   push ax
    mov ax , 1

LeftLoop:
mov word[Row] , ax
mov word[Column] , 0
    call RotatePartition1Left
add ax , 1
cmp ax , 17
jne LeftLoop

pop ax
ret
   
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 
RotateRight:
    push ax
mov ax , 27

RightLoop:
   mov word[Row] , ax
   mov word[Column] , 131
   call RotatePartition2Right
   sub ax , 1
   cmp ax , 16
   jne RightLoop
   
   pop ax
ret
 
 ;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>.
 
GenRandNum:
push bp
mov bp,sp;
push cx
push ax
push dx;


 rdtsc
    ; The result is stored in EDX:EAX

    ; XOR the two halves of the TSC to introduce some entropy
    xor eax, edx

    ; Modulo operation to limit the range
    mov cx, 10       ; Set the divisor
    div cx           ; Divide by 10, remainder in dx

    ; Store the random number in randNum
    mov word [randNum], dx



pop cx;
pop ax;
pop dx;
pop bp;
ret
 
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

 Rabbit:
    push ax
push bx
push cx
push dx
push si
push di
push es
    push ds

   mov ax , 132         ; Calculate Location of Rabbit to create ((132*r)+c)*2
   mul word[Row]
   add ax , word[Column]
   shl ax , 1

   mov di , ax
   mov ax , 0xB800
   mov es ,  ax
	 add di, 264
	mov ax, 0x79DB
   mov word[es:di] , ax
	
  
   mov ah , 0x21  
   mov al , '('
   stosw
   mov al , '\'
   stosw                         ;  (\ /)
   mov al , ' '                  ; ( 0 0 )
   stosw                      
   mov al , '/'
   stosw
   mov al , ')'
   stosw
   
   add di , 252
   
   mov al , '('
   stosw
   mov al , ' '
   stosw
   mov al , '0'
   stosw
   mov al , ' '
   stosw
   mov al , '0'
   stosw
   mov al , ' '
   stosw
   mov al , ')'
   stosw
   
   
    pop ds
    pop es
pop di
    pop si
pop dx
pop cx
pop bx
pop ax

ret
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

Carrot:
    push ax
push bx
push cx
push dx
push si
push di
push es
    push ds

      mov ax , 132         ; Calculate Location of Rabbit to create ((132*r)+c)*2
   mul word[Row]
   add ax , word[Column]
   shl ax , 1

   mov di , ax
   mov ax , 0xB800
   mov es ,  ax
   
   mov ah , 0x66 ;0x21  
   mov al , '|'
   stosw
   stosw
   stosw
   mov ah , 0x44 ;0x21  
   add di , 258
   
   mov al , '\'
   stosw                    
   mov al , ' '
   stosw                
   mov al , '/'
   stosw
    pop ds
    pop es
pop di
    pop si
pop dx
pop cx
pop bx
pop ax

ret
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

Board:
    push bp
mov bp , sp
    push ax
push bx
push cx
push dx
push si
push di
push es
    push ds

   mov ax , 132         ; Calculate Location of Board to create ((132*r)+c)*2
   mul word[Row]
   add ax , word[Column]
   shl ax , 1
 
   mov di , ax
   mov ax , 0xB800
   mov es , ax
   
   mov cx , [bp+4]   ; store size of Board in to CX register
   mov ah , [bp+6]   ; store attribute (colour) of Board
   mov al , '-'
   
   rep stosw
   
    pop ds
    pop es
pop di
    pop si
pop dx
pop cx
pop bx
pop ax
pop bp
ret 4

;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

BoardRotation:
    push bp
mov bp , sp
    push ax
push bx
push cx
push dx
push si
push di
push es
    push ds

  cmp word[Check], 0
  jle BoardRotationLoop2
   
   
BoardRotationLoop1:      ; move board leftward
    call RotatePartition1Left
call Delay
	
    sub word[Check] , 1
    cmp word[Check] , 0
    je right
 
    jmp last
   
   
BoardRotationLoop2:      ; move board rightward
    add word[Row] , 1
    call RotatePartition2Right
call Delay
	
    add word[Check] , 1
cmp word[Check] , 0
je left
    jmp last
 
left:
    sub word[Row] , 1
    mov word[Check] , 140
     jmp last
 
right:
add word[Row] , 1
mov word[Check] , -140


last:  





    pop ds
    pop es
pop di
    pop si
pop dx
pop cx
pop bx
pop ax
pop bp
ret
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
BoardRotate:
 
  mov word[Row] , 28
  mov word[Column] , 0
  call BoardRotation
 
 
 
  ; mov word[Row] , 33
  ; mov word[Column] ,0
  ; call BoardRotation
 
  mov word[Row] , 36	;only for rabbit
  mov word[Column] , 0
  call BoardRotation
  
   mov word[Row] , 37	;only for rabbit
  mov word[Column] , 0
  call BoardRotation
  
  mov word[Row] , 38
  mov word[Column] , 0
  call BoardRotation

;...............................................................................

    cmp word[CheckBoard], 0
  jle add1
   
   
sub1:
    sub word[Bar1Col] , 2
	sub word[Bar3Col] , 2

	
   sub word[RabbitCurrCol],2
    
   sub word[CheckBoard] , 1
cmp word[CheckBoard] , 0
je right2
    jmp last2
   
  
add1:

    add word[Bar1Col] , 2
	add word[Bar3Col] , 2

   add word[RabbitCurrCol] , 2
   add word[CheckBoard] , 1
cmp word[CheckBoard] , 0
je left2
    jmp last2
   
left2:
   
    mov word[CheckBoard] , 34
     jmp last2
 
right2:

mov word[CheckBoard] , -34   

last2:  

	

ret
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
PrintScore:

	push ax
	push bx
	push cx
	push ds
	push es
	push si

   cmp word[carrotFlag] , 1
    jne NotInc
   
   mov bx , word[CarrotCol]
   add bx , 2
   
   mov ax, word[RabbitCurrCol]
   sub ax , 6
   
    mov dx, word[RabbitCurrCol]
    add dx , 6
   
   cmp  bx , ax 
   jl NotInc
   cmp bx , dx
   jg NotInc
   
   add word[Score] ,1
   ; call Playmusic
   
NotInc:

mov ax, [Score]
push 20
push ax
call printnum

pop si
pop es
	pop ds
	pop cx
	pop bx
	pop ax

ret
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

PrintStr:

	push bp
	mov bp , sp
	push ax
	push bx
	push cx
	push ds
	push es
	
			xor ax, ax
			xor bx , bx
			mov ah, 0x13
			mov bh, 0
			mov bl, [bp+8]
			mov dx, [bp+10]
			mov cx, [bp+6]
			push cs
			pop es
			mov bp, [bp+4]
			int 0x10
	
	pop es
	pop ds
	pop cx
	pop bx
	pop ax
	pop bp


ret 8

;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

; Music:dw 0

; Playmusic:
	; push bp
	; mov bp , sp
	; push ax
	; push bx
	; push cx
	; push ds
	; push es

; ; 2) now let's just read "getthem.imf" file content
		; ;    every 4 bytes. i'll use si register as index.
		
		; ; 2) now let's just read "getthem.imf" file content
		; ;    every 4 bytes. I'll use SI register as index.
		
		; mov si, 0 ; current index for music_data
		
	; .next_note:
	
		; ; 3) the first byte is the opl2 register
		; ;    that is selected through port 388h
		; mov dx, 388h
		; mov al, [si + music_data + 0]
		; out dx, al
		
		; ; 4) the second byte is the data need to
		; ;    be sent through the port 389h
		; mov dx, 389h
		; mov al, [si + music_data + 1]
		; out dx, al
		
		; ; 5) the last 2 bytes form a word
		; ;    and indicate the number of waits (delay)
		; mov bx, [si + music_data + 2]
		
		; ; 6) then we can move to next 4 bytes
		; add si, 4
		
		; ; 7) now let's implement the delay
		
		
		; mov cx, 90 ; <- change this value according to the speed
		              ; ;    of your computer / emulator
	; .delay:
	
		; ; if keypress then exit
		
		
		; loop .delay
		
		
		
		; ; 8) let's send all content of music_data
		; cmp si, [music_length]
		; jb .next_note
		
	; .exit:
; pop es
	; pop ds
	; pop cx
	; pop bx
	; pop ax
	; pop bp
	
; ret
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>.

;--------------------------------------------------------
; timer interrupt service routine
;--------------------------------------------------------
timer:		push ax
			
			sub word[DelayTime],50
			cmp word [cs:timerflag], 1 ; is the printing flag set
			jne skipall ; no, leave the ISR

			inc word [cs:seconds] ; increment tick count
			push 256
			push word [cs:seconds]

			call printnum ; print tick count
            cmp word[cs:seconds] , 100
			jne skipall
			
			mov word[Life] , 0
			
skipall:	mov al, 0x20
			out 0x20, al ; send EOI to PIC
			pop ax
			iret ; return from interrupt
;--------------------------------------------------------
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
mov es, ax ; point es to video base
mov ax, [bp+4] ; load number in ax
mov bx, 10 ; use base 10 for division
mov cx, 0 ; initialize count of digits
nextdigit: mov dx, 0 ; zero upper half of dividend
div bx ; divide by 10
add dl, 0x30 ; convert digit into ascii value
push dx ; save ascii value on stack
inc cx ; increment count of values
cmp ax, 0 ; is the quotient zero
jnz nextdigit ; if no divide it again
mov di, [bp+6] ; point di to 70th column
nextpos: pop dx ; remove a digit from the stack
mov dh, 0x07 ; use normal attribute
mov [es:di], dx ; print char on screen
add di, 2 ; move to next screen location
loop nextpos ; repeat for all digits on stack
pop di
pop dx
pop cx
pop bx
pop ax 
pop es
pop bp
ret 4
;--------------------------------------------------------
Getch: xor ah, ah    ; ah = 0
int 16h
ret
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 PrintMainScreen:
   
; following code just changes your screen resolution to 43x132 Mode



  call Intro
  call weather
 
  mov word[Row] , 2
  mov word[Column] , 92
  call Sun
 
  mov word[Row] , 0
  mov word[Column] , 2
  call Bird
 
  mov word[Row] , 1
  mov word[Column] , 6
  call Bird
 
  mov word[Row] , 1
  mov word[Column] , 20
  call Bird
 
  mov word[Row] , 2
  mov word[Column] , 30
  call Bird
 
  mov word[Row] , 2
  mov word[Column] , 38
  call Bird
 
  mov word[Row] , 4
  mov word[Column] , 40
  call Bird
 
  mov word[Row] , 2
  mov word[Column] , 50
  call Bird
 
  mov word[Row] , 3
  mov word[Column] , 60
  call Bird
 
  mov word[Row] , 1
  mov word[Column] , 70
  call Bird
 
  mov word[Row] , 4
  mov word[Column] , 80
  call Bird
 
  mov word[Row] , 4
  mov word[Column] , 6
  push 10                ; size of Building
  call Building
 
  mov word[Row] ,   6
  mov word[Column] , 22
  push 8                ; size of Building
  call Building
 
  mov word[Row] , 4
  mov word[Column] , 40
  push 10                ; size of Building
  call Building
 
  mov word[Row] ,  6
  mov word[Column] , 64
  push 15                 ; size of Building
  call Building
 
  mov word[Row] , 4
  mov word[Column] , 104
  push 20                ; size of Building
  call Building
 

  call Road
 
  mov word[Row] , 17
  mov word[Column] , 50
  call Truck
 
  mov word[Row] , 22
  mov word[Column] , 150
  call Truck
 
  mov word[Row] , 12
  mov word[Column] , 6
  call Trees
 
  mov word[Row] , 11
  mov word[Column] , 70
  call Trees
 
  mov word[Row] , 10
  mov word[Column] , 116
  call Trees
 
  mov word[Row] , 11
  mov word[Column] , 174
  call Trees
 
  mov word[Row] , 12
  mov word[Column] , 200
  call Trees
 
  mov word[Row] , 11
  mov word[Column] , 256
  call Trees
 
  

 
 
 
; mov word[Row] , 35
 ; mov word[Column] , 193
 ; call Rabbit
 call PrintAllBoard


push 0x0030        ;; push coordinates 
push 0x07
push 13
push message1
call PrintStr

push ds
		push buffer1+2
		call strlen		

push ax
push 0x0040        ;; push coordinates 
push 0x07
dec ax
push ax
push buffer1+2
call PrintStr
pop ax
push 0x0002
push 0x07
push 8
push scoreMessage
call PrintStr

push 0x0078
push 0x07
push 8
push TimerMessage
call PrintStr
 
 
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
   
  
PlayAnimation:
cmp word[pauseFlag] ,1
	jne NoPause
	call IntroClrscr
	push 0x1636        ;; push coordinates 
push 0x07
push 34
push pauseMsg
call PrintStr
	
	
NoPause:
	cmp word[Life] , 0
	je Dead1
	
 
   call RotateLeft
   ;call Delay
   ; call Delay
 
   call RotateRight
   ;call Delay
   ; call Delay
  
   call BoardRotate
   ; call Delay
   
 
   jmp PlayAnimation
   
   
ret
   
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
   
start:
 mov ah , 0x00
 mov al , 0x54
 int 0x10
 xor ax, ax
		mov es, ax										; point es to IVT base
		cli
		mov ax, [es:9*4]
		mov [oldisr], ax								; save offset of old routine
		mov ax, [es:9*4+2]
		mov [oldisr+2], ax		
				
		mov ax, [es:8*4]
		mov [TimerOldisr], ax								; save offset of old routine
		mov ax, [es:8*4+2]
		mov [TimerOldisr+2], ax					; save segment of old routine
			
														; disable interrupts
		mov word [es:9*4], kbisr						; store offset at n*4
		mov [es:9*4+2], cs								; store segment at n*4+2
		mov word [es:8*4], timer ; store offset at n*4
		mov [es:8*4+2], cs ; store segment at n*4+
		sti					
 call PrintMainScreen
 call PlayAnimation  
 
 Dead1:
			
			call Outro
		
		cli
		mov ax,[oldisr]	
		mov word [es:9*4], ax
		mov ax,[oldisr+2]		
		mov [es:9*4+2], ax
		
		mov ax,[TimerOldisr]	
		mov bx,[TimerOldisr+2]
		
		mov word [es:8*4], ax	
		mov [es:8*4+2], bx
		sti	
	

; mov ah, 0
		; mov al, 3
		; int 0x10

mov dx, start
add dx , 15
mov cl, 4
shr dx, cl			
			
mov ax ,0x4c00
int 0x21
