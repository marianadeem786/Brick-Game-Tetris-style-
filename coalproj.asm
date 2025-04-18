[org 0x0100] 

 jmp start 

 
scored: dw 0
score: dw 'Score: 0'
len: dw 8

time: dw 'time: '
len3: dw 6
nshape: dw 'Next shape: '
len4: dw 12
end: dw '[Press enter to end game]'
len5: dw 25
main: dw '__GAME STARTED__'
len6: dw 16
ended: dw '__GAME ENDED__'
len7: dw 14
notfull: dw 0


min: dw 0 
sec1: dw 0
sec2: dw 0
right: dw 0
left: dw 0
tickcount: dw 0 
moveflag: dw 0
oldisr: dd 0 
oldisr2: dd 0
count: dw 300
current: dw 0

; keyboard interrupt service routine 
kbisr:

 in al, 0x60 ; read a char from keyboard port 
 cmp al, 0x4b ; is the key left arrow 
 jne nextcmp ; no, try next comparison 
 
 mov word[cs: left], 1

 jmp nomatch ; leave interrupt routine
 
nextcmp: cmp al, 0x4d ; is the key right shift 
 jne nextcmp2 ; no, leave interrupt routine 
 mov word[cs: right], 1


 jmp nomatch
 nextcmp2: cmp al, 0x1c ; is the enter key 
 jne nomatch
 je near exit ; no, leave interrupt routine 
 


nomatch: ;mov al, 0x20 
 ;out 0x20, al ; send EOI to PIC 

 ;iret 
 jmp far [cs:oldisr2] 


timer:
 
  cmp word [cs:count], 0
  je endd

  push ax
 inc word [cs:tickcount]; increment tick count 
 
 cmp word[cs:tickcount], 18
 jle endy
 sub word[cs: count], 1
 mov word[cs: tickcount], 0
  cmp word[cs:sec2] , 9
  jge addsec1
  add word[cs:sec2], 1
  jmp continue
  addsec1:
  mov word[cs: sec2], 0
  cmp word[cs:sec1], 5
  jge addmin
  add word[cs:sec1], 1
  jmp continue

  addmin:
  mov word[cs: sec1], 0
  add word[cs:min], 1
  
   continue:
   
 call printnum ; print tick count 
 endy:
; mov al, 0x20
 ;out 0x20, al ; end of interrupt 
 pop ax 

  ;iret ; return from interrupt 
  jmp far [cs:oldisr] 
 
 endd:
 call clrscr
 call displogo2
 xor ax, ax 
 mov es, ax ; point es to IVT base 
 
 
 cli ; disable interrupts 
  mov ax, [oldisr2]
 mov [es:8*4], ax ; restore old offset from ax 
 mov ax, [oldisr2+2]
 mov [es:8*4+2], ax ;
 mov ax, [oldisr]
 mov [es:9*4], ax ; restore old offset from ax 
 mov ax, [oldisr+2]
 mov [es:9*4+2], ax ; restore old segment from bx 
sti

			mov ax, 0x4c00									; terminate program
			int 0x21 

 horiz:
        push bp  
        mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push di
	push si
mov di, word[bp + 4]
mov ah, byte[bp + 6]
mov cx,3
loo:
sub di, 152
sub di, 160
mov ah, byte[bp + 6]
push ax
push di
call square
loop loo
        pop si
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
ret 4
displogo:
call clrscr
mov ax,0x22
push ax
mov di,1166
push di
call horiz
mov ax, 0x22
push ax
mov di, 1166
push di
call shape5
mov ax,0x22
push ax
mov di,2446
push di
call horiz
mov ax,0x22
push ax
mov di,1836
push di
call shape5
mov ax,0x22
push ax
mov di,1210
push di
call shape5
mov ax,0x22
push ax
mov di,1210
push di
call horiz
mov ax,0x22
push ax
mov di,1242
push di
call shape5
mov ax,0x22
push ax
mov di,2490
push di
call horiz
ret


 displogo2:
;first o
mov ax,0x44
push ax
mov di,1120
push di 
call horiz
mov ax,0x44
push ax
mov di,1120
push di
call shape5
mov ax,0x44
push ax
mov di,2400
push di
call horiz
mov ax,0x44
push ax
mov di,1152
push di 
call shape5

;second o
mov ax,0x44
push ax
mov di,1162
push di 
call horiz
mov ax,0x44
push ax
mov di,1162
push di
call shape5
mov ax,0x44
push ax
mov di,2442
push di
call horiz
mov ax,0x44
push ax
mov di,1194
push di 
call shape5

;p
mov ax,0x44
push ax
mov di,1204
push di 
call horiz
mov ax,0x44
push ax
mov di,1204
push di
call shape5
mov ax,0x44
push ax
mov di,2004
push di 
call shape5
mov ax,0x44
push ax
mov di,2324
push di 
call horiz
mov ax,0x44
push ax
mov di,1236
push di 
call shape5

;s
mov ax,0x44
push ax
mov di,1246
push di 
call horiz
mov ax,0x44
push ax
mov di,1246
push di
call shape5
mov ax,0x44
push ax
mov di,2206
push di 
call horiz
mov ax,0x44
push ax
mov di,2070
push di
call shape5
mov ax,0x44
push ax
mov di,3318
push di 
call horiz



mov ax, 3104	;di
push ax
mov ax, score	;6
push ax
mov ax, [len]	;4
push ax
call display
mov ax, 0x8f
push ax
mov ax, 3118
push ax
mov ax, [scored]
push ax
call printnummi

ret

beep:

     push ax
      push bx
      push cx
      push dx


      mov     al, 182         ; Prepare the speaker for the
      out     43h, al         ;  note.
      mov     ax, 9121        ; Frequency number (in decimal)
      ;  for middle C.
      out     42h, al         ; Output low byte.
      mov     al, ah          ; Output high byte.
      out     42h, al
      in      al, 61h         ; Turn on note (get value from
      ;  port 61h).
      or      al, 00000011b   ; Set bits 1 and 0.
      out     61h, al         ; Send new value.
      mov     bx, 25          ; Pause for duration of note.
      .pause1:
     mov     cx, 6535
      .pause2:
     dec     cx
      jne     .pause2
      dec     bx
      jne     .pause1
      in      al, 61h         ; Turn off note (get value from
      ;  port 61h).
      and     al, 11111100b   ; Reset bits 1 and 0.
      out     61h, al         ; Send new value.

      pop dx
      pop cx
      pop bx
      pop ax

      ret

printnum: 
 push es 
 
 mov ax, 0xb800 
 mov es, ax ; point es to video base 
 mov ax, word[min] ; load number in ax 
 mov bx, 10 ; use base 10 for division 
 mov cx, 0 ; initialize count of digits 
nextdigit: mov dx, 0 ; zero upper half of dividend 
 div bx ; divide by 10 
 add dl, 0x30 ; convert digit into ascii value 
 push dx ; save ascii value on stack 
 inc cx ; increment count of values 
 cmp ax, 0 ; is the quotient zero 
 jnz nextdigit ; if no divide it again 
 mov di, 772 ; point di to 70th column 
nextpos: pop dx ; remove a digit from the stack 
 mov dh, 0x07 ; use normal attribute 
 mov [es:di], dx ; print char on screen 
 add di, 2 ; move to next screen location 
 loop nextpos ; repeat for all digits on stack 

 mov dx, 0x073a
 mov [es:di], dx ; print colon on screen 
  add di, 2
 mov ax, word[sec1] ; load number in ax 
 mov bx, 10 ; use base 10 for division 
 mov cx, 0 ; initialize count of digits 
nextdigit2: mov dx, 0 ; zero upper half of dividend 
 div bx ; divide by 10 
 add dl, 0x30 ; convert digit into ascii value 
 push dx ; save ascii value on stack 
 inc cx ; increment count of values 
 cmp ax, 0 ; is the quotient zero 
 jnz nextdigit2 ; if no divide it again 
 
nextpos2: pop dx ; remove a digit from the stack 
 mov dh, 0x07 ; use normal attribute 
 mov [es:di], dx ; print char on screen 
 add di, 2 ; move to next screen location 
 loop nextpos2 ; repeat for all digits on stack 

 
 mov ax, word[sec2] ; load number in ax 
 mov bx, 10 ; use base 10 for division 
 mov cx, 0 ; initialize count of digits 
 mov dx, 0 ; zero upper half of dividend 
 div bx ; divide by 10 
 add dl, 0x30 ; convert digit into ascii value 
 push dx ; save ascii value on stack 
 inc cx ; increment count of values 
 ; is the quotient zero 
 ; if no divide it again 
 
pop dx ; remove a digit from the stack 
 mov dh, 0x07 ; use normal attribute 
 mov [es:di], dx ; print char on screen 
 add di, 2 ; move to next screen location 
 ; repeat for all digits on stack 

 pop es
 ret 

clrscr:
	push es
	push ax
	push di
	mov ax, 0xb800
	mov es, ax
	mov di, 0
	
	nextloc:
		mov word[es:di], 0x3320
		add di, 2
		cmp di, 4000
		
	jne nextloc
	
	pop di
	pop ax
	pop es
	
ret



box:
	
	
	mov ax, 0xb800
	mov es, ax
	mov cx, 37	;horizontal line 
	mov si,0
	mov di, 0
loop1:
	mov word[es:di], 0x5520	
	add di, 2
	inc si
	cmp cx, si
jne loop1
	

mov cx, 24	; vertical line on right and di is the same as above it will increment from tht
mov si, 0

loop2:
	mov word[es:di], 0x5520
	add di,160		;bcazz each space is printing in a new row 
	inc si
	cmp cx, si
jne loop2	

mov di,0
mov cx, 24			; vertical line on left 
mov si, 0

loop3:
	mov word[es:di], 0x5520
	add di, 160
	inc si
	cmp cx, si
jne loop3	

mov cx, 38	; another horizontal line on the botton,
mov si, 0

loop4:
	mov word[es:di], 0x5520
	add di, 2
	inc si
	cmp cx, si
jne loop4		
ret

 display: 
	push bp
	mov bp, sp
	push es
	push ax
	push cx
	push si
	push di


	mov ax, 0xb800
	mov es, ax
	mov di, [bp+8]		;di passed 
	mov si, [bp+6]
	mov cx, [bp+4]
	mov ah, 0x30
	mov al, [si]
	mov bx, 0
	
next:				
	
	mov word[es:di], ax
	add di, 2
	inc bx
	add si,1
	mov al, [si]
	cmp cx, bx
	
jne next	
	

	pop di
	pop si
	pop cx
	pop ax
	pop es
	pop bp
ret



shape1:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push di
	push si

	
	mov di, word[bp + 4]
	
mov cx,3

s1:
mov ah, byte[bp + 6]
push ax
push di
call square
loop s1


sub di, 152
sub di, 160
mov ah, byte[bp + 6]
push ax
push di
call square

	pop si
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
ret 4

shape2:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push di
	push si
	

	
	mov di, word[bp + 4]
	
	
mov cx,2

s2:
mov ah, byte[bp + 6]
push ax
push di
call square
sub di, 152
sub di, 160
mov ah, byte[bp + 6]
push ax
push di
call square

loop s2



	pop si
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
ret 4

shape3:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push di
	push si
	

	mov di, word[bp + 4]
	
mov ah, byte[bp + 6]
push ax
	push di
call square

mov cx,3
sub di, 8
s3:

mov ah, byte[bp + 6]
push ax
push di
call square
sub di, 152
sub di, 160
loop s3

	pop si
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
ret 4

shape4:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push di
	push si
	


	mov di, word[bp + 4]


mov cx,2

s4:

mov ah, byte[bp + 6]
push ax
push di
call square
loop s4
 
 sub di, 168
 sub di, 160
 mov cx, 2
sb4:

mov ah, byte[bp + 6]
push ax
push di
call square
loop sb4
	pop si
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
ret 4

shape5:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push di
	push si
	


	mov di, word[bp + 4]


mov cx,3

s5:

mov ah, byte[bp + 6]
push ax
push di
call square

loop s5
 


	pop si
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
ret 4
square:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push si
	
	mov ax, 0xb800
	mov es, ax
	mov di, word[bp + 4]
	mov ah, [bp + 6]
	mov al, 0x20
mov cx,0
mov bx, 2
sq:
	new:
	mov word[es:di], ax
	add di, 2
	inc cx
	cmp cx, 4
	jne new
add di, 152
mov cx,0
dec bx
cmp bx, 0
jne sq

	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
ret 4



delay:push cx
mov cx,20
delay_loop1:
push cx
mov cx,0xFFFF
delay_loop2:
loop delay_loop2
pop cx
loop delay_loop1
pop cx
ret

printnumm: push bp 
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
nextdigitt: mov dx, 0 ; zero upper half of dividend 
 div bx ; divide by 10 
 add dl, 0x30 ; convert digit into ascii value 
 push dx ; save ascii value on stack 
 inc cx ; increment count of values 
 cmp ax, 0 ; is the quotient zero 
 jnz nextdigitt ; if no divide it again 
 mov di, [bp+6]
 nextposs: pop dx ; remove a digit from the stack 
 mov dh, 0x030 ; use normal attribute 
 mov [es:di], dx ; print char on screen 
 add di, 2 ; move to next screen location 
 loop nextposs ; repeat for all digits on stack
 pop di 
 pop dx 
 pop cx 
 pop bx 
 pop ax 
 pop es 
 pop bp 
 ret 4

 printnummi: push bp 
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
nextdigittt: mov dx, 0 ; zero upper half of dividend 
 div bx ; divide by 10 
 add dl, 0x30 ; convert digit into ascii value 
 push dx ; save ascii value on stack 
 inc cx ; increment count of values 
 cmp ax, 0 ; is the quotient zero 
 jnz nextdigittt ; if no divide it again 
 mov di, [bp+6]
 nextposss: pop dx ; remove a digit from the stack 
 mov dh, [bp+8] ; use normal attribute 
 mov [es:di], dx ; print char on screen 
 add di, 2 ; move to next screen location 
 loop nextposss ; repeat for all digits on stack
 pop di 
 pop dx 
 pop cx 
 pop bx 
 pop ax 
 pop es 
 pop bp 
 ret 4
 scrolldown: push bp
mov bp,sp
push ax
push cx
push si
push di
push es
push ds

mov ax, 0xb800
mov es, ax ; point es to video base
mov ds, ax ; point ds to video base
mov di, 3682 ; point di to lower right column

add di, 76
mov si, di
sub si, 320
scrolling:
push di
push si
mov cx, 39 ;width of frame
std
rep movsw
pop si
pop di


sub di, 160
sub si, 160
cmp si, 320
jg scrolling


;first row
mov ax, 0x3320
mov di, 322
mov cx, 36
cld
rep stosw

pop ds
pop es
pop di
pop si
pop cx
pop ax
pop bp
ret 2

addscore:

add word[scored], 10
mov ax, 452
push ax
mov ax, [scored]
push ax
call printnumm
call scrolldown
ret

checkscore:
mov ax, 0xB800
mov es, ax

mov di, 3682

mov cx, 37
mov ax, 0x3320
repne scasw
cmp cx, 0
je addscore

call generaterandom


checkfull:
mov ax, 0xb800
mov es, ax
	mov di, 322
	mov ax, 0x3320
	mov cx, 35
	
	repe scasw
	
	cmp cx, 0
	
	jne near exit
	
	
ret




generaterandom:

call checkfull
 
mov ax, 0x22
push ax
mov di, 1720
push di
call shape1

call moveshape5
call delay
mov ax, 0x33
push ax
mov di, 1720
push di
call shape1

call checkfull
call delay
mov ax, 0x44
push ax
mov di, 1720
push di
call shape2
	call moveshape1
mov ax, 0x33
push ax
mov di, 1720
push di
call shape2
call delay

call checkfull

mov ax, 0x66
push ax
mov di, 1720
push di
call shape3

call moveshape2

mov ax, 0x33
push ax
mov di, 1720
push di
call shape3
call delay

call checkfull

mov ax, 0x11
push ax
mov di, 1720
push di
call shape4

call moveshape3

mov ax, 0x33
push ax
mov di, 1720
push di
call shape4

call checkfull

call delay
mov ax, 0x00
push ax
mov di, 1720
push di
call shape5

call moveshape4

call delay
mov ax, 0x33
push ax
mov di, 1720
push di
call shape5

call checkscore

ret 



start: 
call clrscr ; call clrscr subroutine 



call displogo
call delay
call delay
call delay
call clrscr
call box
mov ax ,2666	;di
push ax
mov ax, end	;6
push ax
mov ax, [len5]	;4
push ax
call display

mov ax, 440	;di
push ax
mov ax, score	;6
push ax
mov ax, [len]	;4
push ax
call display



mov ax, 760		;di
push ax
mov ax, time		;6
push ax
mov ax, [len3] 		;4
push ax
call display


mov ax ,1558	;di
push ax
mov ax, nshape	;6
push ax
mov ax, [len4]	;4
push ax
call display


	
xor ax, ax 
 mov es, ax ; point es to IVT base 
 mov ax, [es:9*4] 
 mov [oldisr], ax ; save offset of old routine 
 mov ax, [es:9*4+2] 
 mov [oldisr+2], ax ; save segment of old routine 

 mov ax, word [es:8*4]
 mov  [oldisr2], ax ; save offset of old routine
 mov bx, [es:8*4+2]
mov  [oldisr2+2], bx ; save segment of old routine


 cli ; disable interrupts 
 mov word [es:9*4], kbisr ; store offset at n*4 
 mov [es:9*4+2], cs ; store segment at n*4+2 

  mov word [es:8*4], timer; store offset at n*4 
 mov [es:8*4+2], cs ; store segment at n*4+2 
  sti ; enable interrupts 


  mov cx, 10

  play:
call generaterandom

loop play





exit:

call clrscr
call displogo2



xor ax, ax 
 mov es, ax ; point es to IVT base 
 
 
 cli ; disable interrupts 
  mov ax, [oldisr2]
 mov [es:8*4], ax ; restore old offset from ax 
 mov ax, [oldisr2+2]
 mov [es:8*4+2], ax ;
 mov ax, [oldisr]
 mov [es:9*4], ax ; restore old offset from ax 
 mov ax, [oldisr+2]
 mov [es:9*4+2], ax ; restore old segment from bx 
sti

mov ah,0x1 ; special intrupt to not show the cursor
int 0x21

 mov ax, 0x4c00 ; terminate program 
 int 0x21 

moveshape1:
mov word[moveflag], 1
		                ;moving shape1

mov di, 334	;di
mov word[cs: current], di


move:
mov ax, 0x22
push ax
mov  di, [cs: current]
push di
call shape1
cmp word[left], 1
je moveleft1
cmp word[right], 1
je moveright1
add di, 1120
mov cx, 8
mov ax, 0x3320
repe scasw
cmp cx, 0
jne stop1

call delay
mov ax, 0x33
push ax
mov  di, [cs: current]
push di
call shape1
add di, 320
mov   [cs: current], di

cmp word[moveflag], 0
jne move

stop1:
call beep
mov ax, 0x22
push ax
mov  di, [cs: current]
push di
call shape1

ret

moveleft1:
mov ax, 0x33
push ax
mov  di, [cs: current]
push di
call shape1
sub word[current], 2
mov word[left], 0
 jmp move

 moveright1:
mov ax, 0x33
push ax
mov  di, [cs: current]
push di
call shape1
add word[current], 2
mov word[right], 0
 jmp move






moveshape2:
mov word[moveflag], 1
				;moving shape2

mov di, 342
mov word[current], di
move2:

mov ax, 0x44
push ax
mov  di, [cs: current]
push di
call shape2

cmp word[left], 1
je moveleft2
cmp word[right], 1
je moveright2

add di, 324
mov cx, 2
mov ax, 0x3320
repe scasw
cmp cx, 0
jne stop2
call delay
mov di, word[current]
add di, 808
mov cx, 5
mov ax, 0x3320
repe scasw
cmp cx, 0
jne stop2


mov ax, 0x33
push ax
mov  di, [cs: current]
push di
call shape2
add di, 320
mov  word[current], di
cmp word[moveflag], 1
je move2

stop2:
call beep
mov word[moveflag], 0
mov ax, 0x44
push ax
mov  di, [cs: current]
push di
call shape2
ret

moveleft2:
mov ax, 0x33
push ax
mov  di, [cs: current]
push di
call shape2
sub word[current], 2
mov word[left], 0
 jmp move2

 moveright2:
mov ax, 0x33
push ax
mov  di, [cs: current]
push di
call shape2
add word[current], 2
mov word[right], 0
 jmp move2



moveshape3:
				;shape 3
mov word[moveflag], 1
mov di, 372
mov word[current], di
move3:

mov ax, 0x66
push ax
mov  di, [cs: current]
push di
call shape3

cmp word[left], 1
je moveleft3
cmp word[right], 1
je moveright3

add di, 796
mov cx, 10
mov ax, 0x3320
repe scasw
cmp cx, 0
jne stop3
mov ax, 0x66
push ax
mov  di, [cs: current]
push di
call shape3
call delay
mov ax, 0x33
push ax
mov  di, [cs: current]
push di
call shape3
add word[current], 320
mov  di, [cs: current]
cmp word[moveflag], 0
jne move3
stop3:
call beep
mov ax, 0x66
push ax
mov  di, [cs: current]
push di
call shape3
ret
moveleft3:
mov ax, 0x33
push ax
mov  di, [cs: current]
push di
call shape3
sub word[current], 2
mov word[left], 0
 jmp move3

 moveright3:
mov ax, 0x33
push ax
mov  di, [cs: current]
push di
call shape3
add word[current], 2
mov word[right], 0
 jmp move3


moveshape4:
				;shape 4
mov word[moveflag], 1
mov di, 370
mov word[current], di

move4:

mov ax, 0x11
push ax
mov  di, [cs: current]
push di
call shape4

cmp word[left], 1
je moveleft4
cmp word[right], 1
je near moveright4

add di, 644
mov cx, 3
mov ax, 0x3320
repe scasw
cmp cx, 0
jne stop4
mov di, word[current]
add di, 958
mov cx, 2
mov ax, 0x3320
repe scasw
cmp cx, 0
jne stop4

call delay
mov ax, 0x33
push ax
mov  di, [cs: current]
push di
call shape4
add word[current], 320
mov  di, [cs: current]
cmp word[moveflag], 0
jne move4

stop4:
call beep
cmp word[moveflag], 0
mov ax, 0x11
push ax
mov  di, [cs: current]
push di
call shape4

ret
moveleft4:
mov ax, 0x33
push ax
mov  di, [cs: current]
push di
call shape4
sub word[current], 2
mov word[left], 0
 jmp move4

 moveright4:
mov ax, 0x33
push ax
mov  di, [cs: current]
push di
call shape4
add word[current], 2
mov word[right], 0
 jmp move4




 
moveshape5:

		mov word[moveflag], 1		;shape 5

mov di, 326
mov word[current], di

move5:

mov ax, 0x00
push ax
mov  di, [cs: current]
push di
call shape5


cmp word[left], 1
je moveleft5
cmp word[right], 1
je moveright5

add di, 1120
mov cx, 8
mov ax, 0x3320
repe scasw
cmp cx, 0
jne stop5
call delay
mov ax, 0x33
push ax
mov  di, [cs: current]
push di
call shape5
add word[current], 320
cmp word[moveflag], 0
jne move5
stop5:
call beep
mov ax, 0x00
push ax
mov  di, [cs: current]
push di
call shape5

ret 

moveleft5:
mov ax, 0x33
push ax
mov  di, [cs: current]
push di
call shape5
sub word[current], 2
mov word[left], 0
 jmp move5

 moveright5:
mov ax, 0x33
push ax
mov  di, [cs: current]
push di
call shape5
add word[current], 2
mov word[right], 0
 jmp move5

 



