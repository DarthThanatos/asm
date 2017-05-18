; to compile in cmd:
; ml /Bllink16 UCR_lib.asm

.xlist
	include stdlib.a
	includelib stdlib.lib
.list

.386
option segment:use16

print_decimal proto c number: word 

argument struct 
	filename byte 25 dup("$")
	value byte 33 dup("$")
	next_segment word ? ;offset is 0
	file_length word ?
	index word 0
argument ends ;64 bytes block 

startup macro 
	mov ax, dseg
	mov ds, ax
	;make PSP size bigger 
	mov bx, 100h
	mov ah, 4ah
	int 21h
endm 

print_string macro xx
	pusha 
	mov dx, offset xx 
	mov ah, 9 
	int 21h 
	popa 
endm

printf macro xx 
	pusha 
	mov dx, xx 
	mov ah, 9 
	int 21h 
	popa
endm 

print_char macro xx 
	pusha 
	mov dl, xx 
	mov ah, 2 
	int 21h 
	popa
endm 

Quit macro           
	mov ah, 04ch
	int 21h
endm 

dseg segment para public 'data'
dseg ends

cseg segment para public 'code'
	assume  cs:cseg, ds:dseg

	Main proc
		startup
		call abc
		invoke print_decimal, 264
		Quit
	Main endp

	abc proc
		mov cx, 5 
		xor ax, ax
		@@: 
			mov al, 'a'
			putc
		loop @B
		ret 
	abc endp 

	print_decimal proc c number: word 
		local counter: word
		mov counter,0 
		pusha  
		mov ax, number
		;abs:
		shrd ax,ax,15 ;mask = n >> 15 
		mov bx,ax 
		mov dx, number
		xor ax,bx 
		sub ax,dx 
		js positive_number ;sign turned on due to operation -> positive before
		cmp number,0 ;if 0 do not print "-"
		jz @F
		print_char '-'
		jmp @F
		positive_number:
			mov ax, number 
			@@: 
				mov bx,0Ah
				xor dx, dx 
				cmp ax, 0 
				jz @F 
				div bx
				;dx = reminder, ax = quotient 
				push dx
				inc counter
			jmp @B 
			@@:
				mov cx, counter
				cmp cx,0 
				jz print_zero
			@@:
				pop dx 
				add dx, '0'
				print_char dl 
			loop @B
			jmp print_decimal_ret
			print_zero:
				print_char '0'
			print_decimal_ret:
				print_char ' '
				popa
				ret  
	print_decimal endp
cseg ends

sseg segment para stack 'stack'
	stk db 1024 dup (?)
sseg ends

zzzzzzseg segment para public 'zzzzzz'
	LastBytes db 16 dup (?)
zzzzzzseg ends

end Main