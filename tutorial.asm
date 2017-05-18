; to compile in cmd:
; ml /Bllink16 tutorial.asm
.MODEL small, c 
.386

.xlist
	include         stdlib.a
	includelib      stdlib.lib
.list

argument struct 
	value byte ?
	next typedef ptr argument 
argument ends 


print_string macro xx  

	Comment ^ 
	makro wypisuje ciag znakow
	z wybranego miejsca w pamieci 
	reprezentowanego przez etykiete ^

	pusha 
	lea dx,xx
	mov ah,9
	int 21h
	popa 
endm

printf macro xx  ; makro wypisuje ciag znakow z wybranego miejsca w pamieci
	pusha 
	mov dx,xx
	mov ah,9
	int 21h
	popa 
endm

print_character macro xx 
	pusha 
	mov dl, xx 
	mov ah, 2 
	int 21h 
	popa 
endm 

print_command_line macro  
	xor cx, cx 
	xor di, di 
	mov cl, [es:80h]
	cmp cl, 0 ; in case
	jz @F     ; command line is empty

	continue:
		print_character [di + es:81h]
		inc di
	loop continue
	print_character 0Ah
	@@:
		popa  
endm 

startUp macro 
	mov ax, seg data_segment
	mov ds, ax
	mov ax, seg stack_segment
	mov ss, ax 
endm

compare_strings proto xx: ptr byte, yy: ptr byte
light_diodes proto diode_number: byte 

data_segment segment para public 'data'
	intro db 'plz type sth: ',13,10,"$"
	hex db "0123456789ABCDEF"
	string_one byte "the quicker",13,10,"$"
	string_two byte "the quicker",13,10,"$"
	matched db "words match",13,10,"$"
	no_matched db "words do not match",13,10,"$"
	capslock_on db "CAPSLOCK_ON",13,10,"$"
	capslock_off db "CAPSLOCK_OFF",13,10,"$"
	convert db "you pressed the key with ascii code "
	key db ?,?,"h",13,10,"$"
	word_length word 0
	header typedef ptr argument
data_segment ends 

code_segment segment para public 'code'
	assume  cs:code_segment, ds:data_segment
	Main proc 
		startUp
		print_command_line
		invoke compare_strings, offset string_one, offset string_two
		print_string intro 
		call translate
		ExitPgm
	Main endp 

	Return:
		mov ah,04ch
		int 21h

	Print_Enter:
		mov dl,10
		mov ah,2
		int 21h
		ret

	translate:
		call my_scanf 
		mov bx, offset hex
		mov ah, al ; ah << al
		and al, 00001111b ; e.g. 41h - first goes 1, 4 is masked
		xlat 
		mov key[1], al 
		mov cl, 12 
		shr ax, cl 
		; ah >> al - that means 4 goes in a place 
		; where originally 1 stood at
		xlat 
		mov key, al 
		print_string convert
		ret
		
	my_scanf:
		mov ah, 8
		int 21h ;get character from keyboard
		pusha ;save everything
		mov dl, al 
		mov ah,2
		int 21h ; display what was typed
		call Print_Enter
		popa ;restore everything 
		ret

	get_length proc xx: ptr byte 
		pusha 
		mov word_length, 0 
		xor di,di 
		mov bx, xx
		mov al, '$'
		increment:
			cmp al, [di + bx]
			jz get_length_end
			inc di
			inc word_length
		jmp increment 
		get_length_end: 
			popa 
			ret 
	get_length endp

	compare_strings proc near xx: ptr byte, yy: ptr byte ;arguments are offsets of strings
		pusha
		mov ax, seg data_segment ;load data segment
		mov ds, ax 
		mov es, ax
		invoke get_length, xx
		mov cx, word_length
		cld ;work upwards
		mov si, xx 
		mov di, yy 
		repe cmpsb
		je all_match
		no_match:
			print_string no_matched
			jmp @F
		all_match:
			print_string matched
		@@:
			popa 
			ret 
	compare_strings endp

	delay proc 
		pusha 
		mov ah, 86h 
		mov cx, 0Fh 
		mov dx, 0
		int 15h
		popa 
		ret
	delay endp 

	light_diodes proc diode_number : byte
		pusha
		xor ax,ax 
		mov cx, 3
		@@:    
            call check_keyboard
			mov al, 0edh
			out 60h,al 
			xor al,al
			mov al, diode_number 
			out 60h,al 
			print_string capslock_on
			;wait
			call delay
			call check_keyboard
			mov al, 0edh
			out 60h,al 
			xor al,al
			out 60h,al
			print_string capslock_off
			call delay
		loop @B
		popa  
		ret
	light_diodes endp 

	check_keyboard proc 
		push ax 
		@@:
			in al, 64h
			and al,2
		jnz @B
		pop ax
		ret
	check_keyboard endp

code_segment ends 

stack_segment segment para stack 'stack'
	stk db 1024 dup ("stack   ")
stack_segment ends

last_segment segment para public 'zzzzzz'
	LastBytes db 16 dup (?)
last_segment ends

END Main   