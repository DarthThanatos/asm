; to compile in cmd:
; ml /Bllink16 strcat.asm

.model small, c 
.386

read proto file_segment: word
print_decimal proto number: word 

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
	open_error_msg byte 'Open: ', 10, 13, "$"
	read_error_msg byte 'Read: ', 10, 13, "$"
	close_error_msg byte 'Close: ', 10, 13, "$"
	malloc_error_msg byte 'Malloc: ', 10, 13, "$" 
	hex db "0123456789ABCDEF"
	file_error_msg byte 'Procedure ended with error code: '
	key db ?,?,"h",13,10,"$"
	head word 0
	tail word 0
	p word 0
	argc byte 0
dseg ends

cseg segment para public 'code'
	assume  cs:cseg, ds:dseg

	Main proc
		startup
		call get_files_names
		call open_files 
		call run_concatenation
		Quit
	Main endp

	run_concatenation proc 
		local files_served : byte 
		mov files_served,0 
		pusha
		push es 
		while_loop_outer:
			mov al, argc 
			cmp files_served, al  
			jz run_concatenation_ret
			mov ax, head 
			mov p, ax 
			for_loop:
				cmp p,0 
				jz while_loop_outer
				mov es, p 
				while_loop_inner:
					check_if_eof:
						mov ax, word ptr [es: 25d + 33d + 2d + 2d]
						cmp word ptr [es: 25d + 33d + 2d], ax 
						;if p->index == p->file_length 
						jz after_inner_while
					check_if_file_was_served:
						mov ax, word ptr [es: 25d + 33d + 2d]
						dec ax ;p->file_length - 1 
						cmp word ptr [es: 25d + 33d + 2d + 2d], ax 
						;if p->index != p->file_length - 1
						jnz check_if_eol
						inc files_served
					check_if_eol:
						mov di, [es:25d + 33d + 2d + 2d] ;p->index 
						mov al, byte ptr [es:25d + di] ;al = p->value[p->index]
						cmp al, 13 ;if(p->value[p->index] == '\n')
						jnz char_on_screen
						add word ptr[es: 25d + 33d + 2d + 2d], 2 
						;omiting 10 and 13 characters
						mov ax, word ptr [es: 25d + 33d + 2d + 2d]
						cmp word ptr [es: 25d + 33d + 2d], ax ;if p->index != p->file_length
						jnz after_inner_while
						inc files_served
						;if it's an end we must increment files_served before jumping
						jmp after_inner_while ;break
					char_on_screen:
						print_char al
						inc word ptr [es:25d + 33d + 2d + 2d]
				jmp while_loop_inner
				after_inner_while:
					mov ax, [es: 25d +33d] ;p->next 
					cmp ax, 0 
					jz print_new_line
					print_colon:
						print_char ":"
						jmp next_node
					print_new_line:
						print_char 10
						print_char 13 
					next_node:
						mov ax, word ptr [es:25d + 33d]
						mov p, ax 
			jmp for_loop
		jmp while_loop_outer 
		run_concatenation_ret:
			pop es 
			popa 
			ret 
	run_concatenation endp 

	print_decimal proc number: word 
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
				print_char 10 
				print_char 13 
				popa
				ret  
	print_decimal endp

	iterate_through_list proc 
		pusha 
		push ds 
		mov ax, head 
		mov p, ax 
		@@:
			cmp p,0 
			jz @F 
			mov ax, p 
			mov ds,ax 
			mov dx, 0 
			mov ah, 9
			int 21h 
			print_char 10 
			print_char 13
			mov dx, 25d 
			int 21h 
			print_char 10 
			print_char 13
			invoke print_decimal, word ptr [ds: 25d + 33d + 2d]
			invoke print_decimal, word ptr [ds: 25d + 33d + 2d + 2d]
			mov ax, word ptr [ds:25d + 33d]
			mov p,ax 
		loop @B
		@@:
			pop ds 
			popa 
			ret 
	iterate_through_list endp  

	open_files proc 
		pusha 
		push es 
		mov ax, head 
		mov p, ax 
		@@:
			mov es,p
			cmp p, 0
			jz @F
			invoke read, p 
			mov di, 25d + 33d
			mov ax, word ptr es:[di]  
			mov p,ax ;p = p->next
		jmp @B
		@@:
			pop es 
			popa 
			ret 
	open_files endp

	malloc proc 
		pusha
		push es
		mov ah, 48h 
		mov bx, 4h ;4 * 16 bytes 
		int 21h 
		jc malloc_error
		mov es, ax 
		mov di, 25d + 33d 
		mov word ptr es:[di], 0 ;p->next = NULL
		mov word ptr [es: 25d + 33d + 2d + 2d],0
		cmp head,0
		jnz head_not_null
		head_null:
			mov head, ax
			mov tail, ax 
			jmp @F
		head_not_null:
			mov es, tail 
			mov di, 25d + 33d 
			mov word ptr es:[di],ax ;tail->next = p
			mov tail,ax ;tail = p
			jmp @F
		malloc_error:
			print_string malloc 
			call display_error
			Quit
		@@:
			pop es 
			popa
			ret 
	malloc endp 

	read proc file_segment: word 
		local handle: word
		pusha 
		push ds 
		mov ax, file_segment
		mov ds, ax 
		mov dx, 0d 
		mov ah, 3dh
		mov al, 0
		int 21h
		mov handle, ax 
		jc open_error
		mov ah,3fh 
		mov dx, 25d  
		mov cx, 33d 
		mov bx, handle 
		int 21h 
		jc read_error
		mov di, 25d + 33d + 2d ;arg->file_length 
		mov word ptr ds:[di], ax
		mov di, ax 
		mov byte ptr [ds:25d + di], "$" ;end buffer with $ so it can be read 
		mov bx, handle 
		mov ah, 3eh
		int 21h 
		jc close_error
		pop ds 
		jmp @F
		open_error:
			pop ds 
			print_string open_error_msg
			jmp file_error
		read_error:
			pop ds 
			print_string read_error_msg
			jmp file_error
		close_error:
			pop ds 
			print_string close_error_msg
			jmp file_error
		file_error: 
			push ds 
			push ax 
			mov ax, file_segment
			mov ds, ax
			printf 0d 
			print_char ':'
			print_char ' '
			pop ax 
			pop ds 
			call display_error
			pop ds 
			popa
			Quit 
		@@:
			popa 
			ret 
	read endp 

	display_error proc  
		pusha 
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
		print_string file_error_msg
		popa 
		ret
	display_error endp

	get_files_names proc 
		pusha
		push es
		xor cx, cx 
		mov cl, byte ptr [es:80h]
		cmp cl, 0 
		jz get_files_names_ret
		xor si,si 
		eliminate_spaces:
			mov al, byte ptr [es:81h + si]
			cmp al, ' '
			jnz argument_parsing
			inc si 
		loop eliminate_spaces
		jmp get_files_names_ret
		argument_parsing:
			inc argc 
			call malloc 
			xor di,di 
			@@:
				mov al, byte ptr [es:81h + si]
				cmp al, ' '
				jz end_word
				push es 
				mov es, tail
				mov byte ptr es:[di],al
				pop es 
				inc si 
				inc di 
			loop @B
			end_word:
				push es 
				mov es, tail 
				mov byte ptr es:[di], 0 
				mov byte ptr es:[di+1], "$"
				pop es 
				cmp cx,0 
				jnz eliminate_spaces
		get_files_names_ret:
			pop es 
			popa
			ret 
	get_files_names endp 
cseg ends

sseg segment para stack 'stack'
	stk db 1024 dup (?)
sseg ends

zzzzzzseg segment para public 'zzzzzz'
	LastBytes db 16 dup (?)
zzzzzzseg ends

end Main