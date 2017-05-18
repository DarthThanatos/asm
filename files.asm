; to compile in cmd:
; ml /Bllink16 files.asm

.model small, c 
.386

read proto file_name: ptr byte, buffer: ptr byte  
malloc proto block_size: word 

argument struct 
	filename byte 25 dup(?)
	value byte 100 dup(?)
	next_segment word ? ;offset is 0
	file_length word ?
	index word ?
argument ends 

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

get_file_name_x macro  xx
	mov al, byte ptr [es:81h + si]
	cmp al, ' '
	jz @F
	mov byte ptr ds: [offset xx + di], al 
	inc di 
	inc si 
endm 

Quit macro           
	mov ah, 04ch
	int 21h
endm 

end_file_name macro  xx
	mov byte ptr ds: [offset xx + di], 0 
	comment ^
	mov byte ptr ds: [offset xx + di + 1], 10 
	mov byte ptr ds: [offset xx + di + 2], 13 
	^
	mov byte ptr ds: [offset xx + di + 3], "$"
endm 

eradicate_spaces_x macro xx
	mov al, byte ptr [es:81h + si]
	cmp al, ' '
	jnz xx
	inc si 
endm 

dseg segment para public 'data'
	hello byte 'hello world', 10 ,13, "$"
	no_args byte 'Wrong usage: concatenate.exe file_name_one file_name_two; do not use tabs and other white characters but spaces',10,13,"$"
	open_error_msg byte 'Open: ', 10, 13, "$"
	read_error_msg byte 'Read: ', 10, 13, "$"
	close_error_msg byte 'Close: ', 10, 13, "$"
	malloc_error_msg byte 'Malloc: ', 10, 13, "$" 
	file_name_one byte 25 dup(?)
	file_name_two byte 25 dup(?)
	file_content_one byte 101 dup("$")
	file_content_two byte 101 dup("$")
	hex db "0123456789ABCDEF"
	file_error_msg byte 'Procedure ended with error code: '
	key db ?,?,"h",13,10,"$"
dseg ends

cseg segment para public 'code'
	assume  cs:cseg, ds:dseg

	Main proc
		mov ax, dseg
		mov ds, ax
		call get_files_names 
		invoke read, offset file_name_one, offset file_content_one
		invoke read, offset file_name_two, offset file_content_two
		print_string file_name_one
		print_char 10d 
		print_char 13d 
		print_string file_content_one
		print_char 10d 
		print_char 13d 
		print_string file_name_two
		print_char 10d 
		print_char 13d 
		print_string file_content_two 
		Quit
	Main endp

	create_list proc 
		pusha 
		
		popa
		ret 
	create_list endp 

	malloc proc block_size: word 
		pusha 
		mov ah, 48h 
		mov bx, block_size
		int 21h 
		jc malloc_error
		jmp @F
		malloc_error:
			print_string malloc 
			call display_error
			Quit
		@@:
			popa 
			ret 
	malloc endp 

	read proc file_name: ptr byte, buffer: ptr byte 
		local handle: word
		pusha 
		mov dx, file_name
		mov ah, 3dh
		mov al, 0
		int 21h
		mov handle, ax 
		jc open_error
		mov ah,3fh 
		mov dx, buffer 
		mov cx, 100d 
		mov bx, handle 
		int 21h 
		jc read_error
		mov bx, handle 
		mov ah, 3eh
		int 21h 
		jc close_error
		jmp @F
		open_error:
			print_string open_error_msg
			jmp file_error
		read_error:
			print_string read_error_msg
			jmp file_error
		close_error:
			print_string close_error_msg
			jmp file_error
		file_error:
			printf file_name
			print_char ':'
			print_char ' ' 
			call display_error
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
		xor cx,cx 
		mov cl, [es:80h]
		cmp cl, 0
		jz error_handler 
		xor si,si
		eradicate_spaces_at_beginning:
			eradicate_spaces_x @F
		loop eradicate_spaces_at_beginning
		jmp error_handler ;nothing but spaces - error 
		@@: xor di,di 
		get_file_name_one:
			get_file_name_x file_name_one
		loop get_file_name_one
		jmp error_handler ;loop finished and another file's name had not been loaded
		@@: end_file_name file_name_one
		eradicate_spaces_between:
			eradicate_spaces_x @F
		loop eradicate_spaces_between
		jmp error_handler ;loop finished and another file's name had not been loaded
		@@: xor di, di 
		get_file_name_two:
			get_file_name_x file_name_two
		loop get_file_name_two
		@@: 
			end_file_name file_name_two
			cmp cx,0 
			jz return_from_init
		check_if_nothing_more:
			eradicate_spaces_x error_handler
		loop check_if_nothing_more
		jmp return_from_init ;loop finished an other symbols than spaces not detected, can proceed
		error_handler:
			print_string no_args
			Quit
		return_from_init:
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