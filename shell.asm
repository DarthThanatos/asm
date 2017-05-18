; to compile in cmd:
; ml /Bllink16 shell.asm

.xlist
	include stdlib.a
	includelib stdlib.lib
.list

print_string macro xx
	push ax 
	push dx 
	mov dx, offset xx 
	mov ah, 9 
	int 21h 
	pop dx 
	pop ax 
endm

end_word_properly macro 
	nop
	nop
	nop 
	push es 
	push si 
	push di 
	mov es, head_segment
	mov si, head_offset
	add si,di
	mov byte ptr es:[si],10
	mov byte ptr es:[si + 1],13
	mov byte ptr es:[si + 2],"$"
	pop di 
	pop si 
	pop es 
endm 

argument struct 
	value byte 50 dup (?)
	next_segment word 0
	next_offset word 0
argument ends 

dseg segment para public 'data'
	head_offset word 0
	head_segment word 0
	p_segment word 0
	p_offset word 0 
	word_offset word 0
	outro db "exiting...", 10, 13, "$"
dseg ends

cseg segment para public 'code'
	assume  cs:cseg, ds:dseg

	Main proc
		mov ax, dseg
		mov ds, ax
		meminit 
		call print_cmd
		call print_list
		print_string outro 
		Quit:           
			ExitPgm
	Main endp

	print_list proc 
		mov ax, head_segment
		mov p_segment,ax 
		mov ax, head_offset
		mov p_offset, ax 
		iterate:
			mov ax, word ptr p_segment 
			mov bx, word ptr p_offset
			add ax, bx
			cmp ax, 0
			jz NULL_PTR 
			mov di, word ptr p_offset
			mov es, word ptr p_segment
			print_argument:
				mov al, byte ptr es:[di]
				cmp al,"$"
				jz after_printing
				putc 
				inc di
			jmp print_argument
			after_printing:
				mov di, p_offset
				mov bx, word ptr es:[di + 50]
				mov p_segment, bx 
				mov bx, word ptr es:[di + 50 + 2]
				mov p_offset, bx 
		jmp iterate
		NULL_PTR: 
			mov ax, seg dseg 
			mov ds, ax 
			ret 
	print_list endp

	print_cmd proc
		push cx 
		push es 
		push di 
		push si 
		push bx 
		xor bx, bx 
		xor cx, cx 
		mov si, -1  
		mov cl, [es:80h]
		cmp cl, 0
		jz return
		jmp create_list 
		dispatch_string:
			mov al, byte ptr [es:81h + 1 + si]
			cmp al, ' ' 
			jz create_list
			push si 
			push es
			mov es, head_segment
			mov si, head_offset
			add si,di 
			mov byte ptr es:[si], al 
			pop es 
			pop si  
			inc si
			inc di 
		loop dispatch_string
		jmp return 
		create_list:
			push cx
			push ax 
			push es 
			call new 
			xor di,di
			inc si 
			pop es 
			pop ax 
			pop cx 
			dec cx 
		jmp dispatch_string
		return:
			end_word_properly ; last string must be ended as well 
			pop bx 
			pop si 
			pop di 
			pop es 
			pop cx 
			ret 
	print_cmd endp 

	new proc 
		cmp si, -1
		jz allocate
		;properly end the previous string 
		end_word_properly
		allocate:
			mov cx, sizeof argument 
			malloc
			mov ax, head_segment
			mov word ptr es:[di + 50], ax 
			mov ax, head_offset
			mov word ptr es:[di + 50 + 2], ax 
			mov ax, es 
			mov head_segment,ax 
			mov ax, di 
			mov head_offset, ax
			ret 
	new endp 
cseg ends

sseg segment para stack 'stack'
	stk db 1024 dup (?)
sseg ends

zzzzzzseg segment para public 'zzzzzz'
	LastBytes db 16 dup (?)
zzzzzzseg ends

end Main