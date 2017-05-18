; to compile in cmd:
; ml /Bllink16 orders.asm

.xlist
	include stdlib.a
	includelib stdlib.lib
.list

option segment: use16
.386 

image_width equ 128d 
image_height equ 128d 
graphic_mode_width equ 320d 
graphic_mode_height equ 200d
file_name_length equ 10d 
commands_buffer_size equ 150d
argument_buffer_size equ 6d 
line_color equ 150d
tortoise_length equ 60d

print_decimal proto c number: word 
load_data proto c name_offset_of_file: word, segment_for_data: word, size_of_input: word 
prepare_array proto c string_length:word, string_offset:word, string_segment: word 
recognize_command proto c string_length: word, cx_value: word, si_value: word 
move_to_eol proto c cx_value: word, si_value: word 
parse_single_arg proto c cx_value:word, si_value:word
parse_no_args proto c cx_value:word, si_value:word 
set_cx_to_max proto c num_one: word, num_two: word 
my_atoi proto c string_segment: word, string_offset: word
FPU_rotation proto c angle: word 
convert_and_push proto c value: word 
set_di_at_xy proto c x:word, y:word, seg_width_first_part: byte, seg_width_second_part: byte
paint_tortoise proto c image_segment:word

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
	hello byte 'hello world', 10 ,13, "$"
	file_with_picture byte 'rotated.txt',0,"$"
	file_with_orders byte file_name_length dup("$")
	open_error_msg byte 'Open: ', 10, 13, "$"
	read_error_msg byte 'Read: ', 10, 13, "$"
	close_error_msg byte 'Close: ', 10, 13, "$"
	name_too_long_msg byte 'Name of the file containing the orders is too long', 10,13, "$"
	input_error_msg byte 'Wrong usage: orders.exe file_name', 10, 13, "$"
	hex db "0123456789ABCDEF"
	file_error_msg byte 'Procedure ended with error code: '
	key db ?,?,"h",13,10,"$"
	command byte 8 dup("$")
	argument byte argument_buffer_size dup ("$")
	rotate byte 'rotate', "$"
	move byte 'move', "$"
	penup byte 'penup', "$"
	pendown byte 'pendown',"$"
	not_recognized_msg byte ' not recognized', 10, 13, "$"
	line_number word 1
	error_msg_one_arg byte 'wrong usage: one decimal int argument required', 10, 13, "$"
	error_msg_no_arg byte 'wrong usage: this command does not take any arguments', 10, 13, "$"
	recognized_msg byte ' recognized', 10,13,"$"
	line_info byte "line: ","$"
	atoi_invalid_arg byte "invalid, non-digit argument", 10, 13, "$"
	flag byte 0
	pen_flag byte 1
	error_msg_command_too_long byte 'command too long', 10, 13, "$"
	error_msg_argument_too_long byte 'argument too long - overflow', 10, 13, "$"
	rotating_info byte 'wait, rotating the image...',10,13,"$"
	float real8 1.0
	global_angle word 270d 
	left_corner_x word (graphic_mode_width/2 - image_width/2)
	left_corner_y word (graphic_mode_height/2 - image_height/2)
	info byte 'TYPE ANY CHARACTER TO PROCEED', 10, 13, "$"
	pendown_info byte 'PENDOWN - PEN ON',10,13,"$"
	penup_info byte 'PENUP - PEN OFF',10,13,"$"
	move_info byte 'MOVING BY ',"$"
	rotate_info_one byte 'ROTATION ANGLE: ', "$"
	rotate_info_two byte 'GLOBAL ANGLE: ', "$"
dseg ends

ImageSegment segment para public 'image_data'
	Image byte image_height * image_width + 1 dup (?)
	image_bytes_read word ?
ImageSegment ends

RotatedImageSegment segment para public 'rot_image_data'
	RotatedImage byte image_height * image_width + 1 dup (0)
	rotated_image_bytes_read word ?
RotatedImageSegment ends

OrdersSegment segment para public 'orders_data'
	Commands byte commands_buffer_size + 1 dup(?)
	orders_bytes_read word ?
OrdersSegment ends

PaintedLinesSegment segment para public 'lines'
	Lines byte graphic_mode_height*graphic_mode_width + 1 dup(0)
	lines_bytes_read word ?
PaintedLinesSegment ends

cseg segment para public 'code'
	assume  cs:cseg, ds:dseg

	Main proc c
		mov ax, dseg
		mov ds, ax
		call get_file_name
		invoke prepare_array, 50, 0, seg OrdersSegment; prepare OrdersSegment
		invoke load_data, offset file_with_picture, seg ImageSegment,  image_width * image_height
		invoke load_data, offset file_with_orders, seg OrdersSegment, commands_buffer_size
		invoke paint_tortoise, seg ImageSegment
		call turn_graphics_off
		call parse_commands
		Quit 
	Main endp

	turn_speaker_on proc 
		in al, 61h
		or al,3
		out 61h, al 
		ret 
	turn_speaker_on endp 

	turn_graphics_off proc 
		printf offset info
		mov ah,8
		int 21h ;get character
		mov ah,0 
		mov al,3 
		int 10h ;turn write(text) mode on 
		ret
	turn_graphics_off endp 
	
	FPU_move proc c 
		pusha
		push ds 
		push es 
		mov ax, seg dseg 
		mov ds, ax 
		mov flag, 0 
		finit
		prep_x:
			invoke convert_and_push, global_angle
			fldpi 
			fmul ; pop st(0), pop st(1), push st(0) = angle (st(1)) * pi (st(0))
			invoke convert_and_push, 180
			fdiv; pop st(0), pop st(1), push st(0) = [angle * pi](st(1))/180 (st(0))
			fsincos 
			invoke convert_and_push,2 ;r = 1
			fmul st(0), st(1)
			frndint 
			call convert_and_pop
			add left_corner_x,ax 
		check_x_lt0:
			cmp left_corner_x, 0 
			jge check_x_gt_right_border
			sub left_corner_x, ax 
			jmp FPU_move_out_of_bound
		check_x_gt_right_border:
			cmp left_corner_x, graphic_mode_width - image_width 
			jle prep_y
			sub left_corner_x,ax
			jmp FPU_move_out_of_bound
		prep_y:
			invoke convert_and_push,2 
			fmul st, st(2)
			frndint 
			call convert_and_pop
			add left_corner_y,ax 
		check_y_lt0:
			cmp left_corner_y, 0 
			jge check_y_gt_bottom_border
			sub left_corner_y, ax 
			jmp FPU_move_out_of_bound
		check_y_gt_bottom_border:
			cmp left_corner_y, graphic_mode_height - image_height 
			jle FPU_move_ret
			sub left_corner_y,ax
		FPU_move_out_of_bound:
			mov flag,1
		FPU_move_ret:
			pop es 
			pop ds
			popa
			ret
	FPU_move endp 

	FPU_rotation proc c angle: word 
		local x:word, y:word, xp:word, yp:word, value: byte
		mov x,0 
		mov y,0 
		mov xp,0 
		mov yp,0
		pusha
		push es 
		push ds
		printf offset rotating_info
		;establish global_angle
		xor dx,dx 
		mov ax, angle
		mov bx, 360d
		div bx
		;dx = reminder, ax = quotient
		add global_angle, dx 
		xor dx,dx 
		mov ax, global_angle
		div bx 
		mov global_angle, dx
		; ^ algorithm: 
		;angle := angle % 360 
		;global_angle := global_angle + angle
		;global_angle := global_angle % 360
		finit ;init coprocessor registers
		invoke convert_and_push, angle
		fldpi 
		fmul ; st(0) = angle * pi
		invoke convert_and_push, 180
		fdiv;st(0) = angle *pi/180
		fsincos 
		invoke convert_and_push, image_width/2
		invoke convert_and_push, image_height/2
		mov cx, word ptr image_height
		rows:
			push cx 
			mov cx, word ptr image_width
			mov x, 0
			columns:
				push cx 
				invoke convert_and_push, y
				fsub st(0), st(1)
				fmul st(0), st(4)
				invoke convert_and_push, x
				fsub st(0),st(3)
				fmul st(0), st(4)
				fsub ;pop st(0) pop st(1) and push st(1) - st(0)
				fchs ;change sign of st(0)
				fadd st(0), st(2)
				frndint ;round st(0) to the nearest integer
				call convert_and_pop
				mov xp, ax 
				invoke convert_and_push,y
				fsub st(0), st(1)
				fmul st(0),st(3)
				invoke convert_and_push, x
				fsub st(0),st(3)
				fmul st(0), st(5)
				fadd ;pop st(0) pop st(1) and push st(0) + st(1) 
				fadd st(0), st(1)
				frndint
				call convert_and_pop
				mov yp, ax 
				; if ... continue
				mov ax, 0 
				cmp xp, ax
				jl columns_next_step
				mov ax, 0 
				cmp yp,ax 
				jl columns_next_step
				mov ax, word ptr image_width
				cmp xp,ax 
				jge columns_next_step
				mov ax, word ptr image_height
				cmp yp,ax 
				jge columns_next_step
				;else
				mov ax, seg ImageSegment
				mov es, ax 
				invoke set_di_at_xy, x,y, 6,6
				mov al, byte ptr es:[di]; value = ImageSegment[y][x]
				mov byte ptr value, al
				invoke set_di_at_xy, xp,yp,6,6
				mov ax, seg RotatedImageSegment
				mov es, ax 
				mov al, value
				mov byte ptr es:[di], al
				columns_next_step:
					inc x
					pop cx
					dec cx
					cmp cx, 0
			jnz columns
			inc y 
			pop cx
			dec cx 
			cmp cx, 0
		jnz rows
		;now  copy result to ImageSegment
		mov ax, seg RotatedImageSegment
		mov ds, ax 
		mov ax, seg ImageSegment
		mov es,ax 
		cld 
		xor di,di 
		xor si,si 
		mov cx, image_height * image_width/2
		rep movsw
		;clear RotatedImageSegment
		mov ax, seg RotatedImageSegment
		mov es,ax
		xor ax,ax 
		mov cx, image_height * image_width/2
		xor di,di 
		rep stosw
		pop ds 
		pop es 
		popa 
		ret  
	FPU_rotation endp 

	set_di_at_xy proc c x:word, y:word, seg_width_first_part: byte, seg_width_second_part: byte
		local res:word
		pusha 
		mov ax, y
		mov di, y
		mov cl, seg_width_first_part
		shl di, cl
		mov cl,seg_width_second_part
		shl ax, cl
		add di,ax  
		add di, x
		mov res, di 
		popa 
		mov di,res 
		ret 
	set_di_at_xy endp 

	convert_and_pop proc c 
		fstp float
		lesi float ;load address and offset of float to es:di
		ldfpa ;load floating point accumulator with value pointed by es:di
		ftoi ;converts float value in FPA to int value and stores it in ax 
		ret 
	convert_and_pop endp 

	convert_and_push proc c value: word
		mov ax, value
		cmp ax, 0 
		jz pushZero
		itof
		lesi float 
		sdfpa
		fld float ;push float(ax)
		jmp cap_ret
		pushZero:
			fldz
		cap_ret:
			ret 
	convert_and_push endp

	parse_commands proc c
		pusha 
		push es 
		push ds 
		mov ax, seg OrdersSegment
		mov es, ax 
		mov ax, seg dseg 
		mov ds, ax 
		xor di,di
		xor si,si 
		mov cx, word ptr [es:commands_buffer_size + 1]
		phase_one_prep:
			invoke prepare_array, 4, offset command, ds
			invoke prepare_array, 3, offset argument, ds
			cmp cx, 0 
			jz parse_commands_ret
		phase_one:
			mov al, byte ptr es:[si]
			phase_one_check_if_enter:
				cmp al, 13 
				jnz phase_one_check_if_tab
				inc line_number 
				add si, 2
				dec cx  ;loop will decrement cx afterwards as well
				jmp phase_one_next_step
			phase_one_check_if_tab:
				cmp al, 9 ;tabulator
				jnz phase_one_check_if_space 
				inc si 
				jmp phase_one_next_step
			phase_one_check_if_space:
				cmp al, ' '
				jnz @F
				inc si 
				jmp phase_one_next_step
			phase_one_next_step:
		loop phase_one
		jmp parse_commands_ret
		@@:
			xor di, di 
			command_detected_phase:
				command_detected_phase_check_if_tab:
					mov al, byte ptr es:[si]
					cmp al, 9
					jnz command_detected_phase_check_if_space
					invoke recognize_command, di,cx,si
					jmp phase_one_prep
				command_detected_phase_check_if_space:
					cmp al, ' ' 
					jnz command_detected_phase_check_if_enter
					invoke recognize_command, di,cx, si
					jmp phase_one_prep
				command_detected_phase_check_if_enter:
					cmp al, 13 
					jnz update_command
					invoke recognize_command, di,cx, si
					jmp phase_one_prep
				update_command:
					cmp di, 7 
					jg command_detected_phase_increment_index
					mov byte ptr ds:[di + offset command], al
					command_detected_phase_increment_index:
						inc di 
						inc si 
				command_detected_phase_next_step:
			loop command_detected_phase
			;after exiting the loop above we must check if any letter 
			;was inserted into the commands buffer
			mov al, byte ptr [ds:0h + offset command]
			cmp al, "$"
			jz parse_commands_ret 
			invoke recognize_command, di, cx, si
		parse_commands_ret:
			pop ds 
			pop es 
			popa
			ret
	parse_commands endp 

	recognize_command proc c string_length: word, cx_value: word, si_value: word 
		pusha
		push ds 
		push es 
		cmp string_length, 7 
		jg too_long_command_error
		mov ax, seg dseg ;load data segment
		mov ds, ax 
		mov es, ax
		cld ;work upwards
		check_rotate:
			invoke set_cx_to_max, 6, string_length
			mov si, offset command
			mov di, offset rotate
			repe cmpsb
			je rotate_match
		check_move:
			invoke set_cx_to_max, 4, string_length
			mov si, offset command
			mov di, offset move
			repe cmpsb
			je move_match
		check_penup:
			invoke set_cx_to_max, 5, string_length
			mov si, offset command
			mov di, offset penup
			repe cmpsb
			je penup_match
		check_pendown:
			invoke set_cx_to_max, 7, string_length
			mov si, offset command
			mov di, offset pendown
			repe cmpsb
			je pendown_match
		nothing_matches:
			printf offset line_info 
			invoke print_decimal, line_number
			printf offset command 
			printf offset not_recognized_msg
			invoke move_to_eol, cx_value, si_value
			mov cx_value, cx
			mov si_value, si
			mov ah, 8 
			int 21h 
			jmp recognize_command_ret
		rotate_match:
			printf offset line_info 
			invoke print_decimal, line_number
			printf offset rotate
			printf offset recognized_msg
			invoke parse_single_arg, cx_value, si_value
			mov cx_value, cx
			mov si_value, si
			cmp flag, 1
			jz rotate_error
			cmp flag,2 
			jz overflow_error
			mov ah, 8 
			int 21h 
			call rotate_handler
			jmp recognize_command_ret
		move_match:
			printf offset line_info 
			invoke print_decimal, line_number
			printf offset move
			printf offset recognized_msg
			invoke parse_single_arg, cx_value, si_value
			mov cx_value, cx
			mov si_value, si
			cmp flag, 1
			jz move_error
			cmp flag,2 
			jz overflow_error
			mov ah, 8 
			int 21h 
			call move_handler
			jmp recognize_command_ret
		penup_match:
			printf offset line_info 
			invoke print_decimal, line_number
			printf offset penup
			printf offset recognized_msg
			invoke parse_no_args, cx_value, si_value
			mov cx_value, cx
			mov si_value, si
			cmp flag, 1
			jz penup_error
			mov ah, 8 
			int 21h 
			call penup_handler
			jmp recognize_command_ret
		pendown_match:
			printf offset line_info 
			invoke print_decimal, line_number
			printf offset pendown
			printf offset recognized_msg
			invoke parse_no_args, cx_value, si_value
			mov cx_value, cx
			mov si_value, si
			cmp flag, 1 
			jz pendown_error
			mov ah, 8 
			int 21h 
			call pendown_handler
			jmp recognize_command_ret
		too_long_command_error:
			printf offset line_info 
			invoke print_decimal, line_number
			printf offset error_msg_command_too_long
			invoke move_to_eol, cx_value, si_value
			mov cx_value, cx
			mov si_value, si
			mov ah, 8 
			int 21h 
			jmp recognize_command_ret
		rotate_error:
			printf offset line_info 
			invoke print_decimal, line_number
			printf offset rotate
			print_char ':'
			print_char ' '
			printf offset error_msg_one_arg
			mov ah, 8 
			int 21h 
			jmp recognize_command_ret
		move_error:
			printf offset line_info 
			invoke print_decimal, line_number
			printf offset move
			print_char ':'
			print_char ' '
			printf offset error_msg_one_arg
			mov ah, 8 
			int 21h 
			jmp recognize_command_ret
		penup_error:
			printf offset line_info 
			invoke print_decimal, line_number
			printf offset penup
			print_char ':'
			print_char ' '
			printf offset error_msg_no_arg
			mov ah, 8 
			int 21h 
			jmp recognize_command_ret
		pendown_error:
			printf offset line_info 
			invoke print_decimal, line_number
			printf offset pendown
			print_char ':'
			print_char ' '
			printf offset error_msg_no_arg
			mov ah, 8 
			int 21h 
			jmp recognize_command_ret
		overflow_error:
			printf offset line_info 
			invoke print_decimal, line_number
			print_char ':'
			print_char ' '
			printf offset error_msg_argument_too_long
			mov ah, 8 
			int 21h 
			jmp recognize_command_ret
		recognize_command_ret:
			pop es 
			pop ds 
			popa
			mov cx, cx_value
			mov si, si_value
			ret 
	recognize_command endp 

	pendown_handler proc c
		push ds
		push ax 
		mov ax, seg dseg
		mov ds, ax
		mov pen_flag, 1
		pop ax
		pop ds 
		ret 
	pendown_handler endp

	penup_handler proc c
		push ds
		push ax 
		mov ax, seg dseg
		mov ds, ax
		mov pen_flag, 0
		pop ax
		pop ds 
		ret 
	penup_handler endp

	rotate_handler proc c
		pusha  
		invoke my_atoi, ds, offset argument
		cmp flag, 1 
		jz rotate_argument_error
		invoke FPU_rotation, ax 
		invoke paint_tortoise, seg ImageSegment
		printf offset rotate_info_one
		printf offset argument
		print_char 10 
		print_char 13
		printf offset rotate_info_two
		invoke print_decimal, global_angle
		print_char 10 
		print_char 13
		call paint_lines
		call turn_graphics_off
		jmp rotate_handler_ret
		rotate_argument_error:
			printf offset rotate 
			print_char ':'
			print_char ' '
			printf offset atoi_invalid_arg
			mov ah,8 
			int 21h
		rotate_handler_ret:
			popa 
			ret 
	rotate_handler endp

	move_handler proc c
		local x: word, y:word, steps: word
		mov x, 0 
		mov y, 0 
		pusha
		push ds 
		invoke my_atoi, ds, offset argument
		cmp flag, 1 
		jz move_argument_error
		mov steps, ax
		mov cx, ax 
		@@:
			call FPU_move
			cmp flag,1 
			jz move_handler_ret
			invoke paint_tortoise, seg ImageSegment
			printf offset move_info
			invoke print_decimal, steps
			print_char 10 
			print_char 13 
			cmp pen_flag, 1 
			jz print_pendown
			print_penup:
				printf offset penup_info
				jmp after_pen_info
			print_pendown:
				printf offset pendown_info
			after_pen_info:
			push cx  
			mov ax, seg dseg 
			mov ds, ax 
			mov dx, word ptr ds:left_corner_y
			add dx, word ptr image_height/2
			mov cx, word ptr ds:left_corner_x 
			add cx, word ptr image_width/2

			finit  
			invoke convert_and_push, global_angle
			invoke convert_and_push, 180
			fadd
			fldpi 
			fmul ; pop st(0), pop st(1), push st(0) = (angle - 180) (st(1)) * pi (st(0))
			invoke convert_and_push, 180
			fdiv; pop st(0), pop st(1), push st(0) = [angle * pi](st(1))/180 (st(0))
			fsincos 

			invoke convert_and_push, tortoise_length
			fmul st(0), st(1)
			frndint 
			call convert_and_pop
			add cx,ax 

			invoke convert_and_push, tortoise_length
			fmul st, st(2)
			frndint 
			call convert_and_pop
			add dx, ax

			invoke set_di_at_xy, cx, dx, 8,6
			cmp pen_flag, 1
			jnz move_handler_next_step
			mov ax, seg PaintedLinesSegment
			mov es, ax 
			mov byte ptr es:[di], byte ptr line_color
			move_handler_next_step:
				call paint_lines
				call delay
				pop cx
				dec cx 
				cmp cx,0 
			jnz @B
			mov ah,0 
			mov al,3 
			int 10h ;turn write(text) mode on
			jmp move_handler_ret
			move_argument_error:
				printf offset move 
				print_char ':'
				print_char ' '
				printf offset atoi_invalid_arg 
				mov ah, 8 
				int 21h 
			move_handler_ret:
				pop ds 
				popa 
				ret 
	move_handler endp 

	paint_lines proc c 
		local i:word,j:word
		mov i,0 
		mov j,0 
		pusha 
		push es
		mov cx, word ptr graphic_mode_height
		paint_lines_rows:
			push cx 
			mov j,0
			mov cx, word ptr graphic_mode_width
			paint_lines_columns:
				push cx 
				invoke set_di_at_xy, j, i, 8,6
				mov ax, seg PaintedLinesSegment 
				mov es, ax 
				mov dl, byte ptr es:[di]
				cmp dl, line_color
				jnz paint_lines_columns_next_step
				invoke set_di_at_xy, j,i, 8,6 
				mov ax, 0a000h
				mov es, ax
				mov byte ptr es:[di], dl 
				paint_lines_columns_next_step:
					inc j
					pop cx
			loop paint_lines_columns
			inc i
			pop cx 
		loop paint_lines_rows
		pop es 
		popa 
		ret 
	paint_lines endp 

	move_to_eol proc c cx_value:word, si_value: word 
	;if a command was not recognized, skip rest of the characters on this line
		pusha
		push es 
		mov ax, seg OrdersSegment
		mov es, ax 
		mov cx, cx_value
		mov si, si_value
		cmp cx, 0 
		jz move_to_eol_ret
		@@:
			mov al, byte ptr es:[si]
			cmp al, 13 
			jz move_to_eol_ret
			inc si
			dec cx_value
			inc si_value
		loop @B 
		move_to_eol_ret:
			pop es
			popa 
			mov cx, cx_value
			mov si, si_value
			ret 
	move_to_eol endp

	parse_single_arg proc c cx_value:word, si_value:word 
		local arg_c: word, prev_tabspace: word
		mov arg_c, 0
		pusha 
		push ds 
		push es 
		mov ax, seg dseg
		mov ds, ax 
		mov ax, seg OrdersSegment
		mov es, ax
		mov cx, cx_value
		mov si, si_value
		xor di,di 
		mov flag, 0 
		mov prev_tabspace,0 
		; ^ flag indicating if previously tab or space characters were dealt with 
		cmp cx,0 
		jnz @F
		jmp parse_single_arg_ret ;eof and no arguments: parsing error 
		@@:
			mov al, es:[si]
			parse_single_arg_check_if_enter:
				cmp al, 13 
				jnz parse_single_arg_check_if_space
				jmp parse_single_arg_ret; end of line
			parse_single_arg_check_if_space:
				cmp al, ' ' 
				jnz parse_single_arg_check_if_tab
				inc si 
				mov prev_tabspace, 1 
				jmp parse_single_arg_next_step
			parse_single_arg_check_if_tab:
				cmp al, 9 
				jnz parse_single_arg_character_detected
				mov prev_tabspace, 1
				inc si 
				jmp parse_single_arg_next_step
			parse_single_arg_character_detected:
				cmp di, argument_buffer_size - 1 
				;argument_buffer_size = "$", so we must subtract 1 
				jge set_overflow_flag
				mov byte ptr ds:[di + offset argument], al
				set_overflow_flag:
					cmp di, argument_buffer_size - 1 
					;argument_buffer_size = "$", so we must subtract 1 
					jl check_if_inc_argc
					mov flag, 2 
				check_if_inc_argc:
					cmp prev_tabspace, 1 
					jnz parse_single_arg_move_index
					inc arg_c 
				parse_single_arg_move_index:
					inc si 
					inc di 
					mov prev_tabspace, 0 
			parse_single_arg_next_step:
				dec cx_value
				inc si_value 
		loop @B
		parse_single_arg_ret: 
			cmp arg_c, 1 
			jz @F
			mov flag,1 ;more than one arg; priority higher than 1 error code 
			@@:
				pop es 
				pop ds 
				popa 
				mov cx, cx_value
				mov si, si_value
				ret 
	parse_single_arg endp 

	parse_no_args proc c cx_value:word, si_value:word 
		pusha 
		push ds 
		push es 
		mov ax, seg dseg
		mov ds, ax 
		mov ax, seg OrdersSegment
		mov es, ax
		mov cx, cx_value
		mov si, si_value
		mov flag,0 ;for now assume everything is ok
		cmp cx,0 
		jz parse_no_args_ret
		@@:
			mov al, es:[si] 
			parse_no_args_check_if_enter:
				cmp al, 13 
				jnz parse_no_args_check_if_space
				jmp parse_no_args_ret; end of line, no args, command is correct
			parse_no_args_check_if_space:
				cmp al, ' ' 
				jnz parse_no_args_check_if_tab
				inc si 
				jmp no_args_next_step
			parse_no_args_check_if_tab:
				inc si 
				cmp al, 9
				jnz no_args_wrong_usage
				jmp no_args_next_step
				no_args_wrong_usage:
					mov flag, 1
			no_args_next_step:
				dec cx_value
				inc si_value 
		loop @B
		jmp parse_no_args_ret
		parse_no_args_ret:
			pop es 
			pop ds 
			popa 
			mov cx, cx_value
			mov si, si_value
			ret 
	parse_no_args endp 

	prepare_array proc c string_length:word, string_offset:word, string_segment: word 
		pusha 
		push es 
		cld ; Work upward
		mov es, string_segment
		mov ax, '$$' ; Load character to fill
		mov cx, string_length ; Load length of string
		mov di, string_offset ; Load address of destination
		rep stosw 
		pop es 
		popa 
		ret 
	prepare_array endp 

	my_atoi proc c string_segment: word, string_offset: word 
		;arguments: segment and offset of buffer containing a string 
		;which ends in "$", is decimal and contains no spaces and 
		;other characters than digits 0-9 and a dolar mark 
		;res is returned in ax
		local res: word, coefficient: word  
		mov res, 0 
		mov coefficient, 1 
		pusha 
		push es 
		mov es, string_segment
		mov di, string_offset
		mov flag, 0 
		@@:
			mov al, byte ptr es:[di]  
			cmp al, "$"
			jz @F
			left_border:
				cmp al, '0'
				jge right_border ;if (al >= '0')
				jmp wrong_character
			right_border:
				cmp al, '9'
				jle atoi_accepts ;if (al <= '9')
				jmp wrong_character
			atoi_accepts:
				inc di
		jmp @B
		@@:
			sub di, string_offset
			cmp di, 0 
			jz wrong_character
			mov cx, di
			add di, string_offset
		@@:
			dec di 
			xor ax, ax 
			mov al, byte ptr es:[di]
			; digit := digit - (int)'0'
			; digit := digit * 10 
			; res := res + digit 
			sub al, '0'
			mul coefficient
			mov bx, res 
			add bx, ax 
			mov res, bx
			; coefficient := coefficient * 10
			mov ax, coefficient
			mov bx, 10
			mul bx
			mov coefficient, ax
		loop @B 
		jmp atoi_ret
		wrong_character:
			mov flag, 1
			mov res, 0 
		atoi_ret:
			pop es 
			popa 
			mov ax, res 
			ret 
	my_atoi endp 

	get_file_name proc c
		pusha 
		xor si, si 
		xor di, di 
		xor cx, cx 
		mov cl, [es: 80h]
		cmp cx,0
		jz error_handler
		@@: 
			mov al, byte ptr [es:81h + si]
			cmp al, ' '
			jnz @F 
			inc si 
		loop @B
		jmp error_handler
		@@:
			cmp di, file_name_length
			jz name_too_long
			mov al, byte ptr [es:81h + si]
			cmp al, ' ' 
			jz @F
			mov byte ptr [ds:offset file_with_orders + di],al
			inc si 
			inc di 
		loop @B 
		jmp get_file_name_ret
		@@:
			mov al, byte ptr [es:81h + si]
			cmp al, ' ' 
			jnz error_handler
			inc si 
		loop @B
		get_file_name_ret:
			mov byte ptr [ds:offset file_with_orders + di], 0 
			mov byte ptr [ds:offset file_with_orders + di + 1], "$"
			popa 
			ret 
		error_handler:
			printf offset input_error_msg
			Quit
		name_too_long:
			printf offset name_too_long_msg
			Quit
	get_file_name endp 

	paint_tortoise proc c image_segment:word
		local i: word,j: word 
		pusha
		push ds 
		push es 
		mov ax, image_segment
		mov ds, ax 
		mov ax, seg dseg 
		mov es, ax 
		mov ah, 0 
		mov al, 13h
		int 10h ;video mode 320x200
		mov cx, image_height 
		mov i,0
		xor di,di
		write_rows:
			push cx 
			mov cx, image_width
			mov j,0 
			write_columns:
				push cx
				mov al, ds:[di]
				push di
				push ax
				mov dx, i 
				mov ax, seg dseg  
				mov es, ax
				add dx, es:left_corner_y
				mov cx, j 
				add cx, es:left_corner_x
				invoke set_di_at_xy, cx,dx, 8,6
				mov ax, 0a000h  
				mov es, ax 
				pop ax 
				mov byte ptr es:[di], al
				pop di 
				inc j
				inc di 
				pop cx 
			loop write_columns
			inc i 
			pop cx 
		loop write_rows
		pop es 
		pop ds 
		popa 
		ret 
	paint_tortoise endp 

	load_data proc c name_offset_of_file: word, segment_for_data: word, size_of_input: word 
		local handle: word
		pusha 
		push ds 
		mov ax, seg dseg
		mov ds, ax 
		mov dx, name_offset_of_file 
		mov ah, 3dh
		mov al, 0
		int 21h
		mov handle, ax 
		jc open_error
		mov ax, segment_for_data
		mov ds, ax 
		mov ah, 3fh 
		mov dx, 0h 
		mov cx, size_of_input 
		mov bx, handle 
		int 21h 
		jc read_error
		push es
		push di 
		mov es, segment_for_data
		mov di, ax
		;bytes read in ax, it will serve as an index of the data buffer's end
		mov byte ptr es:[di], "$"
		mov di, size_of_input
		mov word ptr es:[di + 1], ax 
		;lets put the amount of actually read bytes after the ending dollar mark 
		pop di
		pop es
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
			printf name_offset_of_file 
			print_char ':'
			print_char ' '
			call display_error
			pop ds 
			popa
			Quit 
		@@:
			popa 
			ret 
	load_data endp 

	set_cx_to_max proc c num_one: word, num_two: word 
		push ax 
		mov ax, num_two
		cmp ax, num_one
		jg num_two_bigger
		num_one_bigger:
			mov cx, num_one
			jmp set_cx_to_max_ret
		num_two_bigger:
			mov cx, num_two
		set_cx_to_max_ret:
			pop ax
			ret 
	set_cx_to_max endp 

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

	display_error proc c 
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

	delay proc 
		pusha 
		mov ah, 86h 
		mov cx, 0Ah 
		mov dx, 0
		int 15h
		popa 
		ret
	delay endp 

cseg ends

sseg segment para stack 'stack'
	stk db 1024 dup (?)
sseg ends

zzzzzzseg segment para public 'zzzzzz'
	LastBytes db 16 dup (?)
zzzzzzseg ends

end Main