global _start
_start:
; setup stack frame
    push rbp
    mov rbp, rsp
; Program Start
    mov QWORD [rsp+0], 20
    mov QWORD [rsp+8], 0
    mov QWORD [rsp+16], 0
; Exit Program
    mov rdi, QWORD [rsp+8]; Ident('i')
    mov rax, 60
    syscall
