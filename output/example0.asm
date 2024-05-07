global _start
_start:
; setup stack frame
    push rbp
    mov rbp, rsp
; Program Start
; Exit Program
    mov rdi, 69
    mov rax, 60
    syscall
