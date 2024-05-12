global _start
_start:
; setup stack frame
    push rbp
    mov rbp, rsp
; Program Start
    mov rax, 100
    mov rcx, 50
    mov rsi, 50
    add rcx, rsi
    mov rsi, 5
    add rcx, rsi
    cqo
    idiv rcx
    mov QWORD [rsp+0], rax
; Exit Program
    mov rdi, QWORD [rsp+0] ; Ident('ans')
    mov rax, 60
    syscall
