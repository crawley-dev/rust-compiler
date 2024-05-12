global _start
_start:
; setup stack frame
    push rbp
    mov rbp, rsp
; Program Start
    mov rax, 10
    mov rcx, 2
    mov rsi, 4
    add rcx, rsi
    mov rsi, 4
    mov rdi, 2
    sub rsi, rdi
    add rcx, rsi
    sub rax, rcx
    mov rcx, 1
    mov rsi, 1
    add rcx, rsi
    cqo
    idiv rcx
    mov rcx, 4
    mov rsi, 1
    sub rcx, rsi
    imul rax, rcx
    mov QWORD [rsp+0], rax
; Exit Program
    mov rdi, QWORD [rsp+0] ; Ident('ans')
    mov rax, 60
    syscall
