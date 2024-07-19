global _start
_start:
; setup stack frame
    push rbp
    mov rbp, rsp
    ; Program Start
    mov rax, 10
    mov rcx, 2
    mov rsi, 4
    mov rdi, 4
    mov r8, 2
    sub rdi, r8
    imul rsi, rdi
    add rcx, rsi
    sub rax, rcx
    not rax
    mov rcx, 1
    mov rsi, 1
    add rcx, rsi
    cqo
    idiv rcx
    mov rcx, 4
    mov rsi, 1
    sub rcx, rsi
    imul rax, rcx
    mov byte [rbp-1], rax ; Ident('ans')
