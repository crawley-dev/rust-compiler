global _start
_start:
; setup stack frame
    push rbp
    mov rbp, rsp
    ; Program Start
    mov rax, 5
    mov rcx, 20
    mov rsi, 5
    sub rcx, rsi
    add rax, rcx
    mov byte [rbp-1], rax ; Ident('num')
    mov byte [rbp-2], 8 ; Ident('num2')
    mov rax, 5
    neg rax
    mov byte [rbp-3], rax ; Ident('num3')
    mov rax, 10
    mov rcx, 2
    sub rax, rcx
    mov rcx, 3
    imul rax, rcx
    mov rcx, 5
    mov rsi, 3
    mov rdi, 2
    add rsi, rdi
    cqo
    idiv rsi
    add rax, rcx
    mov byte [rbp-4], rax ; Ident('x')
; Exit Program
    mov rdi, [rbp-1] ; Ident('num')
    mov rax, 60
    syscall
