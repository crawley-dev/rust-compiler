global _start
_start:
; setup stack frame
    push rbp
    mov rbp, rsp
    ; Program Start
    mov rax, 5
    mov rcx, 5
    add rax, rcx
    mov byte [rbp-1], rax ; Ident('unsigned_test')
    mov rax, 10
    mov rcx, 2
    imul rax, rcx
    mov rcx, 3
    mov rsi, 5
    mov rdi, 10
    mov r8, 5
    cqo
    idiv r8
    add rsi, rdi
    sub rcx, rsi
    add rax, rcx
    mov byte [rbp-1], rax
    mov rax, [rbp-1] ; Ident('unsigned_test')
    lea rax, [rbp+1]
    mov qword [rbp-9], rax ; Ident('i_ptr')
    mov rax, [rbp-1] ; Ident('unsigned_test')
    lea rax, [rbp+1]
    mov rax, [rax]
    mov byte [rbp-10], rax ; Ident('test_test')
; Exit Program
    mov rdi, [rbp-10] ; Ident('test_test')
    mov rax, 60
    syscall
