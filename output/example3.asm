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
    mov rax, [rbp-1] ; Ident('unsigned_test')
    lea rax, [rbp+1]
    mov qword [rbp-9], rax ; Ident('i_ptr')
    mov rax, [rbp-1] ; Ident('unsigned_test')
    lea rax, [rbp+1]
    mov rax, [rax]
    mov byte [rbp-10], rax ; Ident('test_test')
    mov rax, [rbp-9] ; Ident('i_ptr')
    mov rax, [rax]
    mov rcx, [rbp-9] ; Ident('i_ptr')
    mov rcx, [rcx]
    sub rax, rcx
    mov byte [rbp-10], rax
; Exit Program
    mov rdi, [rbp-10] ; Ident('test_test')
    mov rax, 60
    syscall
