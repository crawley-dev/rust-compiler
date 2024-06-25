global _start
_start:
; setup stack frame
    push rbp
    mov rbp, rsp
    ; Program Start
    mov byte [rbp-1], 5 ; Ident('unsigned_test')
    mov rax, [rbp-1] ; Ident('unsigned_test')
    lea rax, [rbp+1]
    mov qword [rbp-9], rax ; Ident('i_ptr')
    mov rax, [rbp-1] ; Ident('unsigned_test')
    lea rax, [rbp+1]
    mov rax, qword [rax]
    mov byte [rbp-10], rax ; Ident('test_test')
; Exit Program
    mov rdi, [rbp-10] ; Ident('test_test')
    mov rax, 60
    syscall
