global _start
_start:
; setup stack frame
    push rbp
    mov rbp, rsp
    ; Program Start
    mov dword [rbp-4], 5
    mov dword [rbp-8], 0
    mov byte [rbp-9], 2
    mov byte [rbp-10], 5
    mov rax, 5
    neg rax
    mov dword [rbp-8], rax
    mov rax, [rbp-10] ; Ident('unsigned_int')
    neg rax
    mov dword [rbp-8], rax
    mov rax, [rbp-10] ; Ident('unsigned_int')
    lea [rbp+10]
    mov qword [rbp-18], rax
    mov rax, [rbp-10] ; Ident('unsigned_int')
    lea [rbp+10]
    mov byte [rbp-19], rax
