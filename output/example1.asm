global _start
_start:
; setup stack frame
    push rbp
    mov rbp, rsp
    ; Program Start
 ; Ident('y')
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
    mov dword [rbp-12], rax ; Ident('x')
; Exit Program
    mov rdi, [rbp-12] ; Token { kind: Ident, value: Some("x"), pos: (6, 2) }
    mov rax, 60
    syscall
