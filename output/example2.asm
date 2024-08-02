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
    mov word [rbp-2], rax ; Ident('num')
    mov byte [rbp-3], 8 ; Ident('num2')
    mov byte [rbp-3], 6 ; Ident('test')
; If
    mov rax, [rbp-3] ; Token { kind: Ident, value: Some("test"), pos: (7, 7) }
    mov rcx, 5
    cmp rax, rcx
    sete al
    movzx rax, al
    cmp rax, 0 
    je .1_IF_FALSE
; Exit Program
    mov rdi, 5
    mov rax, 60
    syscall
.1_IF_FALSE:
; Exit Program
    mov rdi, [rbp-2] ; Token { kind: Ident, value: Some("num"), pos: (8, 10) }
    mov rax, 60
    syscall
