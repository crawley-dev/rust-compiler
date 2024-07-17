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
; Exit Program
    mov rax, [rbp-1] ; Token { kind: Ident, value: Some("num"), pos: (8, 6) }
    mov rcx, 1
    add rax, rcx
    mov rdi, rax
    mov rax, 60
    syscall
