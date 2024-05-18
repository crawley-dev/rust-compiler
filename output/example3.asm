global _start
_start:
; setup stack frame
    push rbp
    mov rbp, rsp
    ; Program Start
    mov rax, 2
    mov rcx, 3
    add rax, rcx
    mov QWORD [rsp+0], rax
    mov rax, QWORD [rsp+0] ; Ident('ans')
    mov rcx, 50
    add rax, rcx
    mov QWORD [rsp+0], rax
; Exit Program
    mov rdi, QWORD [rsp+0] ; Ident('ans')
    mov rax, 60
    syscall
