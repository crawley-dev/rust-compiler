global _start
_start:
; setup stack frame
    push rbp
    mov rbp, rsp
; Program Start
; Add
    mov rax, 100
; Divide
    mov rax, 200
    mov rcx, 10
; Subtract
; Multiply
    mov rax, 3
    mov rcx, 10
; Exit Program
    mov rdi, QWORD [rsp+0] ; Ident('ans120')
    mov rax, 60
    syscall
