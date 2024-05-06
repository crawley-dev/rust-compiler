global _start
_start:
; setup stack frame
    push rbp
    mov rbp, rsp
; Program Start
    mov QWORD [rsp+0], 20
; LogicalOr
    mov rax, 0
    cmp rax, 0
    jne label2_OR_TRUE
; GreaterThan
; Add
    mov rax, 5
    mov rcx, 1
    add rax, rcx
    mov rcx, 6
    cmp rax, rcx
    setg al
    movzx rax, al
    mov rcx, rax; test
    cmp rcx, 0
    je label1_OR_FALSE
label2_OR_TRUE:
    mov rax, 1
    jmp label3_OR_FINAL
label1_OR_FALSE:
    mov rax, 0
label3_OR_FINAL:
    movzx rax, al
; Exit Program
    mov rdi, QWORD [rsp+8] ; Ident('n')
    mov rax, 60
    syscall
