; If
    mov rax, 5
    cmp rax, 0 
    je .1_IF_FALSE
; Exit Program
    mov rdi, 10
    mov rax, 60
    syscall
.1_IF_FALSE:
    mov QWORD [rsp+0], 200
; Exit Program
    mov rdi, QWORD [rsp+0] ; Ident('ans')
    mov rax, 60
    syscall
