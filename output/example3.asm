global _start
_start:
    mov rax, 69
    push rax
    mov rax, 70
    push rax
    mov rax, 69
    push rax
    push QWORD [rsp + 16]
    pop rax
    pop rbx
    cmp rax, rbx
    jne label1_if_false
    push QWORD [rsp + 8]
    mov rax, 60
    pop rdi
    syscall
label1_if_false:
    mov rax, 70
    push rax
    push QWORD [rsp + 8]
    pop rax
    pop rbx
    cmp rax, rbx
    jne label2_elif
    push QWORD [rsp + 0]
    mov rax, 60
    pop rdi
    syscall
label2_elif:
    mov rax, 10
    push rax
    mov rax, 60
    pop rdi
    syscall
