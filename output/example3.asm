global _start
_start:
    mov rax, 69
    push rax
    mov rax, 70
    push rax
    mov rax, 69
    push rax
    mov rax, 5
    push rax
    push QWORD [rsp + 24]
    pop rax
    pop rbx
    add rax, rbx
    push rax
    pop rax
    pop rbx
    cmp rax, rbx
    sete al
    movzx rax, al
    push rax
    pop rax
    cmp rax, 0
    jle label1_if_false
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
    sete al
    movzx rax, al
    push rax
    pop rax
    cmp rax, 0
    jle label2_elif
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
