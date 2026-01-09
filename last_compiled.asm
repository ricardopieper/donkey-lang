	.file	"program"
	.text
	.globl	main
	.p2align	4
	.type	main,@function
main:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$1, 4(%rsp)
	movl	4(%rsp), %edi
	callq	.Lprint_int
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc

	.p2align	4
	.type	.Lprint_int,@function
.Lprint_int:
	.cfi_startproc
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movl	%edi, %esi
	movl	%edi, 4(%rsp)
	movl	$.L.str, %edi
	movq	$.L.str, 8(%rsp)
	xorl	%eax, %eax
	movq	$3, 16(%rsp)
	callq	printf@PLT
	addq	$40, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	.Lprint_int, .Lfunc_end1-.Lprint_int
	.cfi_endproc

	.type	.L.str,@object
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"%i\n"
	.size	.L.str, 4

	.section	".note.GNU-stack","",@progbits
