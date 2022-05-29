.main:
    stackoffset     16 
    push_imm32      15
    storeaddr_rel32 bp+0
    push_imm32      3
    storeaddr_rel32 bp+4    
    loadaddr_rel32  bp+0    
    loadaddr_rel32  bp+4    
    sums32                  
    storeaddr_rel32 bp+8    
    push_imm32      5       
    push_imm32      0       
    loadaddr_rel32  bp+8    
    loadaddr_rel32  bp+0    
    push_reg        bp      
    call            some_function    
    pop_reg         bp      
    pop32                   
    pop32                   
    sums32                  
    storeaddr_rel32 bp+12   
    loadaddr_rel32  bp+12   
    loadaddr_rel32  bp+4    
    sums32                  
    storeaddr_rel32 bp+12

some_function:
    loadaddr_rel32  bp-16   
    loadaddr_rel32  bp-12   
    stackoffset     12      
    push_imm32       0      
    loadaddr_rel32  bp+0    
    push_reg        bp      
    call            half             
    pop_reg         bp      
    pop32                   
    loadaddr_rel32  bp+4    
    sums32                  
    storeaddr_rel32 bp+8    
    loadaddr_rel32  bp+0     
    loadaddr_rel32  bp+8     
    muls32                  
    storeaddr_rel32 bp-20   
    stackoffset     0       
    return                  

half: 
    loadaddr_rel32  bp-12   
    stackoffset     4       
    loadaddr_rel32  bp+0    
    divs_imm32      2       
    storeaddr_rel32 bp-16   
    stackoffset     0       
    return 