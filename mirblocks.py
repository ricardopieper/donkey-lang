#this file holds some examples of how hir -> mir might look like
#but we do a simple representation, from ast -> mir directly

def simplest(x: int) -> int:
    return x


def simplest_block(x: int) -> int:
    defscope 0:
        x: int
    
    defblock 0:
        usescope 0
        return x; 
    
    gotoblock 0


def double_if_even(x: int) -> int:
    if x % 2 == 0:
        return x * 2
    else:
        return x

def double_if_even_block(x: int) -> int:
    defscope 0:
        x: int
    
    defscope 1:
        inheritscope 0 
    
    defscope 2:
        inheritscope 0 
    

    defblock 0:
        usescope 0
        if x % 2 == 0:
            gotoblock 1
        else:
            gotoblock 2
    
    defblock 1:
        usescope 1
        return x * 2
    
    defblock 2:
        usescope 2
        return x
    
    gotoblock 0

def double_if_even_but_triple_if_divisible_by_3(x: int) -> int:
    result = x
    if x % 2 == 0:
        multiplier = 2
        result = result * multiplier

    if result % 3 == 0:
        multiplier_3 = 3
        result = result * multiplier_3

    return result
    
def double_if_even_but_triple_if_divisible_by_3_block(x: int) -> int:
    defscope 0:
        x: int
        result: int

    defscope 1:
        usescope 0
        multiplier: int

    defscope 2:
        usescope 0
        multiplier_3: int

    defblock 0:
        usescope 1
        if x % 2 == 0:
            gotoblock 1
        else:
            pass

        if result % 3 == 0:
            gotoblock 2
        else:
            pass

        return result            

    defblock 1:
        usescope 1
        multiplier = 2
        result = result * multiplier
    
    defblock 2:
        usescope 2
        multiplier_3 = 3
        result = result * multiplier_3
    
    gotoblock 0



def simplest_missing_return(x: int) -> int:
    if x == 0:
        return x
    

def simplest_missing_return_block(x: int) -> int:
    defscope 0:
        x: int    

    defblock 0:
        usescope 0
        if x == 0:
            return x
        else:
            pass

    gotoblock 0

    #When we try to check this, we know that the function returns a type 
    #and we'll go to the root block 0, we will also see that there are 2 branches, 
    #one of them returns, and the other just does nothing... so this function is not well defined


def more_complex_missing_return(x: int) -> int:
    if x == 0:
        return x
    else:
        if x == 1:
            return 2

 def more_complex_missing_return_block(x: int) -> int:

    defscope 0:
        x: int

    defblock 0:
        usescope 0

        if x == 0:
            gotoblock 1
        else:
            gotoblock 2    

    defscope 1:
        inheritscope 0

    defblock 1:
        usescope 1
        return x

    defscope 2:
        inheritscope 0

    defblock 2:
        usescope 2
        if x == 1:
            gotoblock 3
        else:
            pass 
    
    defscope 3:
        inheritscope 2

    defblock 3:
        usescope 3
        return 2

       
    gotoblock 0


            