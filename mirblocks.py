#this file holds some examples of how hir -> mir might look like
#but we do a simple representation, from ast -> mir directly
#This document represents how the design evolved, as my understanding of the problem evolved.
from readline import set_completer


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

def if_elif_chain_last_else_necessary_in_hir(x: int) -> int:
    #in this case we always return
    if x == 0:
        return 0
    elif x == 2:
        return 2
    elif x == 3:
        return 3
    else:
        return x
    #so it doesnt matter
    if x == 0:
        return x
    else:
        if x == 2:
            return 2
        else:
            if x == 3:
                return 3
            return x

    #but imagine this:
    if x == 0:
        return 0
    elif x == 2:
        return 2
    elif x == 3:
        x = 9
    else:
        return x
    
    return x * 2

    #in this case, if x == 3 then we reassign to 9 and multiply by 2 later 
    if x == 0:
        return x
    else:
        if x == 2:
            return 2
        else:
            if x == 3:
                x = 9
            else: #we have to have this else otherwise it would return 9
                return x
    return x * 2

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


def scope_inheritance_counterexample(x: int) -> int:
    if x == 1:
        print(y)
        return 2
    y = 1

#the way it works in my head is as follows:
def scope_inheritance_counterexample_blocks(x: int) -> int:
    defscope 0:
        x: int
        y: int
    
    defblock 0:
        usescope 0
        if x == 1:
            gotoblock 1
        y = 1

    defscope 1:
        inheritscope 0

    defblock 1:
        usescope 1
        print(y)
        return 2    

    gotoblock 0

#BUt this shouldn't work. This is just variables grouped by indentation level.
#In theory each time a variable is declared, 
#at some level of "indentation", it should result in a new scope. 
#It *can* inherit a previous scope's variable, so that we don't have to copy them everytime

def scope_inheritance_counterexample_fixed_blocks(x: int) -> int:
    defscope 0:
        x: int
    
    defblock 0:
        usescope 0
        if x == 1:
            gotoblock 1
        y = 1

    defscope 1:
        inheritscope 0

    defblock 1:
        usescope 1
        print(y)
        return 2    

    gotoblock 0



#A more familiar example where many variables get defined.
#Notice that the discount variable is different in both scopes. They are completely different variables.
#In python they would be accessible outside after the if. But we don't. If you want a variable to be accessible
#from the outside, you need to declare first
def bar() -> bool:
    customer = get_gustomer()
    customer_name = customer.get_name()
    customer_age = customer.get_age()
    if customer_age >= 18:
        print("customer is of legal age, can buy drinks")
        customer_birth_month = customer.get_birth_month()
        print("customer was born in " + customer_birth_month.to_string())
        current_month = get_current_month()
        if current_month == customer_birth_month:
            print("this is customer birthmonth, apply discount")
            discount = 0.15
            give_customer_discount(customer, discount)
        else:
            discount = 0
            print("No discount")
        return True
    else:
        print("customer is not of legal age, can only buy juice, soda and water")
        restrict_products(customer, ["juice", "soda", "water"])
        return False

def bar_blocks() -> bool:

    defscope 0:
        customer: Customer
    
    defscope 1:
        inheritscope 0
        customer_name: str

    defscope 2:
        inheritscope 1
        customer_age: i32

    defblock 0:
        usescope 0
        customer = get_customer()
        gotoblock 1
    
    defblock 1:
        usescope 1
        customer_name = customer.get_name()

    defblock 2:
        usescope 2
        customer_age = customer.get_age()
        if customer_age >= 18:
            gotoblock 3
        else:
            gotoblock 10

    defscope 3:
        inheritscope 2

    defblock 3:
        usescope 3
        print("customer is of legal age, can buy drinks")
        gotoblock 4

    defscope 4:
        inheritscope 3
        customer_birth_month: i32

    defblock 4:
        usescope 4
        customer_birth_month = customer.get_birth_month()
        print("customer was born in " + customer_birth_month.to_string())
        gotoblock 5


    defscope 5:
        inheritscope 4
        current_month: i32

    defblock 5:
        usecope 5
        current_month = get_current_month()
        if current_month == customer_birth_month:
            gotoblock 6
        else:
            gotoblock 8

    defscope 6:
        inheritscope 5

    defblock 6:
        inheritscope 6
        print("this is customer birthmonth, apply discount")
        gotoblock 7
    
    defscope 7:
        inheritscope 6
        discount: f32

    defblock 7:
        usescope 7
        discount = 0.15
        give_customer_discount(customer, discount)
        gotoblock 9

    defscope 8:
        inheritscope 6
        discount: f32
    
    defblock 8:
        usescope 8
        discount = 0
        print("No discount")
        gotoblock 9

    defscope 9:
        inheritscope 5

    defblock 9:
        usescope 9
        return True

    defscope 10:
        inheritscope 2
    
    defblock 10:
        usescope 10
        print("customer is not of legal age, can only buy juice, soda and water")
        gotoblock 11

    defscope 11:
        inheritscope 10
        $0: arr<str> #HIR expr reduce

    defblock 11:
        usescope 11 
        restrict_products(customer, $0)
        return False


#One possible optimization is to reduce the number of blocks and scopes
#especially when we have sequential variable declarations
#and we also ignore new scopes that don't declare anything
def bar_blocks_optimization_pass() -> bool:

    defscope 0:
        inheritscope 0 #there's always a default inherit from 0, we need some code to ignore self-inherits
        customer: Customer
        customer_name: str
        customer_age: i32
  
    defblock 1:
        usescope 0
        customer_name = customer.get_name()
        customer = get_customer()
        customer_age = customer.get_age()
        if customer_age >= 18:
            gotoblock 2
        else:
            gotoblock 10

    defblock 1:
        usescope 0
        print("customer is of legal age, can buy drinks")
        gotoblock 2

    defscope 2:
        inheritscope 0
        customer_birth_month: i32

    defblock 2:
        usescope 4
        customer_birth_month = customer.get_birth_month()
        print("customer was born in " + customer_birth_month.to_string())
        gotoblock ...

    ... you get the idea
    

#Let's take that example and remove the return
def bar() -> bool:
    customer = get_gustomer()
    customer_name = customer.get_name()
    customer_age = customer.get_age()
    if customer_age >= 18:
        print("customer is of legal age, can buy drinks")
        customer_birth_month = customer.get_birth_month()
        print("customer was born in " + customer_birth_month.to_string())
        current_month = get_current_month()
        if current_month == customer_birth_month:
            print("this is customer birthmonth, apply discount")
            discount = 0.15
            give_customer_discount(customer, discount)
        else:
            discount = 0
            print("No discount")
    else:
        print("customer is not of legal age, can only buy juice, soda and water")
        restrict_products(customer, ["juice", "soda", "water"])
        return False
    
    #implicit empty return here (equivalent of having just a return keyword here)

#Now the true banch of the first IF will point to the implicit empty return.
#Every block has to go somewhere! Or return.
# 
#So we can recursively navigate through the blocks (navigate through all paths of the control flow graph), 
#find all "leaf blocks" and check that all of them have the same type.


The understanding so far:

 - MIR blocks have a scope
 - Scopes can inherit from a larger previous scope
 - Scopes are **not** identation levels
 - Scope creation happens:
   - Every time a new variable is introduced
   - Every time a branch is introduced 
 - All MIR blocks need to tell where they will go: Return or gotoblock
 - Therefore MIR blocks are connected by finalizers ^^^^^^^^^^^^^^^^^
 - Blocks can finalize and goto a previous block! Think about loops
    - Return inside loops
    - Continue and break
 - In this case during CFG navigation we need to detect a cycle to avoid infinite loops
 - This does not change the problem. We will still find leaf blocks and check their types.
 - Language does not have support for loops at this point. But being able to return to a previous block
   is sufficient at MIR level


#we can never be happy
def counterexample():
    x: int = x

#the algorithm we outlined accepts this, but it shouldn't 
def counterexample_block():
    defscope 0:
        x: int

    defblock 0:
        usescope 0
        x: int = x

    
#whatever solution we come up with needs to support this
def counterexample2():
    x: int = 1
    x = x + 1

#the algorithm we outlined accepts this, but it shouldn't 
def counterexample2_block():
    defscope 0:
        x: int

    defblock 0:
        usescope 0
        x: int = 1
        gotoblock 1

    
    defblock 1:
        x = x + 1


#one way to do this is to invert the relationship between scopes and blocks:
#the scope defines what *will become available* to the block whose scope inherits it.
#this means the same thing that it currently is? no, look:

#let's make declarations and assignments *completely separated things*, and we introduce an empty scope
#    x: int = 1
#    x = x + 1
def counterexample2_block():
    #the null scope, only has globals available. Remember self-inherits are ignored
    defscope 0: 
        inheritscope 0 

    defblock 0:
        usescope 0
        gotoblock 1

    defscope 1:
        inheritscope 0 #block 1 can only read from scope 0! can write to 0 too
        x: int         #but it can write to x 

    defblock 1:
        usescope 1 #uses scope 1 but can only read from 0
        x = 1      # can write on scope 1 
        gotoblock 2

    defscope 2:         #force creation of a new scope 
        inheritscope 1  #but allow read from 1
                        #but it's empty so we can read and write to x

    defblock 2:         
        usescope 2      # now we can read and write from x
        x = x + 1

    gotoblock 0

#x: int = x + 1
#this wont work, and it shouldn't work
def counterexample_block():
    defscope 0: 
        inheritscope 0

    defblock 0:
        usescope 0
        gotoblock 1

    defscope 1:
        inheritscope 0
        x: int

    defblock 0:
        usescope 1
        x = x + 1       #read from X will be detected during checks, and X will not be found
        return

    gotoblock 0


def wasteful():
    y = 1
    x = y + 1

#The current problem I'm facing is that MIR generates way too much shit
def wasteful_blocks() -> Void:
    #this is fair enough, it's finishing the scope that contains the parameters, in this case none
    defscope 0:
        inheritscope 0
    #this creates a new scope for write-only y
    defscope 1:
        inheritscope 0
        y : i32
    #commits the y for read
    defscope 2:
        inheritscope 1
    #new scope for write-only x
    defscope 3:
        inheritscope 0
        x : i32
    #commit it so that we can read x after
    defscope 4:
        inheritscope 3
    #this is the block for the input parameters, in this case nothing is done with them, and there are none
    defblock 0:
        usescope 0
        gotoblock 1
    #this is the block that writes to readonly y, then the next block can use scope 2 to write to x and read y 
    defblock 1:
        usescope 1
        y = 1
        gotoblock 2
    #but it just doesn't do anything lmao
    defblock 2:
        usescope 2
        gotoblock 3
    #this could be block 2 using scope 3
    defblock 3:
        usescope 3
        x = y + 1
        gotoblock 4
    #this could easily be put in block 3, but it could be a read on x    
    defblock 4:
        usescope 4
        return

#If the HIR could ensure with 100% certainty that access to variables are valid, then we have no need for this entire
#commit mechanism.

#For now let's generate a bunch of code and once we can prove the HIR works, or if we have any ideas to simplify, we do it.