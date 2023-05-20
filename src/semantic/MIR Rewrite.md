This is a brainstorm document for the rewriting of the MIR stage.
Currently it is buggy AF, and I think I have a non-recursive method to generate it.


This document is a mess, but the following is the latest idea on a moderately complicated example:


Case 9: While loop that returns after a few iterations, where we increase the counter conditionally, but we return if x == 2, arbitrarily.

def main() -> i32:
    x = 10
    while x > 0:
        if x > 5:
            x = x + 1
        else:
            x = x + 2
        if x == 2:
            return x
    return x

Queue:
[
    {
        Block: 0
        Node: List[Declaration, While, Return]
        DefaultFinish: EmptyReturn ////safe to do, will result in typecheck error if the function is not void and the user forgot to return something, which is not the case
    },
]


Codegen of block 0, default finish: EmptyReturn
Number of statements: 3
Not grouped, running group operation.
Result: [[Declaration], [While], [Return]]
Reschedule.
New queue state:
Queue:
[
    {
        Block: 0
        Node: Group[If]
        DefaultFinish: Goto(1)
    },
    {
        Block: 1
        Node: Group[While]
        DefaultFinish: Goto(2)
    },
    {
        Block: 2
        Node: Group[While]
        DefaultFinish: EmptyReturn //Inherits the codegen parameter default finish
    },
]


Codegen of block 0, default finish: Goto(1)
Number of statements: 1
Already grouped.
Since we just have a declaration, we can just generate the code for it and move on. No need (and no way) to schedule more jobs.

defblock 0:
    x = 10
    gotoblock 1


New queue state:
Queue:
[
    {
        Block: 1
        Node: Group[While]
        DefaultFinish: Goto(2)
    },
    {
        Block: 2
        Node: Group[Return]
        DefaultFinish: EmptyReturn
    }
]


Codegen of block 1, default finish: Goto(2)
Number of statements: 1 (a While statement)
Already grouped.
Since it's a nested structure, we have to also schedule codegen for the internals of the while statement.
The code of the while statement generator will know that this has to be done.
And since while loops don't have an "else", the branch's else will be just the continuation block, so that when the while finishes, it goes to block 2 and returns the function.

defblock 1:
    if x > 0:
        gotoblock 3 (run a grouping and schedule it, remember to loopback to block 1!)
    else:
        gotoblock 2 [the default finish]



New queue state:
Queue:
[
    {
        Block: 2
        Node: Group[Return]
        DefaultFinish: EmptyReturn
    },
    {
        Block: 3
        Node: List[If, If] //this is not grouped, codegen for block 3 will have to run a grouping operation
        DefaultFinish: Goto(1)  
    },
]

Codegen of block 2, default finish: EmptyReturn
Number of statements: 1
Already grouped.


defblock 2:
    return x //default finish ignored 


New queue state:
Queue:
[
    {
        Block: 3
        Node: List[If, If]
        DefaultFinish: Goto(1)
    },
]

Codegen of block 3, default finish: Goto(1)
Number of statements: 2
Not grouped, run a grouping operation.
Result: [[If], [If]]
Reschedule.

New queue state:
Queue:
[
    {
        Block: 3
        Node: Group[If]
        DefaultFinish: Goto(4) //must flow into the other if
    },
    {
        Block: 4
        Node: Group[If]
        DefaultFinish: Goto(1) //the last one always inherits the goto from codegen parameters
    },
]

Codegen of block 3, default finish: Goto(4)

//Reminder: this is the code we are compiling:
if x > 5:
    x = x + 1
else:
    x = x + 2
Number of statements: 1
Already grouped.

//Since this is a nested structure, we have to schedule codegen for the internals of the if statement. Same way as the while statement.

defblock 3:
    if x > 5:
        gotoblock 5 (schedule the statement list)
    else:
        gotoblock 6 (schedule the statement list)

New queue state:
Queue:
[
    {
        Block: 4
        Node: Group[If]
        DefaultFinish: Goto(1)
    },
    {
        Block: 5
        Node: Group[Assign]
        DefaultFinish: Goto(4) //the last one always inherits the goto from codegen parameters
    },
    {
        Block: 6
        Node: Group[Assign]
        DefaultFinish: Goto(4) //the last one always inherits the goto from codegen parameters
    },
]


Codegen of block 4, default finish: Goto(1)

//Reminder: this is the code we are compiling:
if x == 2:
    return x
Number of statements: 1
Already grouped.

//Yes this is incredibly simple but we have to generate another group....

defblock 4:
    if x == 2:
        gotoblock 7 (schedule the statement list)
    else:
        //no code, just run the default finish
        gotoblock 1

New queue state:
Queue:
[
    {
        Block: 5
        Node: Group[Assign]
        DefaultFinish: Goto(4) //the last one always inherits the goto from codegen parameters
    },
    {
        Block: 6
        Node: Group[Assign]
        DefaultFinish: Goto(4) //the last one always inherits the goto from codegen parameters
    },
    {
        Block: 7
        Node: Group[Return x]
        DefaultFinish: Goto(1) //inherits the default finish from codegen parameters but we don't care because it's a return
    },
]

//codegen of the 5,6,7 blocks is trivial, just generate the code for the statements and move on.

Full CFG:
defblock 0:
    x = 10
    gotoblock 1

defblock 1:
    if x > 0:
        gotoblock 3 (run a grouping and schedule it, remember to loopback to block 1!)
    else:
        gotoblock 2 [the default finish]

defblock 2:
    return x //default finish ignored 

defblock 3:
    if x > 5:
        gotoblock 5 (schedule the statement list)
    else:
        gotoblock 6 (schedule the statement list)

defblock 4:
    if x == 2:
        gotoblock 7 (schedule the statement list)
    else:
        //no code, just run the default finish
        gotoblock 1

defblock 5:
    x = x + 1
    gotoblock 4

defblock 6:
    x = x + 2
    gotoblock 4

defblock 7:
    return x
    gotoblock 1



Case 10:

def main():
    if True:
        print("Indeed, it's true")

Queue: 
[
    {
        Block: 0
        Node: List[If]
        DefaultFinish: EmptyReturn
    },
]

Codegen of block 0, default finish: EmptyReturn
Number of statements: 1
Not grouped, run a grouping operation.
Result: [[If]]
Reschedule.

New queue state:
Queue:
[
    {
        Block: 0
        Node: Group[If]
        DefaultFinish: EmptyReturn
    },
]

Codegen of block 0, default finish: EmptyReturn
Number of statements: 1
Already grouped.

defblock 0:
    if True:
        gotoblock 1 (schedule the statement list)
    else:
        //no code, just run the default finish
        gotoblock 1
    




//This one has no return statement, the code just finishes with the while loop.

def main():
    x = 10
    while x > 0:
        y = x + 10
        x = x - 1


Queue:
[
    {
        Block: 0
        Node: List[Declaration, While]
        DefaultFinish: EmptyReturn
    },
]


Codegen of block 0, default finish: EmptyReturn
Number of statements: 2
Not grouped, run a grouping operation.
Result: [[Declaration], [While]]
Reschedule.

New queue state:
Queue:
[
    {
        Block: 0
        Node: Group[Declaration]
        DefaultFinish: Goto(1)
    },
    {
        Block: 1
        Node: Group[While]
        DefaultFinish: EmptyReturn
    },
]

Codegen of block 0, default finish: Goto(1)
Number of statements: 1
Already grouped.

defblock 0:
    x = 10
    gotoblock 1

New queue state:
Queue:
[
    {
        Block: 1
        Node: Group[While]
        DefaultFinish: EmptyReturn
    },
]

Codegen of block 1, default finish: EmptyReturn
Number of statements: 1
Already grouped.

defblock 1:
    if x > 0:
        gotoblock 2 (schedule the statement list)
    else:
        //no code, just run the default finish
        return //or gotoblock 3 and schedule a fake empty return?

New queue state:
Queue:
[
    {
        Block: 2
        Node: List[Declaration, Assign]
        DefaultFinish: Goto(1)
    }
]

Codegen of block 2, default finish: Goto(1)
Number of statements: 1
Not grouped, run a grouping operation.
Result: [[Declaration], [Assign]]
Reschedule.

New queue state:
Queue:
[
    {
        Block: 2
        Node: Group[Declaration]
        DefaultFinish: Goto(3)
    },
    {
        Block: 3
        Node: Group[Assign]
        DefaultFinish: Goto(1)
    }
]

//Ok, jumping some steps, those are trivial to generate.

Full CFG:

defblock 0:
    x = 10
    gotoblock 1

defblock 1:
    if x > 0:
        gotoblock 2
    else:
        return

defblock 2:
    y = x + 10
    gotoblock 3

defblock 3:
    x = x - 1
    gotoblock 1



















Simplest case:


def main() -> i32:
    return 1

//This is a breadth-first traversal of the MIR
//First: generate the first level of the main function

/*
Continuations:
Empty

Queue:
Empty
*/

Codegen of block 0, nodes: &HIR, Parent Block: None, Continuation block: None
defblock 0:
    return 1


Full CFG:
defblock 0:
        return 1



Case 1: if statement with no else branch
=======

//In the remaining cases you'll see "if True" everywhere, that is just to introduce branching, imagine that in real life we'd have anything more
//useful there. 

def main() -> i32:
    if True:
        return 1
    return 0

/*
Continuations:
Empty

Queue:
Empty
*/

//The MIR generation is a breadth-first traversal of the MIR in which instead of eagerly generate code as soon as we visit a node,
//instead we enqueue the generation of the code of the node, and then we dequeue it and generate it when we have no more nodes.

Stage 1: Enqueue level 0 of MIR
- Enqueue code generation of block 0, nodes: &HIR, Parent Block: None, Continuation block: 1
- Enqueue code generation of block 1, nodes: &HIR, Parent Block: None, Continuation block: None

defblock 0:
    [Enqueue code generation of block 0, which continues onto 1]

defblock 1:
    [Enqueue code generation of block 1, no continuation]



Queue:
[
    {
        Block: 0
        Node: &HIR
        Parent Block: None
        Continuation: 1
        Loopback: None
    }
    {
        Block: 1
        Node: &HIR
        Parent Block: None
        Continuation: None
        Loopback: None
    }
]




Codegen of block 0, nodes: &HIR, Parent Block: None, Continuation block: 1
defblock 0:
    if True:
        gotoblock 2
    else:
        gotoblock 1 //if there is no else branch, we just go to the continuation block.

defblock 2:
    [enqueue generation of code inside the true branch of the if, same continuation]


Queue:
[
    {
        Block: 1
        Node: &HIR
        Parent Block: None
        Continuation: None
        Loopback: None
    }
    {
        Block: 2
        Node: &HIR
        Parent Block: 0
        Continuation: 2
        Loopback: None
    }
]

Codegen of block 1, parent block: 0, continuation block: None:
    defblock 1:
        return 0

  
Queue:
[
    {
        Block: 2
        Node: &HIR
        Parent Block: 0
        Continuation: 2
        Loopback: None
    }
]

Codegen of block 2, parent block: 0, continuation block: 2:
    defblock 1:
        return 1


Full CFG:
defblock 0:
    if True:
        gotoblock 2
    else:
        gotoblock 1

    defblock 1:
        return 0

    defblock 2:
        return 1


Case 2: Both branches return:
========

def main() -> i32:
    if True:
        return 1
    else:
        return 0


/*
Queue: [
    {
        Block: 0
        Node: &HIR
        Parent Block: None
        Continuation: None
        Loopback: None
    }
]

*/

Codegen of block 0, nodes: &HIR, Parent Block: None, Continuation block: None
    defblock 0:
        if True:
            gotoblock 1
        else:
            gotoblock 2

    defblock 1:
        [enqueue generation of code inside the true branch of the if]

    defblock 2:
        [enqueue generation of code inside the false branch of the if]

/*
Continuations:
Empty

Queue:
[
    {
        Block: 1
        Node: &HIR
        Parent Block: 0
    },
    {
        Block: 2
        Node: &HIR
        Parent Block: 0
    }
]
*/


//here we will be able to dequeue both in succession

Codegen of block 1, parent block: 0, continuation block: None
    defblock 1:
        return 1

Codegen of block 2, parent block: 0, continuation block: None
    defblock 2:
        return 0


Case 3: More complex:
============


def main() -> i32:

    x = 1
    y = 2
    if True:
        z = 3
        y = y + z
        x = x + y
    return y


/* 
Continuations:
Empty

Queue:
Empty
*/

//One detail is that every new variable declaration creates a new block, so x and y have different blocks that flows sequentially

Codegen of block 0, nodes: &HIR, Parent Block: None, Continuation block: None
    defblock 0:
        x = 1
        gotoblock 1

    defblock 1:
        y = 2
        gotoblock 2

    defblock 2:
        if True:
            gotoblock 3
        else:
            //because this branch is empty, we delegate to the direct continuation of block 2 which in the future will be block 4
            [delegated: get the direct continuation of block 2]
            //when we generate block 4 (which we will do immmediately after this, in the breadth-first traversal),
            //we will need to check if the previous block generation resulted in a request for a direct continuation, 
            //and if so, we will need to register this new block as the direct continuation of block 2

    defblock 3:
        [enqueue generation of code inside the true branch of the if]

    defblock 4 (register as direct continuation of block 2):
        return y

/* 
Continuations:
Block2 -> Block4

Queue:
[
    {
        Block: 3
        Node: &HIR
        Parent Block: 2
    }
]
*/

Codegen of block 3, parent block: 2, continuation block: 4
    defblock 3:
        z = 3
        gotoblock 5
    
    defblock 5:
        y = y + z
        x = x + y
        //since there's no more code to generate, we have to delegate control to our parent block 2 and it's continuation.
        //If we did not have a continuation block (because we are on the root level), we would have to create a new block
        //which would likely result in an implicit return statement
        [delegate: get the direct continuation of block 2]

//Queue is empty
//Continuations: Block2 -> Block4
//CFG before resolving delegations: 

defblock 0:
    x = 1
    gotoblock 1

defblock 1:
    y = 2
    gotoblock 2

defblock 2:
    if True:
        gotoblock 3
    else:
        [delegated: get the direct continuation of block 2]

defblock 3:
    z = 3
    gotoblock 5

defblock 4:
    return y

defblock 5:
    y = y + z
    x = x + y
    [delegated: get the direct continuation of block 2]

//Resolve delegations:

defblock 0:
    x = 1
    gotoblock 1

defblock 1:
    y = 2
    gotoblock 2

defblock 2:
    if True:
        gotoblock 3
    else:
        gotoblock 4

defblock 3:
    z = 3
    gotoblock 5

defblock 4:
    return y

defblock 5:
    y = y + z
    x = x + y
    gotoblock 4


Case 4: Example that breaks the compiler in pieces currently
================

def main() -> i32:
    if True:
        x = 1
        if True:
            return 1
    y = 1
    return y

//This is a breadth-first traversal of the CFG
//First: generate the first level of the main function. When we find an if statement, we pre-generate the branch and the necessary fallback

/*
Continuations:
Empty

Queue:
Empty
*/


Codegen of block 0, nodes: &HIR, Parent Block: None, Continuation block: None
    defblock 0:
        if True:
            gotoblock 1
        else:
            [delegated: get the direct continuation of block 0]

    defblock 1:
        [enqueue generation of code inside the first if]

    defblock 2 (direct continuation of block 0):
        y = 1
        gotoblock 3

    defblock 3:
        return y

/*
Continuations:
Block0 -> Block2

Queue:
[{
    Block: 1
    Node: &HIR
    Parent Block: 0
}]
*/


//dequeue code generation of code inside the first if
Codegen of block 1, parent block: 0, continuation block: 2:
    defblock 1:
        x = 1
        gotoblock 4

    defblock 4:
        if True:
            gotoblock 5
        else:
            [delegated: get the direct continuation of block 4]

    defblock 5:
        [enqueue generation of code inside the second if] //it will be just a return 1

    defblock 6: (direct continuation of block 4)
        //Here is the issue: We are done with code generation when we generate block 4. The next step will be dequeuing block 5 generation,
        //and that's it. This block will stay empty. In this case, the continuation of block 4 would be to delegate control to the parent block 0,
        //which continues onto block 2.
        //While we're generating block 1, we will have to run a finalizing procedure on the breadth-first traversal to 
        //detect if we have unfinished continuation blocks. In these cases, we do the delegating procedure since we will have this data on
        //the function call parameters, no need for extra bookkepping. 

/*
Continuations:
Block0 -> Block2
Block4 -> Block6

Queue: [
    {
        Block: 5
        Node: &HIR
        Parent Block: 4
    }
]
*/

//Finalizing procedure of block 1:
//detect any unfinished blocks that have been registered as continuations.
//Block 6 was registered as a continuation of block 4. Since there's no more code, delegate to parent block 0 which continues onto block 2.

defblock 1:
    x = 1
    gotoblock 4

defblock 4:
    if True:
        gotoblock 5
    else:
        [delegated: get the direct continuation of block 4]

defblock 5:
    [enqueue generation of code inside the second if] //it will be just a return 1

defblock 6: (direct continuation of block 4)
    gotoblock 2


//dequeue code generation of code inside the second if
Codegen of block 5, parent block: 4, continuation block: 6:
    defblock 5:
        return 1


Full CFG:

defblock 0:
    if True:
        gotoblock 1
    else:
        gotoblock 2 

defblock 1:
    x = 1
    gotoblock 4

defblock 2:
    y = 1
    gotoblock 3

defblock 3:
    return y

defblock 4:
if True:
    gotoblock 5
else:
    gotoblock 6

defblock 5:
    return 1

defblock 6:
    gotoblock 2




Case 5: Just an if statement
=======
    
//Ok this example is stupid, but legal. There is an implicit return block in the end.

def main():
    if True:
        return


/*
Continuations:
Empty

Queue:
Empty
*/

Codegen of block 0, parent block: None, continuation block: None:

defblock 0:
    if True:
        gotoblock 1;
    else:
        [delegated: get the direct continuation of block 4]


defblock 1:
    [enqueue generation of code inside the if]

defblock 2 (register as continuation of block 0):
    //No more code



/*
Continuations:
Block 0 -> Block 2

Queue: [
    {
        Block: 1
        Node: &HIR
        Parent Block: 0
    }
]
*/

//Finalizing procedure of block 0:
//detect any unfinished blocks that have been registered as continuations.
//Block 2 was registered as a continuation of block 0. Since there's no more code, delegate to parent block None which continues onto block None.
//If continuation block is None, then we just return.

defblock 2 (register as continuation of block 0):
    return

Codegen of block 1,  parent block: 0, continuation block: 2:

defblock 1:
    return

Full CFG:

defblock 0:
    if True:
        gotoblock 1;
    else:
        gotoblock 2

defblock 1:
    return

defblock 2:
    return


Case 6: While loop that immediately returns
=======

This is stupid, but potentially compiler-breaking

def main():
    while True:
        return

//While statements are just if statements with a goto to the beginning of the while loop.

Codegen of block 0, parent block: None, continuation block: None:

defblock 0:
    if True:
        gotoblock 1
    else:
        [delegated: get the direct continuation of block 0]

defblock 1:
    [enqueue generation of code inside the while loop, with loopback to block 0 if the block doesn't return]

defblock 2:
    //no more code

//Finalizing procedure of block 0:
//detect any unfinished blocks that have been registered as continuations.
//Block 2 was registered as a continuation of block 0. Since there's no more code, delegate to parent block None which continues onto block None.
//If continuation block is None, then we just return.


defblock 2 (register as continuation of block 0):
    return

/*

Continuations:
Block 0 -> Block 2

Queue: [
    {
        Block: 1
        Node: &HIR
        Parent Block: 0
        Loopback: 0
    }
]

 */

Codegen of block 1, parent block: 0, continuation block: 2:

defblock 1:    
    return

Full CFG:

defblock 0:
    if True:
        gotoblock 1
    else:
        gotoblock 2

defblock 1:
    return

defblock 2:
    return



Case 7: While loop that returns after a few iterations

def main():
    x = 10
    while x > 0:
        x = x + 1
    return x

Codegen of block 0, parent block: None, continuation block: None:

defblock 0:
    x = 10
    gotoblock 1    

defblock 1:
    if x > 0:
        gotoblock 2
    else:
        [delegated: get the direct continuation of block 1] //this is block 3

defblock 2:
    [enqueue generation of code inside the while loop, with loopback to block 1 if the block doesn't return]

defblock 3:
    return x

//Finalizing procedure of block 0:
//detect any unfinished blocks that have been registered as continuations.
//Block 3 was registered as a continuation of block 1. We actually already generated code for block 3 so that's done.
//No work to do here.


/*
Continuations:
Block 1 -> Block 3

Queue: [
    {
        Block: 2
        Node: &HIR
        Parent Block: 1
        Loopback: 1
    }
]
*/

Codegen of block 2, parent block: 1, continuation block: 3, loopback: 1

defblock 2:
    x = x + 1
    gotoblock 1 //here we decided to use the loopback instead of the continuation block. If there is a loopback we use it.


Full CFG: 

defblock 0:
    x = 10
    gotoblock 1

defblock 1:
    if x > 0:
        gotoblock 2
    else:
        gotoblock 3

defblock 2:
    x = x + 1
    gotoblock 1

defblock 3:
    return x


Case 8: While loop that returns after a few iterations, where we increase the counter conditionally

def main():
    x = 10
    while x > 0:
        if x > 5:
            x = x + 1
        else:
            x = x + 2
    return x

//In this case we have to pay attention to the loopbacks, the branches can't go to the return block, it has to go to the loopback.

defblock 0:
    x = 10
    gotoblock 1

defblock 1:
    if x > 0:
        gotoblock 2
    else:
        [delegated: get the direct continuation of block 1] //this is block 3

defblock 2:
    [enqueue generation of code inside the while loop, with loopback to block 1 if the block doesn't return]

defblock 3:
    return x

//blablabla finalization procedure, nothing to do here

/*
Continuations:
Block 1 -> Block 3

Queue:
[
    {
        Block: 2
        Node: &HIR
        Parent Block: 1
        Loopback: 1
    }
]
*/

Codegen of block 2, parent block: 1, continuation block: 3, loopback: 1

defblock 2:
    if x > 5:
        gotoblock 4
    else:
        gotoblock 5

defblock 4:
    [enqueue generation of code inside the if statement, with loopback to block 1 if the block doesn't return]

defblock 5:
    [enqueue generation of code inside the else statement, with loopback to block 1 if the block doesn't return]

/*
Continuations: 
Block 1 -> Block 3

Queue:
[
    {
        Block: 4
        Node: &HIR
        Parent Block: 2
        Loopback: 1
    },
    {
        Block: 5
        Node: &HIR
        Parent Block: 2
        Loopback: 1
    }
]
*/

Codegen of block 4, parent block: 2, continuation block: 3, loopback: 1
defblock 4:
    x = x + 1
    gotoblock 1

Codegen of block 5, parent block: 2, continuation block: 3, loopback: 1
defblock 5:
    x = x + 2
    gotoblock 1

Full CFG:

defblock 0:
    x = 10
    gotoblock 1

defblock 1:
    if x > 0:
        gotoblock 2
    else:
        gotoblock 3

defblock 2:
    if x > 5:
        gotoblock 4
    else:
        gotoblock 5

defblock 3:
    return x

defblock 4:
    x = x + 1
    gotoblock 1

defblock 5:
    x = x + 2
    gotoblock 1





Algorithm idea:
===============

//First, schedule an analysis of the body, which will result in statement groups, i.e.
//it breaks the statement in groups when needed. This analysis will have a "pivot block" and for the
//body root, it is 0.

def main():
    x = 1
    x = 2
    x = 3
    x = 4

//this should result in 2 groups. The first group is the declaration of x, and the second group are the assignments, which can be in a single group.
//This case is kinda silly but illustrates the problem. If statements, While statements, return statements and declarations always cause a group break.

//What we will see in the code is kind of a job system. We have 2 types of jobs:
//Analysis { body:Vec<HIRNode>, block: BlockId, parent: BlockId, continuation: BlockId, loopback: BlockId }: 
//    Responsible for a first-pass analysis of the HIR. It will generate the groups and schedule more jobs.
//Codegen { group: Vec<HIRNode>, block: BlockId, parent: BlockId, continuation: BlockId, loopback: BlockId }:
//    Responsible for emitting code for the group. However, when it finds an IF statement, it will schedule more analysis jobs, which in turn will schedule more codegen jobs.

//This is at least theoretical, the problem is that it generates a lot of communication. Since we don't parallelize the codegen, this might be not necessary, we could have the codegen 
//job perform both things. The analysis really only does the grouping, so... 


//A more practical way to do this is to have the codegen do the grouping and take in a pivot block. If we run a grouping and find only one group, we can generate the code.
//If we find a nested structure, we can run a grouping on the inner statements and schedule jobs.
