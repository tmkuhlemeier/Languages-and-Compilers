
# Open questions

## Exercise 4
In the Happy documentation, it is stated that it is more efficient at parsing left-recursive rules. This is because it would result in requiring only constant stack space, whereas right-recursive rules would need stack space proportional to the length of the list being parsed.Left-recursive grammars (and therefore also left-recursive parser combinators) on the other hand, are somthing to be avoided. This is because a left-recursive parser would loop. Right-recursive grammars and parsers don't deal with this issue.

## Exercise 10
Consider a Program that has only one Rule. This Rule is recursive and refers to itself only in the last Command. This means that the last element of the stack is the command that refers to itself. When doing the steps of the Program, all other Commands that were before the recursive call are removed from the stack and when you arrive at the last Command, all the Commands of the Rule are added to the stack again, resulting in the same starting position, with the same amount of Commands.
Now consider a different Program that also only has one Rule and is recursive. Here the first Command of the Rule is the recursive call. At first the stack is the amount of Commands that the Rule has, but after applying the recursive call, the stack is appended with the extra Commands of the Rule, now containing almost twice as many Commands as it has started with. This means that the earlier the recursive Command, the less efficient it is.
