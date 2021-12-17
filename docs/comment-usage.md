[code pal for ABAP](../README.md) > [Comment Usage Check](comment-usage.md)

## Comment Usage Check

### What is the Intent of the Check?

This check searches for comments in the code. Clean Code principles do not forbid you to comment your code - but it encourages you to exploit better means, meaningful names and resort to comments only if that fails. Express yourself in code, not in comments.

### How does the check work?

This check calculates an indicator; precisely, the percentage of comments in relation to the absolute number of real statements (productive code).

### How to solve the issue?

Remove unimportant and/or unnecessary comments.

### What to do in case of exception?

There is no exception for this check since it works as an indicator only. Thus, it is also not possible to suppress its findings.

### Further Readings & Knowledge

* [Clean ABAP: Less is more](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#express-yourself-in-code-not-in-comments)
