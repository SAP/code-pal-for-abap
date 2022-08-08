[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Comment Usage Check](comment-usage.md)

## Comment Usage Check

### What is the intent of the check?

This check counts the number of comments in your code. While Clean Code principles do not prohibit commenting as such, they encourage us to use better means to express ourselves in our code whenever possible, and so an excessive number of comments can be an indication that this guideline is not followed or misunderstood in a particular piece of code.

### How does the check work?

The check counts the number of comments in a compilation unit and then emits a finding that tells you the ratio of comments to productive statements in that code. 

### How to solve the issue?

Perhaps there is no issue and every comment in the code is actually meaningful and necessary. Do not treat findings from this check as issues that need to be "solved" by deleting potentially valuable comments, but consider them an invitation to think about your comment practices and find ways to incorporate the information they convey directly into the code itself.

Of course, if you do decide that some comments are unnecessary, delete them.

### What to do in case of exception?

There are no pseudo comments for this check since it reports a metric that refers to a complete compilation unit. There is no single line in the code it would refer to, so there is no meaningful location where a pseudo comment suppressing it could be placed.
### Further Readings & Knowledge

* [Clean ABAP - Express yourself in code, not comments](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#express-yourself-in-code-not-in-comments)
