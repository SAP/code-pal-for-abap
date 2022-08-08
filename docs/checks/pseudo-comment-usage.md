[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Pseudo Comment Usage Check](pseudo-comment-usage.md)

## Pseudo Comment Usage Check

### What is the intent of the check?

This check lists the number of "code pal for ABAP" pseudo comments per object being used.  
Pseudo comments completely suppress the findings in ATC. Thus, this check can be used whether objects without any other finding use a lot of pseudo comments to suppress findings.

### How does the check work?

It simply counts the number of code pal for ABAP pseudo comments being used.

### How to solve the issue?

Solve the other issues so that fewer pseudo comments are used.

### What to do in case of exception?

There is no exception as this check is merely counting pseudo comments and so its findings do not represent any specific issue.
