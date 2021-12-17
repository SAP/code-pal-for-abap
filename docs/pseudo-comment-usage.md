[code pal for ABAP](../README.md) > [Pseudo Comment Usage Check](pseudo-comment-usage.md)

## Pseudo Comment Usage Check

### What is the Intent of the Check?

This check lists the number of "code pal for ABAP" pseudo comments per object being used.  
Pseudo comments completely suppress the findings in ATC. Thus, this check can be used whether objects without any other finding use a lot of pseudo comments to suppress findings.

### How does the check work?

It simply counts the number of used code pal for ABAP pseudo comments.

### How to solve the issue?

Solve the other issues so that less pragmas are used.

### What to do in case of exception?

There is no exception as this check works as an indicator. Thus, it is not possible to suppress Code Inspector findings from this check.
