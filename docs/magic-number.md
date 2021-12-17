[code pal for ABAP](../README.md) > [Magic Number Check](magic-number.md)

## Magic Number Usage Check

### What is the Intent of the Check?

This check searches for arbitrary values in the source code having no meaningful connotation.

Using magic numbers has disadvantages:

* The code is not readable as you need to understand the meaning of the number again and again.
* If these numbers need to be changed, modifications are required in every place in the code where this number is present. This makes the maintenance of the code inefficient and error prone.

### How does the check work?

It searches for numbers/values in the following statements:

1. `IF`
2. `ELSEIF`
3. `WHEN`
4. `CHECK`
5. `DO`

REMARK: Magic Numbers associated with `SY-SUBRC` are not considered by this check. In addition, the numbers `0` and `1` are ignored.

### How to solve the issue?

Create constants. By the name of the constant the number becomes a meaning which increases the readability. In addition, when maintaining the code, you only need to change the constant. This change can be done without the risk of introducing new errors or without forgetting some places where this change is required.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC CI_MAGIC` which should be placed right after the statement containing the magic number:

```abap
DO 5 TIMES. "#EC CI_MAGIC
  " Loop content
ENDDO.
```

### Further Readings & Knowledge

* [Clean ABAP](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#use-constants-instead-of-magic-numbers)
