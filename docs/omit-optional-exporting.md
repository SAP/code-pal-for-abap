[code pal for ABAP](../README.md) > [Omit Optional EXPORTING](omit-optional-exporting.md)

## Omit Optional EXPORTING

### What is the Intent of the Check?

This check searches for the optional EXPORTING wording which makes the class method call needlessly longer:

Anti-Pattern:
```abap
modify->update(
  EXPORTING
    node   = /dirty/my_bo_c=>node-item
    key    = item->key
    data   = item
    fields = changed_fields ).
```

Pattern:
```abap
modify->update( node   = /clean/my_bo_c=>node-item
                key    = item->key
                data   = item
                fields = changed_fields ).
```


### How does the check work?

This check searches for the usage of the optional keyword EXPORTING in the class method calls.

### How to solve the issue?

Omit the optional keyword EXPORTING (it works implicitly).

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC OPTL_EXP` which should be placed right at the end of the statement:

```abap
  class->meth1( EXPORTING param1 = 'example' ). "#EC OPTL_EXP
```

### Further Readings & Knowledge

* [Clean ABAP](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#omit-the-optional-keyword-exporting)
