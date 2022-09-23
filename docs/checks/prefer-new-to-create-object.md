[code pal for ABAP](../../README.md) > [Documentation](../check_documentation.md) > [Prefer New to Create Object](prefer_new_to_create_object.md)

## Prefer New to Create Object

### What is the intent of the check?

This check searches for `CREATE OBJECT` statements and reports a finding if the type of the instance being created is known statically. Static instance creation with the functional `NEW` constructor allows for inline declarations and more concise code.

### How to solve the issue?

Use `NEW` to create instances of objects when the type is known statically.
### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC PREF_NEW`:

```abap
   DATA prefer_new_to_crt_obj TYPE REF TO y_check_prefer_new_to_crt_obj. 
   CREATE OBJECT prefer_new_to_crt_obj. "#EC PREF_NEW
```

### Example

Before the check:

```abap
   DATA prefer_new_to_create_object TYPE REF TO y_check_prefer_new_to_crt_obj. 
   CREATE OBJECT prefer_new_to_create_object.
```

After the check:

```abap
  DATA(prefer_new_to_create_object) = NEW y_check_prefer_new_to_crt_obj( ).
```

```abap
   DATA prefer_new_to_create_object TYPE REF TO y_check_prefer_new_to_crt_obj. 
   prefer_new_to_create_object = NEW #( ).
```

### Further Readings & Knowledge

* [Clean ABAP - Prefer NEW to CREATE OBJECT](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-new-to-create-object)
