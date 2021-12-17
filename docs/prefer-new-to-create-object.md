[code pal for ABAP](../README.md) > [Prefer New to Create Object](prefer_new_to_create_object.md)

## Prefer New to Create Object

### What is the Intent of the Check?

Prefer `NEW` over `CREATE OBJECT` as it avoids needlessly longer statements.

### How to solve the issue?

Preferably, use `NEW` for creating new objects/instances.

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

* [Clean ABAP](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-new-to-create-object)
