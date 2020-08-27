[BACK](../check_documentation.md)

# Boolean Input Parameter
## What is the Intent of the Check?
Boolean input parameters are often an indicator that a method does two things instead of one.  
Setter methods using boolean variables are okey.

## How to solve the issue?
Splitting the method may simplify the methods' code and describe the different intentions better.

## What to do in case of exception?
You can suppress Code Inspector findings generated by this check using the pseudo comment `"#EC BOOL_PARAM`. 
The pseudo comment has to be placed after the method declaration.

```abap
  METHODS update IMPORTING do_save TYPE abap_bool. "#EC BOOL_PARAM
```

## Example
Before the check: 
```abap
  METHODS update IMPORTING do_save TYPE abap_bool.
```

After the check:
```abap
  METHODS update_without_saving.
  METHODS update_and_save. 
```

## Further Readings & Knowledge
* [ABAP Styleguides on Clean Code](https://github.com/SAP/styleguides/blob/master/clean-abap/CleanABAP.md#split-method-instead-of-boolean-input-parameter)