[code pal for ABAP](../README.md) > [Prefer IS NOT to NOT IS](prefer-is-not-to-not-is.md)

## Prefer IS NOT to NOT IS

### What is the Intent of the Check?

Prefer `IS ... NOT` to `NOT ... IS` because it requires a "mental turnaround" that makes it harder to understand the negation logic.

### How to solve the issue?

Preferably, use a positive condition; but if the negative condition is easier to understand, change the `NOT ... IS` to `IS ... NOT`.

### What to do in case of exception?

In exceptional cases, you can suppress this finding by using the pseudo comment `"#EC PREFER_IS_NOT`:

```abap
  IF NOT variable IS INITIAL. "#EC PREFER_IS_NOT
  ENDIF.
```

```abap
  IF NOT variable CP 'TODO*'. "#EC PREFER_IS_NOT
  ENDIF.
```

```abap
  IF NOT variable = 42. "#EC PREFER_IS_NOT
  ENDIF.
```

### Example

Before the check:

```abap
  IF NOT variable IS INITIAL.
  ENDIF.
```

```abap
  IF NOT variable CP 'TODO*'.
  ENDIF.
```

```abap
  IF NOT variable = 42.
  ENDIF.
```

After the check:

```abap
  IF variable IS NOT INITIAL.
  ENDIF.
```

```abap
  IF variable NP 'TODO*'.
  ENDIF.
```

```abap
  IF variable <> 42.
  ENDIF.
```

### Further Readings & Knowledge

* [Clean ABAP](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-is-not-to-not-is)
