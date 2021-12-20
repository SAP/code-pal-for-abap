# Code Pal for ABAP

[Code Pal for ABAP](../README.md) > [How to Contribute](how-to-contribute.md)

## How to Contribute

How about having 1000s of ABAP developers worldwide using a Check developed by you?  
How about helping our ABAP ecosystem by writing Clean ABAP Checks that could be accessible to everyone?  
Got you curious?  

So, don't miss the opportunity to contribute to this Open-Source Project!  

Let's keep this project up & running!

> :bulb: [Keep it Simple](https://en.wikipedia.org/wiki/KISS_principle): One check validates one single behavior.

### Preparing GitHub and abapGit

Follow the [Contributing to a project](https://docs.abapgit.org/guide-contributing.html).

### Creating a new Check

> We will use the `y_pal_boolean_input_param` as a guide. 

To start, create a new Global Class named `y_pal_<NAME>` under the Code Pal package `*_checks`. It should have a `constructor` and inherit from the `y_code_pal_base`:

```abap
CLASS y_pal_boolean_input_param DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC.
    PUBLIC SECTION.
        METHODS constructor.
    PROTECTED SECTION.
        METHODS inspect_tokens REDEFINITION.
        METHODS add_check_quickfix REDEFINITION.
ENDCLASS.
```

The `constructor` must inherit the superclass, redefine the required configuration, and set the Check message:

> You can find the available configurations in the `y_code_pal_base` constructor. 

```abap
  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC BOOL_PARAM' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }boolean-input-parameter.md|.

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-class_definition ) ).
    relevant_structure_types = VALUE #( ).

    set_check_message( 'Split method instead of Boolean input parameter!' ).
  ENDMETHOD.
```

The `inspect_tokens` will detect the issue, validate the customizing, and raise the check.  You can use the imported `structures` and `statements` to loop the `tokens`.

The `detect_check_configuration` is required to identify the most relevant Check configuration. If no configuration is relevant, the framework will skip it internally. 

The `raise_error` is required to inform the Code Inspector about the finding. 

```abap
  METHOD inspect_tokens.
    CHECK keyword( ) = if_kaizen_keywords_c=>gc_methods.
    CHECK is_setter_method( statement ) = abap_false.
    CHECK has_boolean_input_param( statement ).

    DATA(configuration) = detect_check_configuration( statement ).

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from = statement-from
                 check_configuration = configuration ).
  ENDMETHOD.
```

There are some Checks with distinct ways to scan the code. Please, take a look at how the already existing objects perform before deciding how to implement your new object.

To ensure the remote check capability, you must use the `manager->database_access` object:

```abap 
    DATA(object_description) = manager->database_access->repository_access->get_class_description( name ).    
```

```abap 
    DATA(message_class) = manager->database_access->get_message_class( name ).
```

### Creating the unit test

> We will use the `y_pal_boolean_input_param` as a guide. 

It is highly recommended to follow the Test Driven Development (TDD) approach.

Create one or more `Local Test Class` under the `Global Class` created before. It must inherit the `y_code_pal_unit_test_base` class.  

```abap
CLASS local_test_class DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.
```

The `get_cut` method returns an instance of the class under test.

```abap
  METHOD get_cut.
    result ?= NEW y_pal_boolean_input_param( ).
  ENDMETHOD.
```

The `get_code_with_issue` method returns a code sample that will raise the finding:

```abap
  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS update IMPORTING do_commit TYPE abap_bool. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD update. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.
```

The `get_code_without_issue` method returns a code sample that will not raise the finding (you can use it for false-positives):

```abap
  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS update_without_commit. ' )
      ( '     METHODS update_and_commit. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD update_without_commit. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD update_and_commit. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.
```

The `get_code_with_exemption` method returns a code sample that will omit the finding by using the Pseudo Comment (exemption):

```abap
  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS update IMPORTING do_commit TYPE abap_bool.  "#EC BOOL_PARAM ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD update. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.
```

### Activating the Check

Execute the report `Y_CODE_PAL_REGISTRATION` using the run mode `Activate`.

## Further reading

- [How to write an ATC Check](https://www.sap.com/documents/2018/09/905bfdab-1a7d-0010-87a3-c30de2ffd8ff.html)
- [How to scan ABAP source code](https://www.abapoptimizer.com/blog/how-to-scan-abap)
- [Understanding the GitHub flow](https://guides.github.com/introduction/flow/)
- [Write a good commit message](https://chris.beams.io/posts/git-commit/)
- [Contributing to a project](https://docs.abapgit.org/guide-contributing.html)
