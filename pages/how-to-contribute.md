# Code Pal for ABAP

[Code Pal for ABAP](../README.md) > [How to Contribute](how-to-contribute.md)

## How to Contribute

How about having 1000s of ABAP Developers worldwide using a CHECK developed by you? How about helping our ABAP Ecosystem by writing SAP Code Style Guides Checks that could be accessible by everyone? Got you curious?  

So, don't miss the opportunity to contribute with your ideas to this Open-Source Project! Let's keep this project up & running!

> :bulb: [Keep it Simple](https://en.wikipedia.org/wiki/KISS_principle): One check validates one single behavior.

### How to Fork the Repository

Fork our repository, then clone/pull the forked repo via abapGit into your system.  
Follow the [Fork a repo](https://docs.github.com/en/github/getting-started-with-github/fork-a-repo) guide.

### How to Create a New Check

Create a new global class under the package `*_checks`. It should have a `constructor` and redefine the `inspect_tokens` methods at least.

```abap
CLASS y_check_<its_name> DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
    PUBLIC SECTION.
        METHODS constructor.
    PROTECTED SECTION.
        METHODS inspect_tokens REDEFINITION.
ENDCLASS.
```

The `constructor` method will inherit the superclass, define the check default customization, and set the code inspector message:

```abap
METHOD constructor.
    super->constructor( ).

    settings-prio = <its_priority>.
    settings-threshold = <its_threshold>.
    settings-documentation = |{ c_docs_path-checks }<its_documentation>.md|.
    settings-pseudo_comment = '"#EC <its_pseudo_comment>' ##NO_TEXT.
    " settings...

    set_check_message( '<its_message>' ).
ENDMETHOD.
```

The `inspect_tokens` method will detect the issue, validate the customizing, and raise the check.  
Here, you can use the imported `structures` and `statements` to loop the `tokens`.

```abap
LOOP AT ref_scan_manager->get_tokens( ) ASSIGNING FIELD-SYMBOL(<token>)
FROM statement-from TO statement-to.
    " ...
ENDLOOP.
```

> :bulb: Before start implementing the `inspect_tokens` method, we recommend you to create the unit tests to follow the Test Driven Development (TDD) approach.

The `detect_check_configuration` method validates the check customizing.
If an empty structure is received, it means the check should not be raise.

```abap
DATA(check_configuration) = detect_check_configuration( error_count = <error_count>
                                                        statement = <statement_structure> ).

IF configuration IS INITIAL.
    RETURN.
ENDIF.
```

The `raise_error` method raises the check.

```abap
raise_error( statement_level = statement-level
             statement_index = index
             statement_from = statement-from
             error_priority = configuration-prio ).
```

The `execute_check` method can be redefined when the check searches for an issue in a non-default statement type. The default types are defined in the `y_check_base`->`execute_check`.

### How to Create a Unit-Test

Create a local test class under the global test class created above. It should inherit and implement the abstract methods from the `y_unit_test_base` class.  
We will use the `y_check_prefer_is_not` check as an example.

> :bulb: Create multiples local test classes to validate distinct behaviors.

```abap
CLASS ltc_not_is_initial DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
    PROTECTED SECTION.
        METHODS get_cut REDEFINITION.
        METHODS get_code_with_issue REDEFINITION.
        METHODS get_code_without_issue REDEFINITION.
        METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.
```

The `get_cut` method must return an instance of the class under test.

```abap
METHOD get_cut.
    result ?= NEW y_check_prefer_is_not( ).
ENDMETHOD.
```

The `get_code_with_issue` method must return a snippet of code which raises the check.  

```abap
METHOD get_code_with_issue.
    result = VALUE #(
        ( ' REPORT y_example. ' )

        ( ' START-OF-SELECTION. ' )
        ( '   DATA(count) = 0. ' )
        ( '   IF NOT count IS INITIAL. ' )
        ( '     count = 1. ' )
        ( '   ENDIF. ' )
    ).
ENDMETHOD.
```

The `get_code_without_issue` method must return a snippet of code which do not raise the check, or how the fixed code should be.

```abap
METHOD get_code_with_issue.
    result = VALUE #(
        ( 'REPORT y_example. ' )

        ( ' START-OF-SELECTION.      ' )
        ( '   DATA(count) = 0. ' )
        ( '   IF count IS NOT INITIAL. ' )
        ( '     count = 1. ' )
        ( '   ENDIF. ' )
    ).
ENDMETHOD.
```

The `get_code_with_exemption` method must return a snippet of code which usage of the pseudo comment.

```abap
METHOD get_code_with_exemption.
    result = VALUE #(
        ( 'REPORT y_example. ' )

        ( ' START-OF-SELECTION.      ' )
        ( '   DATA(count) = 0. ' )
        ( '   IF NOT count IS INITIAL. "#EC PREFER_IS_NOT ' )
        ( '     count = 1. ' )
        ( '   ENDIF. ' )
    ).
ENDMETHOD.
```

### How to Test the New Check

Start the transaction `SCI`, and go to the `Code Inspector > Management of > Checks` menu.  
Then, select the new check class and save it.

Start the transaction `SCI` again, and change the global check variant.  
Then, select the new check class and save it.

Extend the `y_demo_failures` class with an example for the new check.  
Then, run the code inspector or ATC using the global check variant.

### How to Submit the New Check

When it is done, please `stage` --> `commit` --> `push` the files to your fork.

In the github.com, create a pull request from your fork to our base repo.

At this point of time, we will verify your code and authorize/approve the merge (if applicable).  
Please create the pull request to merge it with our `master` branch.

Thank you in advance for contributing and sharing your ideas within our community!

**We really appreciate this! :heart_eyes:**

## Further Reading

- [How to write an ATC Check](https://www.sap.com/documents/2018/09/905bfdab-1a7d-0010-87a3-c30de2ffd8ff.html)
- [How to scan ABAP source code](https://www.abapoptimizer.com/blog/how-to-scan-abap)
- [Understanding the GitHub flow](https://guides.github.com/introduction/flow/)
- [Write a good commit message](https://chris.beams.io/posts/git-commit/)
- [Contributing to a project](https://docs.abapgit.org/guide-contributing.html)
