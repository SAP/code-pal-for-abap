INTERFACE lif_ref_scan_manager.
  METHODS:
    set_ref_scan
      IMPORTING io_ref_scan TYPE REF TO cl_ci_scan,
    get_tokens
      RETURNING VALUE(result) TYPE stokesx_tab,
    get_statements
      RETURNING VALUE(result) TYPE sstmnt_tab.
ENDINTERFACE.

INTERFACE lif_pseudo_comment_detector.
  CONSTANTS:
    ec_prefix  TYPE string VALUE `#EC`,
    ec_comment TYPE string VALUE `"#EC`.

  METHODS:
    is_pseudo_comment
      IMPORTING
                !ref_scan_manager TYPE REF TO y_if_scan_manager
                !scimessages      TYPE scimessages
                !test             TYPE sci_chk
                !code             TYPE sci_errc
                !suppress         TYPE sci_pcom OPTIONAL
                !position         TYPE int4
      RETURNING VALUE(result)     TYPE sychar01.
ENDINTERFACE.

CLASS lcl_ref_scan_manager DEFINITION.
  PUBLIC SECTION.
    INTERFACES: y_if_scan_manager.
    CONSTANTS: ok TYPE syst_subrc VALUE 0.

  PRIVATE SECTION.
    DATA ref_scan TYPE REF TO cl_ci_scan.
ENDCLASS.

CLASS lcl_pseudo_comment_detector DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_pseudo_comment_detector.

  PRIVATE SECTION.
    DATA: pcom     TYPE sci_pcom,
          pcom_alt TYPE sci_pcom.

    METHODS:
      determine_pseudo_comments
        IMPORTING
          !scimessages TYPE scimessages
          !test        TYPE sci_chk
          !code        TYPE sci_errc
          !suppress    TYPE sci_pcom OPTIONAL,
      has_comment
        IMPORTING
          !ref_scan_manager TYPE REF TO y_if_scan_manager
          !position         TYPE int4
        RETURNING
          VALUE(result)     TYPE sci_pcom,
      has_inline_comment
        IMPORTING
          !ref_scan_manager TYPE REF TO y_if_scan_manager
          !position         TYPE int4
        RETURNING
          VALUE(result)     TYPE sci_pcom.
ENDCLASS.

CLASS lcl_statistics DEFINITION.
  PUBLIC SECTION.
    INTERFACES: y_if_scan_statistics.

  PRIVATE SECTION.
    DATA: number_errors          TYPE i,
          number_warnings        TYPE i,
          number_pseudo_comments TYPE i.
ENDCLASS.

CLASS lcl_test_code_detector DEFINITION.
  PUBLIC SECTION.
    INTERFACES:
      y_if_testcode_detector.

  PRIVATE SECTION.
    DATA:
      ref_scan_manager TYPE REF TO y_if_scan_manager,
      statement_wa     TYPE sstmnt,
      test_codes       TYPE y_if_testcode_detector=>t_test_codes,
      test_code        TYPE y_if_testcode_detector=>t_test_code.
    METHODS:
      process_statements
        IMPORTING structure TYPE sstruc,
      process_tokens
        IMPORTING statement TYPE sstmnt,
      testclass_added
        IMPORTING token         TYPE stokesx
        RETURNING VALUE(result) TYPE abap_bool,
      testmethod_added
        IMPORTING token         TYPE stokesx
        RETURNING VALUE(result) TYPE abap_bool,
      keyword
        RETURNING VALUE(result) TYPE string,
      get_token_rel
        IMPORTING p_n           TYPE i
        RETURNING VALUE(result) TYPE string,
      determine_test_code,
      is_test_class
        RETURNING VALUE(result) TYPE abap_bool.
ENDCLASS.
