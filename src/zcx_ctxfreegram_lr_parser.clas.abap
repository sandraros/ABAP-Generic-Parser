CLASS zcx_ctxfreegram_lr_parser DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ENUM ty_error STRUCTURE c_error,
             parse_bug_rule_not_found,
             parse_bug_rule_without_element,
             parse_bug_stack_processing,
             parse_bug_goto_not_found,
             parse_bug_unexpected,
             parse_end_of_file,
             parse_token_not_in_grammar,
             parse_unexpected,
             parse_unexpected_token,
           END OF ENUM ty_error STRUCTURE c_error.

    METHODS constructor
      IMPORTING
        !error                 TYPE ty_error
        !previous              TYPE REF TO cx_root OPTIONAL
        msgv1                  TYPE symsgv OPTIONAL
        msgv2                  TYPE symsgv OPTIONAL
        lr_PARSER              TYPE REF TO zcl_ctxfreegram_lr_parser
        VALUE(collect_details) TYPE abap_bool DEFAULT abap_false.

    METHODS get_text REDEFINITION.
    METHODS get_longtext REDEFINITION.

    DATA error TYPE ty_error READ-ONLY.
    DATA msgv1 TYPE symsgv READ-ONLY.
    DATA msgv2 TYPE symsgv READ-ONLY.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA lr_parser        TYPE REF TO zcl_ctxfreegram_lr_parser.
    DATA diagnosis_helper TYPE string.

ENDCLASS.



CLASS zcx_ctxfreegram_lr_parser IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->error = error.
    me->msgv1 = msgv1.
    me->msgv2 = msgv2.
    IF collect_details = abap_true.
      diagnosis_helper = lr_parser->grammar->render_rules( )
                      && |\n|
                      && lr_parser->grammar->render_action_goto_table( ).
    ENDIF.
  ENDMETHOD.


  METHOD get_text.
    CASE error.
      WHEN c_error-parse_unexpected.
        result = 'Unexpected error'(017).
      WHEN c_error-parse_token_not_in_grammar.
        result = 'Token not in grammar &1'(005).
      WHEN c_error-parse_unexpected_token.
        result = 'Unexpected token &1 at state &2'(018).
      WHEN c_error-parse_bug_rule_not_found.
        result = 'Bug rule not found'(006).
      WHEN c_error-parse_bug_rule_without_element.
        result = 'Bug rule without element'(007).
      WHEN c_error-parse_bug_stack_processing.
        result = 'Bug in stack management'(008).
      WHEN c_error-parse_bug_goto_not_found.
        result = 'Bug in GOTO management'(009).
      WHEN c_error-parse_bug_unexpected.
        result = 'Bug unexpected'(020).
      WHEN c_error-parse_end_of_file.
        result = 'End of file'(010).
    ENDCASE.
    replace '&1' in result with msgv1.
    replace '&2' in result with msgv2.
  ENDMETHOD.

  METHOD get_longtext.
    result = get_text( ).
  ENDMETHOD.

ENDCLASS.
