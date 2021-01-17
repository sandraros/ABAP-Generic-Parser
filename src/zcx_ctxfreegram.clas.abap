CLASS zcx_ctxfreegram DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ENUM ty_error STRUCTURE c_error,
             rule_first_character,
             equal_expected_after_lhs,
             character_after_lhs_equal,
             after_rhs_element,
             parse_unexpected_token,
             parse_bug_rule_not_found,
             parse_bug_rule_without_element,
             parse_bug_stack_processing,
             parse_bug_goto_not_found,
             parse_end_of_file,
             missing_start_rule,
             start_rule_must_appear_once,
             start_rule_not_bound,
             lhs_element_not_bound,
             rhs_element_not_bound,
             rules_circular_reference,
           END OF ENUM ty_error STRUCTURE c_error.

    METHODS constructor
      IMPORTING
        !error    TYPE ty_error
        !previous LIKE previous OPTIONAL .

    METHODS get_text REDEFINITION.
    METHODS get_longtext REDEFINITION.

    DATA error TYPE ty_error READ-ONLY.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_ctxfreegram IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->error = error.
  ENDMETHOD.


  METHOD get_text.
    CASE error.
      WHEN c_error-rule_first_character.
        result = 'start of production rule ne peut pas être un de ces 4 caractères: " = , ;'(001).
      WHEN c_error-equal_expected_after_lhs.
        result = 'after le LHS, doit être ='(002).
      WHEN c_error-character_after_lhs_equal.
        result = 'after LHS =, ne peut pas être = ou ,'(003).
      WHEN c_error-after_rhs_element.
        result = 'after un element du RHS, doit être un de ces 2 caractères: , ;'(004).
      WHEN c_error-parse_unexpected_token.
        result = 'invalid Token'(005).
      WHEN c_error-parse_bug_rule_not_found.
        result = 'Bug rule not found'(006).
      WHEN c_error-parse_bug_rule_without_element.
        result = 'Bug rule without element'(007).
      WHEN c_error-parse_bug_stack_processing.
        result = 'Bug in stack management'(008).
      WHEN c_error-parse_bug_goto_not_found.
        result = 'Bug in GOTO management'(009).
      WHEN c_error-parse_end_of_file.
        result = 'End of file'(010).
      WHEN c_error-missing_start_rule.
        result = 'Start rule is not one of the rules'(011).
      WHEN c_error-start_rule_must_appear_once.
        result = 'Start rule must appear once'(012).
      WHEN c_error-start_rule_not_bound.
        result = 'start_rule_not_bound'(013).
      WHEN c_error-lhs_element_not_bound.
        result = 'lhs_element_not_bound'(014).
      WHEN c_error-rhs_element_not_bound.
        result = 'rhs_element_not_bound'(015).
      WHEN c_error-rules_circular_reference.
        result = 'rules_circular_reference'(016).
    ENDCASE.
  ENDMETHOD.

  METHOD get_longtext.
    result = get_text( ).
  ENDMETHOD.

ENDCLASS.
