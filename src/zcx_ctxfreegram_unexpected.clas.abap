class ZCX_CTXFREEGRAM_UNEXPECTED definition
  public
  inheriting from CX_NO_CHECK
  final
  create public .

public section.

  types:
    BEGIN OF ENUM ty_error STRUCTURE c_error,
             after_rhs_element,
             character_after_lhs_equal,
             equal_expected_after_lhs,
             lhs_element_not_bound,
             missing_start_rule,
*             parse_unexpected,
*             parse_token_not_in_grammar,
*             parse_unexpected_token_state,
*             parse_bug_rule_not_found,
*             parse_bug_rule_without_element,
*             parse_bug_stack_processing,
*             parse_bug_goto_not_found,
*             parse_end_of_file,
             rhs_element_not_bound,
             rhs_nonterminal_wo_lhs,
             rule_first_character,
             rules_circular_reference,
             start_rule_must_appear_once,
             start_rule_not_bound,
           END OF ENUM ty_error STRUCTURE c_error .

  data TEXT type STRING read-only .
  data MSGV1 type SYMSGV read-only .
  data MSGV2 type SYMSGV read-only .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !TEXT type STRING optional
      !MSGV1 type SYMSGV optional
      !MSGV2 type SYMSGV optional .

  methods IF_MESSAGE~GET_LONGTEXT
    redefinition .
  methods IF_MESSAGE~GET_TEXT
    redefinition .
protected section.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_CTXFREEGRAM_UNEXPECTED IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
me->TEXT = TEXT .
me->MSGV1 = MSGV1 .
me->MSGV2 = MSGV2 .
  endmethod.


  METHOD GET_LONGTEXT.
    result = get_text( ).
  ENDMETHOD.


  METHOD GET_TEXT.
    result = text.
    replace '&1' in result with msgv1.
    replace '&2' in result with msgv2.
  ENDMETHOD.
ENDCLASS.
