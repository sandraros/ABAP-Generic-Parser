*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_nonterminal DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_ctxfreegram_rule_nonterm.
    METHODS constructor
      IMPORTING
        nonterminal TYPE string.
ENDCLASS.

CLASS lcl_terminal DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_ctxfreegram_rule_term.
    METHODS constructor
      IMPORTING
        terminal TYPE string.
ENDCLASS.

CLASS lcl_terminal_regex DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_ctxfreegram_rule_termregex.
    METHODS constructor
      IMPORTING
        terminal_regex TYPE string.
ENDCLASS.

CLASS lcl_sequence DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_ctxfreegram_rule_seq.
    METHODS constructor
      IMPORTING
        elements TYPE zcl_ctxfreegram_factory=>ty_elements.
ENDCLASS.

CLASS lcl_nonterminal IMPLEMENTATION.

  METHOD constructor.
    zif_ctxfreegram_rule_nonterm~name = nonterminal.
    zif_ctxfreegram_rule_elem~type = zif_ctxfreegram_rule_elem=>c_type-nonterminal.
  ENDMETHOD.

  METHOD zif_ctxfreegram_rule_elem~get_text.
    result = zif_ctxfreegram_rule_nonterm~name.
  ENDMETHOD.

  METHOD zif_ctxfreegram_rule_elem~get_elements.
    elements = VALUE #( ( me ) ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_terminal IMPLEMENTATION.

  METHOD constructor.
    zif_ctxfreegram_rule_term~value = terminal.
    zif_ctxfreegram_rule_elem~type = zif_ctxfreegram_rule_elem=>c_type-terminal.
  ENDMETHOD.

  METHOD zif_ctxfreegram_rule_elem~get_text.
    result = |'{ replace( val = zif_ctxfreegram_rule_term~value sub = `'` with = `''` ) }'|.
  ENDMETHOD.

  METHOD zif_ctxfreegram_rule_elem~get_elements.
    elements = VALUE #( ( me ) ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_terminal_regex IMPLEMENTATION.

  METHOD constructor.
    zif_ctxfreegram_rule_termregex~regex = terminal_regex.
    zif_ctxfreegram_rule_elem~type = zif_ctxfreegram_rule_elem=>c_type-terminal_regex.
  ENDMETHOD.

  METHOD zif_ctxfreegram_rule_elem~get_text.
    result = |'{ replace( val = zif_ctxfreegram_rule_termregex~regex sub = `'` with = `''` ) }'|.
  ENDMETHOD.

  METHOD zif_ctxfreegram_rule_elem~get_elements.
    elements = VALUE #( ( me ) ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_sequence IMPLEMENTATION.

  METHOD constructor.
    zif_ctxfreegram_rule_seq~elements = elements.
    zif_ctxfreegram_rule_elem~type = zif_ctxfreegram_rule_elem=>c_type-sequence.
  ENDMETHOD.

  METHOD zif_ctxfreegram_rule_elem~get_text.
    result = concat_lines_of( sep = ` ` table = VALUE string_table( FOR element IN zif_ctxfreegram_rule_seq~elements ( element->get_text( ) ) ) ).
  ENDMETHOD.

  METHOD zif_ctxfreegram_rule_elem~get_elements.
    elements = zif_ctxfreegram_rule_seq~elements.
  ENDMETHOD.

ENDCLASS.
