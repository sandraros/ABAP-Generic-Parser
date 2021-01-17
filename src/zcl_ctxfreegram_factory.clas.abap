CLASS zcl_ctxfreegram_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES ty_elements TYPE STANDARD TABLE OF REF TO zif_ctxfreegram_rule_elem WITH EMPTY KEY.

    CLASS-METHODS create
      RETURNING
        VALUE(r_result) TYPE REF TO zcl_ctxfreegram_factory.

    METHODS new_sequence
      IMPORTING
        elements        TYPE ty_elements
      RETURNING
        VALUE(r_result) TYPE REF TO zif_ctxfreegram_rule_seq.

    METHODS new_nonterminal
      IMPORTING
        nonterminal     TYPE string
      RETURNING
        VALUE(r_result) TYPE REF TO zif_ctxfreegram_rule_nonterm.

    METHODS new_terminal
      IMPORTING
        terminal        TYPE string
      RETURNING
        VALUE(r_result) TYPE REF TO zif_ctxfreegram_rule_term.

    METHODS new_terminal_regex
      IMPORTING
        terminal_regex  TYPE string
      RETURNING
        VALUE(r_result) TYPE REF TO zif_ctxfreegram_rule_termregex.

    CLASS-DATA singleton TYPE REF TO zcl_ctxfreegram_factory.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ctxfreegram_factory IMPLEMENTATION.


  METHOD new_nonterminal.
    r_result = NEW lcl_nonterminal( nonterminal ).
  ENDMETHOD.


  METHOD new_sequence.
    r_result = NEW lcl_sequence( elements ).
  ENDMETHOD.


  METHOD new_terminal.
    r_result = NEW lcl_terminal( terminal ).
  ENDMETHOD.


  METHOD create.
    IF singleton IS NOT BOUND.
      singleton = NEW zcl_ctxfreegram_factory( ).
    ENDIF.
    r_result = singleton.
  ENDMETHOD.


  METHOD new_terminal_regex.
    r_result = NEW lcl_terminal_regex( terminal_regex ).
  ENDMETHOD.

ENDCLASS.
