CLASS zcl_ctxfreegram_parsed_nonterm DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_ctxfreegram_parsed_symbol.

    TYPES tt_parsed_symbol TYPE STANDARD TABLE OF REF TO zif_ctxfreegram_parsed_symbol WITH EMPTY KEY.

    DATA rule_number TYPE i.
    DATA child_symbols TYPE tt_parsed_symbol.

    CLASS-METHODS create
      IMPORTING
        rule_number   TYPE i
        child_symbols TYPE tt_parsed_symbol
      RETURNING
        VALUE(result) TYPE REF TO zcl_ctxfreegram_parsed_nonterm.

ENDCLASS.

CLASS zcl_ctxfreegram_parsed_nonterm IMPLEMENTATION.

  METHOD create.
    result = NEW zcl_ctxfreegram_parsed_nonterm( ).
    result->rule_number = rule_number.
    result->child_symbols = child_symbols.
  ENDMETHOD.

ENDCLASS.
