CLASS zcl_ctxfreegram_parsed_term DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_ctxfreegram_parsed_symbol.

    DATA token TYPE REF TO zif_ctxfreegram_token.

    CLASS-METHODS create
      IMPORTING
        token         TYPE REF TO zif_ctxfreegram_token
      RETURNING
        VALUE(result) TYPE REF TO zcl_ctxfreegram_parsed_term.

ENDCLASS.

CLASS zcl_ctxfreegram_parsed_term IMPLEMENTATION.

  METHOD create.
    result = NEW zcl_ctxfreegram_parsed_term( ).
    result->token = token.
  ENDMETHOD.

ENDCLASS.
