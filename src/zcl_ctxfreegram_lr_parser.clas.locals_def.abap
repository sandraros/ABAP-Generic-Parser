*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_args DEFINITION.
  PUBLIC SECTION.
    METHODS arg IMPORTING VALUE(arg) TYPE simple RETURNING VALUE(args) TYPE REF TO lcl_args.
    DATA args TYPE TABLE OF string.
ENDCLASS.

CLASS lcl_token_end_of_input DEFINITION
    CREATE PRIVATE.
  PUBLIC SECTION.

    INTERFACES zif_ctxfreegram_token.

    ALIASES offset FOR zif_ctxfreegram_token~offset.
    ALIASES length FOR zif_ctxfreegram_token~length.

    CLASS-METHODS create
      IMPORTING
        text          TYPE string
      RETURNING
        VALUE(result) TYPE REF TO lcl_token_end_of_input.

  PRIVATE SECTION.

    DATA text TYPE string.

ENDCLASS.

interface lif_parsed_symbol.
ENDINTERFACE.

class lcl_parsed_nonterminal DEFINITION
    create PRIVATE.
  PUBLIC SECTION.
    INTERFACES lif_parsed_symbol.
    types tt_parsed_symbol type STANDARD TABLE OF ref to lif_parsed_symbol with EMPTY KEY.
    class-methods create
      importing
        rule_number   type i
        child_symbols type tt_parsed_symbol
      RETURNING
        VALUE(result)  type ref to lcl_parsed_nonterminal.
    data rule_number type i.
    data child_symbols type tt_parsed_symbol.
ENDCLASS.

class lcl_parsed_terminal DEFINITION
    create PRIVATE.
  PUBLIC SECTION.
    INTERFACES lif_parsed_symbol.
    class-methods create
      importing
        token type ref to zif_ctxfreegram_token
      RETURNING
        VALUE(result)  type ref to lcl_parsed_terminal.
    data token type ref to zif_ctxfreegram_token.
*    data terminal_index type i.
*    data line type i.
*    data offset type i.
*    data length type i.
ENDCLASS.
