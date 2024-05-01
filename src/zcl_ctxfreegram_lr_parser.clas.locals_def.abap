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
