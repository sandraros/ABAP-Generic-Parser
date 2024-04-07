*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_token DEFINITION
    CREATE PRIVATE.
  PUBLIC SECTION.

    INTERFACES zif_ctxfreegram_token.

    ALIASES offset FOR zif_ctxfreegram_token~offset.
    ALIASES length FOR zif_ctxfreegram_token~length.

    TYPES ty_character TYPE c LENGTH 1.

    CLASS-METHODS create
      IMPORTING
        offset        TYPE i
        length        TYPE i
        character     TYPE ty_character
      RETURNING
        VALUE(result) TYPE REF TO lcl_token.

  PRIVATE SECTION.

    DATA character TYPE ty_character.

ENDCLASS.

CLASS lcl_token IMPLEMENTATION.

  METHOD create.
    result = NEW lcl_token( ).
    result->offset    = offset.
    result->length    = length.
    result->character = character.
  ENDMETHOD.

  METHOD zif_ctxfreegram_token~get_text.
    IF character = ' '.
      result = ` `.
    ELSE.
      result = character.
    ENDIF.
  ENDMETHOD.

  METHOD zif_ctxfreegram_token~get_detailed_info.
    result = |"{ zif_ctxfreegram_token~get_text( ) }" (offset { offset }, length { length })|.
  ENDMETHOD.

ENDCLASS.
